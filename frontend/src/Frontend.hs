{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Lens (over, set)
import Control.Lens.TH (makeLenses)
import Control.Monad (guard, (<=<), when, void)
import Control.Monad.IO.Class
import Control.Monad.Fix (MonadFix)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:))
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..))
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Text (Text)
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Network
import Text.RawString.QQ
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Char as Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT

import GHC.Generics (Generic)
import Common.Route

import Reflex.Dom (keypress)
import Reflex.Dom.SemanticUI

import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLInputElement as InputElement
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Storage as Storage
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers

-- TODO
-- Semi-auto date
-- Title on printed pages
-- Save / load files
-- Un-obelisk

data Summary = Strengths | Improvements deriving (Eq, Ord, Show, Generic, Enum, Bounded)
instance ToJSON Summary where
  toJSON = \case
    Strengths -> "strengths"
    Improvements -> "improvements"
instance FromJSON Summary where
  parseJSON = Aeson.withText "Summary" $ \t -> case T.toLower t of
    "strengths" -> pure Strengths
    "improvements" -> pure Improvements
    _ -> fail "Unexpected value for Summary"

summaryColour :: Summary -> Color
summaryColour = \case
  Strengths -> Green
  Improvements -> Red

newtype Name = Name { getName :: Text } deriving (Eq, Show, Generic)
deriveJSON Aeson.defaultOptions { Aeson.unwrapUnaryRecords = True } ''Name
instance Aeson.ToJSONKey Name
instance Aeson.FromJSONKey Name
instance Ord Name where
  compare = comparing (reverse . T.words . getName)

displayName :: DomBuilder t m => Name -> m ()
displayName = text . getName

type FeedbackKey = Text
defaultFeedbackKey :: FeedbackKey
defaultFeedbackKey = "Tag"

data Feedback = Feedback
  { _feedback_message :: Text
  , _feedback_summary :: Summary
  } deriving (Eq, Ord, Show, Generic)
instance ToJSON Feedback where
  toJSON f = Aeson.object ["message" .= _feedback_message f, "summary" .= _feedback_summary f]
instance FromJSON Feedback where
  parseJSON = Aeson.withObject "Feedback" $ \o -> do
    msg <- o .: "message"
    summary <- o .: "summary"
    pure $ Feedback
      { _feedback_message = msg
      , _feedback_summary = summary
      }
makeLenses 'Feedback

defaultFeedback :: Feedback
defaultFeedback = Feedback
  { _feedback_summary = Strengths
  , _feedback_message = "Edit here"
  }

type Students = Map Name (Set FeedbackKey)

-- | Entire app state data structure, for serialising
data Data = Data
  { _data_feedback :: Map FeedbackKey Feedback
  , _data_students :: Students
  } deriving (Eq, Ord, Show, Generic)
instance ToJSON Data
instance FromJSON Data
makeLenses 'Data

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text appTitle
    elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css") blank
    el "style" $ text css
  , _frontend_body = prerender_ blank app
  }

css :: Text
css = [r|
  .students { display: block; }
  .students .student { page-break-after: always; }
|]

appTitle :: Text
appTitle = "Mega marking tool 5000"

-- | Simple transition
transition
  :: (Reflex t, HasAction t conf)
  => TransitionType -> Bool -> Event t Bool -> (conf -> conf)
transition t i evt = (& action ?~ def
  { _action_transition = ffor evt $ \b -> Transition t (Just $ toDir b) def
  , _action_initialDirection = toDir i
  , _action_transitionStateClasses = forceVisible
  })
    where toDir x = if x then In else Out

-- | Key at which to store data in session storage
dataStorageKey :: Text
dataStorageKey = "data"

-- | Load saved data from session storage
loadData :: MonadJSM m => m Data
loadData = DOM.liftJSM $ do
  storage <- Window.getSessionStorage =<< DOM.currentWindowUnchecked
  mx <- Storage.getItem storage dataStorageKey
  pure $ fromMaybe (Data mempty mempty) $ Aeson.decodeStrict . T.encodeUtf8 =<< mx

-- | Save data to session storage
saveData :: (MonadJSM (Performable m), PerformEvent t m) => Event t Data -> m ()
saveData dd = performEvent_ $ ffor dd $ \d -> DOM.liftJSM $ do
  window <- DOM.currentWindowUnchecked
  storage <- Window.getSessionStorage window
  Storage.setItem storage dataStorageKey $ LT.toStrict $ Aeson.encodeToLazyText d

listEndoWithKey
  :: (Ord k, Adjustable t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t (Endo a))) -> m (Event t (Endo a))
listEndoWithKey l = (fmap . fmap) fold . listViewWithKey l

data Alter p a
  = Alter_Save a
  | Alter_Edit p
  | Alter_Cancel

fanAlter :: Reflex t => Event t (Alter p a) -> (Event t (), Event t p, Event t a)
fanAlter e =
  ( fmapMaybe (\case Alter_Cancel -> Just (); _ -> Nothing) e
  , fmapMaybe (\case Alter_Edit p -> Just p; _ -> Nothing) e
  , fmapMaybe (\case Alter_Save a -> Just a; _ -> Nothing) e
  )

-- | Generic in place altering
alterWidget
  :: (Adjustable t m, MonadFix m, MonadHold t m)
  => (p -> m (Event t (Alter p a)))
  -> m (Event t (Alter p a))
  -> m (Event t a)
alterWidget f g = mdo
  (cancel, edit, save) <- fmap (fanAlter . switch . current) $ networkHold g $ leftmost
    [g <$ save, g <$ cancel, f <$> edit]
  pure save

editableContent :: MonadWidget t m => m () -> m (Event t (Maybe Text))
editableContent content = mdo
  let s = "min-width: 1rem; display: inline-block; outline: none"
  e <- fst <$> elAttr' "span" ("contenteditable" =: "true" <> "style" =: s) content
  let htmlElement = DOM.HTMLElement . DOM.unElement $ _element_raw e
  DOM.liftJSM $ htmlElement `EventM.on` GlobalEventHandlers.keyDown $ do
    key <- EventM.uiKeyCode
    when (key == 13) $ HTMLElement.blur htmlElement
  performEvent $ ffor (domEvent Blur e) $ \_ -> do
    tc <- Node.getTextContent htmlElement
    Node.setTextContent htmlElement tc
    pure tc

fancyFeedback
  :: MonadWidget t m
  => Dynamic t Summary
  -> Maybe FeedbackKey
  -> m (Event t (Endo Data))
fancyFeedback summary mFeedback = mdo
  let conf = def
        & labelConfig_horizontal |~ True
        & labelConfig_color .~ Dyn (Just . summaryColour <$> summary)
  newText <- label conf $ do
    newText <- editableContent $ maybe blank text mFeedback
    del <- domEvent Click <$> icon' "delete" def
    pure $ leftmost [newText, Nothing <$ del]
  let f Nothing Nothing = id
      f Nothing (Just k) = over data_feedback (M.insert k defaultFeedback)
      f (Just k) Nothing
        = over data_feedback (M.delete k)
        . over data_students (M.map (S.delete k))
      f (Just k') (Just k)
        = over data_feedback (alterKey k' k)
        . over data_students (M.map (alterMember k' k))
  mapAccum_ (\old new -> (new, Endo $ f old new)) mFeedback newText

alterMember :: Ord k => k -> k -> Set k -> Set k
alterMember old new s
  | S.member old s = S.insert new $ S.delete old s
  | otherwise = s

alterKey :: Ord k => k -> k -> Map k a -> Map k a
alterKey old new m = case M.lookup old m of
  Just v -> M.insert new v $ M.delete old m
  Nothing -> m

--mapAccum_
--  :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m)
--  => (a -> b -> (a, c)) -> a -> Event t b -> m (Event t c)


feedbackWidget
  :: MonadWidget t m
  => Dynamic t Data -> m (Event t (Endo Data))
feedbackWidget dynData = list (def & listConfig_selection |~ True) $ do
  alter <- listEndoWithKey (_data_feedback <$> dynData) $ \fb feedback -> do
    listItem def $ do
      newKey <- fancyFeedback (_feedback_summary <$> feedback) (Just fb)
      newSummary <- summaryDropdown $ _feedback_summary <$> feedback
      newMsg <- fmap (fmapMaybe id) $ editableContent $ dynText $ _feedback_message <$> feedback
      pure $ fold
        [ newKey
        , ffor newMsg $ \m -> Endo $ over data_feedback (M.adjust (feedback_message .~ m) fb)
        , ffor newSummary $ \s -> Endo $ over data_feedback (M.adjust (feedback_summary .~ s) fb)
        ]
  new <- fmap (domEvent Click . fst) $ listItem' def $ do
      icon "plus" $ def & style .~ "float: left; margin: 0 1rem"
      el' "div" (text "Add more feedback")
  let new' = Endo $ over data_feedback $ M.insert defaultFeedbackKey defaultFeedback
  pure $ fold [new' <$ new, alter]

fancyStudent
  :: MonadWidget t m
  => Maybe Name
  -> m (Event t (Endo Data))
fancyStudent mName = mdo
  let conf = def & labelConfig_basic |~ True
  newText <- label conf $ do
    newText <- editableContent $ maybe blank displayName mName
    del <- domEvent Click <$> icon' "delete" def
    pure $ leftmost [fmap Name <$> newText, Nothing <$ del]
  let f Nothing Nothing = id
      f Nothing (Just n) = over data_students (M.insert n mempty)
      f (Just n) Nothing = over data_students (M.delete n)
      f (Just n') (Just n)
        = over data_students (M.mapKeysWith (<>) (\k -> if k == n' then n else k))
  mapAccum_ (\old new -> (new, Endo $ f old new)) mName newText

studentsWidget
  :: MonadWidget t m
  => Dynamic t Data -> m (Event t (Endo Data))
studentsWidget dynData = list (def & listConfig_selection |~ True) $ do
  alter <- listEndoWithKey (_data_students <$> dynData) $ \name feedback -> do
    listItem def $ do
      newKey <- fancyStudent (Just name)
      strengths <- feedbackDropdown dynData Strengths
      improvements <- feedbackDropdown dynData Improvements
      let fbs = M.restrictKeys . _data_feedback <$> dynData <*> feedback
      del <- fmap (switch . current . fmap (leftmost . M.elems)) $ listWithKey fbs $ \f fb -> do
        let c = Just . summaryColour . _feedback_summary <$> fb
        label (def & labelConfig_color .~ Dyn c) $ do
          text f
          (f <$) . domEvent Click <$> icon' "delete" def
      pure $ fold
        [ newKey
        , ffor (strengths <> improvements) $ \g -> Endo $ over data_students (M.adjust (S.insert g) name)
        , ffor del $ \d -> Endo $ over data_students (M.adjust (S.delete d) name)
        ]
  new <- fmap (domEvent Click . fst) $ listItem' def $ do
      icon "plus" $ def & style .~ "float: left; margin: 0 1rem"
      el' "div" (text "Add student")
  let new' = Endo $ over data_students $ M.insert (Name "Student") mempty
  pure $ fold [new' <$ new, alter]


addStudentWidget :: MonadWidget t m => m (Event t (Endo Data))
addStudentWidget = form def $ input (def & inputConfig_action |?~ RightAction) $ mdo
  t <- fmap value $ textInput $ def
    & textInputConfig_placeholder |~ "Add Student"
    & textInputConfig_value .~ SetValue "" (Just $ "" <$ e)
  e <- button (def & buttonConfig_icon |~ True & buttonConfig_type .~ SubmitButton) $ icon "plus" def
  pure $ attachWith (\s _ -> Endo $ over data_students $ M.insert (Name s) mempty) (current t) e

studentsWidgetOld
  :: MonadWidget t m
  => Dynamic t Data -> m (Event t (Endo Data))
studentsWidgetOld dynData = el "table" $ listEndoWithKey (_data_students <$> dynData) $ \n fs -> el "tr" $ do
    deleteThisStudent <- el "td" $ label (def & labelConfig_basic |~ True) $ do
      displayName n
      domEvent Click <$> icon' "delete" def
    alterFeedback <- el "td" $ do
      strengths <- feedbackDropdown dynData Strengths
      improvements <- feedbackDropdown dynData Improvements

      let fbs = M.restrictKeys . _data_feedback <$> dynData <*> fs
      del <- fmap (switch . current . fmap (leftmost . M.elems)) $ listWithKey fbs $ \f fb -> do
        let c = Just . summaryColour . _feedback_summary <$> fb
        label (def & labelConfig_color .~ Dyn c) $ do
          text f
          (f <$) . domEvent Click <$> icon' "delete" def
      pure $ leftmost
        [ S.insert <$> strengths
        , S.insert <$> improvements
        , S.delete <$> del
        ]
    pure $ fold
      [ ffor alterFeedback $ \f -> Endo $ over data_students (M.adjust f n)
      , Endo (over data_students (M.delete n)) <$ deleteThisStudent
      ]

app :: forall t m. MonadWidget t m => m ()
app = mdo

  initData <- loadData
  dynData <- foldDyn appEndo initData $ fold
    [ alterFeedback, loadFeedback
    , addStudent, alterStudents, loadStudents
    ]
  saveData $ updated dynData

  el "h1" $ text appTitle
  el "h2" $ text "Feedback"
  alterFeedback <- feedbackWidget dynData

  -- Load pasted feedback
  loadFeedback <- fmap (Endo . over data_feedback . const) <$>
    loadJson "Paste feedback here" (_data_feedback <$> dynData)

  el "h2" $ text "Students"
  addStudent <- addStudentWidget

  alterStudents <- studentsWidget dynData

  -- Load pasted students
  loadStudents <- fmap (Endo . over data_students . const . M.fromSet (const mempty)) <$>
    loadJson "Paste students here" (M.keysSet . _data_students <$> dynData)

  _ <- divClass "students" $ listWithKey (_data_students <$> dynData) $ \n fs -> divClass "student" $ do
    pageHeader H1 (def & classes .~ "name") $ displayName n
    divClass "feedback" $ do
      let fs' = M.restrictKeys . _data_feedback <$> dynData <*> fs
          filtered n' = M.mapMaybe (\f -> _feedback_message f <$ guard (_feedback_summary f == n')) <$> fs'
      pageHeader H4 def $ text "Strengths"
      _ <- listWithKey (filtered Strengths) $ \_ feedback -> do
        divClass "fb" $ dynText feedback
      pageHeader H4 def $ text "Improvements"
      listWithKey (filtered Improvements) $ \_ feedback -> do
        divClass "fb" $ dynText feedback

  pure ()

summaryDropdown
  :: forall t m. MonadWidget t m
  => Dynamic t Summary -> m (Event t Summary)
summaryDropdown summary = do
  let wrapper :: (forall cfg. HasElConfig t cfg => cfg -> cfg) -> m a -> m (El t, a)
      wrapper f = label' $ f $ def
        & labelConfig_color .~ Dyn (Just . summaryColour <$> summary)
        & labelConfig_horizontal |~ True
      conf = def
        & dropdownConfig_selection |~ False
        & dropdownConfig_inline |~ True
  ini <- attachWith (\s _ -> Just s) (current summary) <$> getPostBuild
  e <- fmap _dropdown_change $ dropdownWithWrapper wrapper conf Nothing ini $
    TaggedStatic $ M.fromList $ (\x -> (x, text $ tshow x)) <$> [minBound..maxBound]
  pure $ fmapMaybe id e

feedbackDropdown
  :: forall t m. MonadWidget t m
  => Dynamic t Data -> Summary -> m (Event t FeedbackKey)
feedbackDropdown dynData summary = mdo
  e' <- delay 0 e
  let wrapper :: (forall cfg. HasElConfig t cfg => cfg -> cfg) -> m a -> m (El t, a)
      wrapper f = label' $ f $ def
        & labelConfig_color |?~ summaryColour summary
        & labelConfig_basic |~ True
      conf = def
        & dropdownConfig_selection |~ False
        & dropdownConfig_inline |~ True
  e <- fmap _dropdown_change $ dropdownWithWrapper wrapper conf Nothing (Nothing <$ e') $
    TaggedDynamic $ M.mapMaybeWithKey (\k f -> text k <$ guard (_feedback_summary f == summary)) . _data_feedback <$> dynData
  pure $ fmapMaybe id e

loadJson :: (MonadWidget t m, FromJSON a, ToJSON a, Monoid a) => Text -> Dynamic t a -> m (Event t a)
loadJson placeholder contents = form def $ input (def & inputConfig_action |?~ RightAction) $ mdo
  ti <- do
    pb <- getPostBuild
    let enc = LT.toStrict . Aeson.encodeToLazyText
        enc' = enc <$> contents
    textInput $ def
      & textInputConfig_placeholder |~ placeholder
      & textInputConfig_value . event ?~ leftmost
        [ updated enc'
        , tag (current enc') pb
        , enc mempty <$ clear
        ]
  positive <- do
    (copyButton, _) <- button' (def & buttonConfig_positive .~ Dyn positive) $ text "Copy"
    (copied, triggerCopy) <- newTriggerEvent
    DOM.liftJSM $ DOM.uncheckedCastTo DOM.HTMLElement (_element_raw copyButton)
      `EventM.on` GlobalEventHandlers.click $ do
        success <- copyToClipboardUsing $ _inputElement_raw $ _textInput_builderElement ti
        liftIO $ triggerCopy success
    done <- delay 0.5 copied
    holdDyn Nothing $ leftmost
      [ Nothing <$ done
      , ffor copied $ Just . \case True -> Success; False -> Error
      ]
  clear <- button (def & buttonConfig_type .~ SubmitButton) $ text "Clear"
  pure $ (mempty <$ clear) <> fmapMaybe (Aeson.decodeStrict . T.encodeUtf8) (_textInput_input ti)

-- | Copy the given text to the clipboard
copyToClipboard
  :: MonadJSM m
  => Text -- ^ Text to copy to clipboard
  -> m Bool -- ^ Did the copy take place successfully?
copyToClipboard t = do
  doc <- DOM.currentDocumentUnchecked
  ta <- DOM.uncheckedCastTo TextArea.HTMLTextAreaElement <$> Document.createElement doc ("textarea" :: Text)
  TextArea.setValue ta t
  body <- Document.getBodyUnchecked doc
  _ <- Node.appendChild body ta
  HTMLElement.focus ta
  TextArea.select ta
  success <- Document.execCommand doc ("copy" :: Text) False (Nothing :: Maybe Text)
  _ <- Node.removeChild body ta
  pure success

-- | Copy the given text to the clipboard
copyToClipboardUsing
  :: MonadJSM m
  => DOM.HTMLInputElement -- ^ TextArea to copy to clipboard
  -> m Bool -- ^ Did the copy take place successfully?
copyToClipboardUsing ta = do
  doc <- DOM.currentDocumentUnchecked
  --ta <- DOM.uncheckedCastTo TextArea.HTMLTextAreaElement <$> Document.createElement doc ("textarea" :: Text)
  body <- Document.getBodyUnchecked doc
  HTMLElement.focus ta
  InputElement.select ta
  success <- Document.execCommand doc ("copy" :: Text) False (Nothing :: Maybe Text)
  pure success
