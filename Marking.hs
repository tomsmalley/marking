{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

import Control.Lens (over, set, _1, _2)
import Control.Lens.TH (makeLenses)
import Control.Monad (when, void, join, replicateM_)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:))
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Endo (..))
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCJS.DOM.Types (MonadJSM, liftJSM)
import Reflex.Dom.SemanticUI
import Text.RawString.QQ

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.FileReader     as FileReader
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Storage as Storage
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Types as Types
import qualified GHCJS.DOM.Window as Window
#ifndef ghcjs_HOST_OS
import qualified Language.Javascript.JSaddle.Warp as Warp
#endif

type AppWidget js t m = (MonadWidget t m, Prerender js t m)

-- | If the element exists, delete it; otherwise insert it.
flipSet :: Ord a => a -> Set a -> Set a
flipSet a s = if S.member a s then S.delete a s else S.insert a s

-- | Alter the item if it exists
alterMember :: Ord k => k -> k -> Set k -> Set k
alterMember old new s
  | S.member old s = S.insert new $ S.delete old s
  | otherwise = s

-- | Alter the key if it exists
alterKey :: Ord k => k -> k -> Map k a -> Map k a
alterKey old new m = case M.lookup old m of
  Just v -> M.insert new v $ M.delete old m
  Nothing -> m

data Summary
  = Excellent
  | Good
  | Improvements
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)
instance ToJSON Summary where
  toJSON = \case
    Excellent -> "excellent"
    Good -> "good"
    Improvements -> "improvements"
instance FromJSON Summary where
  parseJSON = Aeson.withText "Summary" $ \t -> case T.toLower t of
    "strengths" -> pure Excellent
    "excellent" -> pure Excellent
    "good" -> pure Good
    "improvements" -> pure Improvements
    _ -> fail "Unexpected value for Summary"

summaryColour :: Summary -> Color
summaryColour = \case
  Excellent -> Green
  Good -> Yellow
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
  { _feedback_summary = Good
  , _feedback_message = "Edit here"
  }

-- | Entire app state data structure, for serialising
data Data = Data
  { _data_feedback :: Map FeedbackKey Feedback
  , _data_students :: Map Name (Set FeedbackKey, Text)
  , _data_title :: Text
  } deriving (Eq, Ord, Show, Generic)
instance ToJSON Data
instance FromJSON Data
makeLenses 'Data

appTitle :: Text
appTitle = "Mega marking tool 5000"

-- | Key at which to store data in session storage
dataStorageKey :: Text
dataStorageKey = "data"

-- | Load saved data from session storage
loadData :: MonadJSM m => m Data
loadData = liftJSM $ do
  storage <- Window.getSessionStorage =<< DOM.currentWindowUnchecked
  mx <- Storage.getItem storage dataStorageKey
  pure $ fromMaybe (Data mempty mempty "") $ Aeson.decodeStrict . T.encodeUtf8 =<< mx

-- | Save data to session storage
saveData :: (MonadJSM (Performable m), PerformEvent t m) => Event t Data -> m ()
saveData dd = performEvent_ $ ffor dd $ \d -> liftJSM $ do
  window <- DOM.currentWindowUnchecked
  storage <- Window.getSessionStorage window
  Storage.setItem storage dataStorageKey $ LT.toStrict $ Aeson.encodeToLazyText d

-- | Button to save some data to a file
saveFile :: (ToJSON a, AppWidget js t m) => Text -> Text -> Dynamic t a -> m ()
saveFile lbl name d = do
  let b64 = T.decodeUtf8 . B64.encode . LBS.toStrict . Aeson.encode <$> d
      as = ffor b64 $ \b -> "download" =: name <> "href" =: ("data:text/plain;base64," <> b)
  void $ button (def & buttonConfig_type .~ LinkButton & attrs .~ Dyn as) $ text lbl

-- | Button to load some data from a file
loadFile :: (FromJSON a, AppWidget js t m) => Text -> m (Event t a)
loadFile lbl = do
  fileInput <- inputElement $ def & set initialAttributes fileInputAttrs
  let newFile = fmapMaybe listToMaybe $ updated $ _inputElement_files fileInput
  loaded <- performEventAsync $ ffor newFile $ \file cb -> liftJSM $ do
    fr <- FileReader.newFileReader
    FileReader.readAsText fr (pure file) (Nothing @Text)
    void $ fr `EventM.on` FileReader.loadEnd $ liftJSM $ do
      liftIO . cb . join =<< traverse (Types.fromJSVal . Types.unStringOrArrayBuffer) =<< FileReader.getResult fr
  (e, _) <- button' def $ text lbl
  let htmlElement = DOM.HTMLElement . DOM.unElement $ _element_raw e
  void $ liftJSM $ htmlElement `EventM.on` GlobalEventHandlers.click $ liftJSM $
    HTMLElement.click (_inputElement_raw fileInput)
  pure $ fmapMaybe (Aeson.decodeStrict . T.encodeUtf8 =<<) loaded
  where
    fileInputAttrs = M.fromList
      [ ("type", "file")
      , ("style", "display: none")
      , ("accept", ".txt")
      ]

listEndoWithKey
  :: (Ord k, Adjustable t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t (Endo a))) -> m (Event t (Endo a))
listEndoWithKey l = (fmap . fmap) fold . listViewWithKey l

editableContent :: AppWidget js t m => m () -> m (Event t (Maybe Text))
editableContent content = mdo
  let s = "min-width: 1rem; display: inline-block; outline: none"
  e <- fst <$> elAttr' "span" ("contenteditable" =: "true" <> "style" =: s) content
  let htmlElement = DOM.HTMLElement . DOM.unElement $ _element_raw e
  void $ liftJSM $ htmlElement `EventM.on` GlobalEventHandlers.keyDown $ do
    key <- EventM.uiKeyCode
    when (key == 13) $ HTMLElement.blur htmlElement
  performEvent $ ffor (domEvent Blur e) $ \_ -> do
    tc <- Node.getTextContent htmlElement
    Node.setTextContent htmlElement tc
    pure tc

fancyFeedback
  :: AppWidget js t m
  => Dynamic t Summary
  -> Maybe FeedbackKey
  -> m (Event t (Endo Data))
fancyFeedback summary mFeedback = do
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
        . over data_students (M.map (over _1 $ S.delete k))
      f (Just k') (Just k)
        = over data_feedback (alterKey k' k)
        . over data_students (M.map (over _1 $ alterMember k' k))
  mapAccum_ (\old new -> (new, Endo $ f old new)) mFeedback newText

feedbackWidget
  :: AppWidget js t m
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

  new <- form def $ input (def & inputConfig_action |?~ RightAction) $ mdo
    t <- fmap value $ textInput $ def
      & textInputConfig_placeholder |~ "Add Feedback"
      & textInputConfig_value .~ SetValue "" (Just $ "" <$ e)
    e <- button (def & buttonConfig_icon |~ True & buttonConfig_type .~ SubmitButton) $ icon "plus" def
    pure $ attachWith (\f _ -> Endo $ over data_feedback $ M.insert f defaultFeedback) (current t) e

  pure $ fold [new, alter]

stopClickPropagation :: (MonadJSM m, RawElement d ~ Types.Element) => Element er d t -> m ()
stopClickPropagation e = do
  let htmlElement = DOM.HTMLElement . DOM.unElement $ _element_raw e
  void $ liftJSM $ htmlElement `EventM.on` GlobalEventHandlers.click $ EventM.stopPropagation

fancyStudent
  :: AppWidget js t m
  => Maybe Name
  -> m (Event t (Endo Data))
fancyStudent mName = do
  let conf = def & labelConfig_basic |~ True
  (e, newText) <- label' conf $ do
    newText <- editableContent $ maybe blank displayName mName
    del <- domEvent Click <$> icon' "delete" def
    pure $ leftmost [fmap Name <$> newText, Nothing <$ del]
  stopClickPropagation e
  let f Nothing Nothing = id
      f Nothing (Just n) = over data_students (M.insert n mempty)
      f (Just n) Nothing = over data_students (M.delete n)
      f (Just n') (Just n)
        = over data_students (M.mapKeysWith (<>) (\k -> if k == n' then n else k))
  mapAccum_ (\old new -> (new, Endo $ f old new)) mName newText

studentsWidget
  :: AppWidget js t m
  => Dynamic t Data -> m (Event t (Endo Data))
studentsWidget dynData = list (def & listConfig_selection |~ True) $ do
  alter <- listEndoWithKey (_data_students <$> dynData) $ \name feedback -> do
    (e, (newKey, swap)) <- listItem' def $ do
      newKey <- fancyStudent (Just name)
      swap <- fmap (switch . current . fmap (leftmost . M.elems)) $ listWithKey (_data_feedback <$> dynData) $ \k f -> do
        let sel = S.member k . fst <$> feedback
            conf = def
              & labelConfig_basic .~ Dyn (fmap not sel)
              & labelConfig_color .~ Dyn (Just . summaryColour . _feedback_summary <$> f)
        (e, _) <- label' conf $ text k
        stopClickPropagation e
        pure $ k <$ domEvent Click e
      text " "
      display $ T.length . snd <$> feedback
      pure (newKey, swap)
    let mkAction = def
          { _action_transition = Transition Instant Nothing def <$ domEvent Click e
          , _action_initialDirection = Out
          }
    comments <- form (def & action ?~ mkAction) $ input (def & inputConfig_fluid |~ True) $ do
      initialComment <- sample $ snd <$> current feedback
      fmap _textAreaElement_input $ textAreaElement $ def
        & initialAttributes .~ "placeholder" =: "Additional comments"
        & textAreaElementConfig_initialValue .~ initialComment
    pure $ fold
      [ newKey
      , ffor swap $ \d -> Endo $ over data_students (M.adjust (over _1 $ flipSet d) name)
      , ffor comments $ \c -> Endo $ over data_students (M.adjust (set _2 c) name)
      ]
  new <- form def $ input (def & inputConfig_action |?~ RightAction) $ mdo
    t <- fmap value $ textInput $ def
      & textInputConfig_placeholder |~ "Add Student"
      & textInputConfig_value .~ SetValue "" (Just $ "" <$ e)
    e <- button (def & buttonConfig_icon |~ True & buttonConfig_type .~ SubmitButton) $ icon "plus" def
    pure $ attachWith (\s _ -> Endo $ over data_students $ M.insert (Name s) mempty) (current t) e
  pure $ new <> alter

summaryDropdown
  :: forall js t m. AppWidget js t m
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

-- | Main app
app :: AppWidget js t m => m ()
app = do
  dynData <- container def $ mdo

    initData <- loadData
    dynData <- foldDyn appEndo initData $ alterFeedback <> alterStudents <> alterTitle <> fmap (Endo . const) loadAll
    saveData $ updated dynData

    loadAll <- segment (def & segmentConfig_color |?~ Purple) $ pageHeader H1 def $ do
      text appTitle
      subHeader $ text "This tool will save your current data in the browser until you close the window."
      saveFile "Save All" "snapshot" dynData
      loadFile "Load All"

    alterTitle <- do
      i <- input (def & inputConfig_fluid |~ True) $ textInput $ def
        & textInputConfig_placeholder .~ "Title (appears on printout)"
        & textInputConfig_value .~ SetValue (_data_title initData) (Just $ _data_title <$> loadAll)
      pure $ Endo . set data_title <$> _textInput_input i

    alterFeedback <- segment def $ do
      header def $ do
        text "Feedback"
        divClass "sub header" $ text "Add different feedback types here. Feedback is referenced by the short name, but the printout will display the full description."
      alter <- feedbackWidget dynData
      clear <- (Endo (set data_feedback mempty . over data_students (M.map (over _1 $ const mempty))) <$) <$> button def (text "Clear")
      load <- fmap (Endo . set data_feedback) <$> loadFile "Load"
      saveFile "Save" "feedback" $ _data_feedback <$> dynData
      pure $ clear <> alter <> load

    alterStudents <- segment def $ do
      header def $ do
        text "Students"
        divClass "sub header" $ text "Add a class of students and give them feedback. Saving/loading will only load the students, not the feedback associated with them."
      alter <- studentsWidget dynData
      clear <- (Endo (set data_students mempty) <$) <$> button def (text "Clear")
      load <- fmap (Endo . set data_students . M.fromSet (const mempty)) <$> loadFile "Load Class"
      saveFile "Save Class" "students" $ M.keysSet . _data_students <$> dynData
      pure $ clear <> alter <> load

    pure dynData

  divClass "students" $ void $ listWithKey (_data_students <$> dynData) $ \n fs -> divClass "student" $ do
    divClass "student-name" $ text "Name: " >> displayName n
    divClass "feedback-title" $ dynText $ _data_title <$> dynData
    let feedbackBlock f = do
          let fs' = ffor2 dynData fs $ \d ->
                  M.filter (\fb -> _feedback_summary fb == f)
                . M.restrictKeys (_data_feedback d)
                . fst
              as = ffor fs' $ \m -> "class" =: (if M.null m then ("hidden " <>) else id) "feedback-block"
          elDynAttr "div" as $ do
            divClass "type" $ text $ tshow f <> ":"
            void $ el "ul" $ listWithKey fs' $ \k fb -> el "li" $ do
              text $ k <> ": "
              dynText $ _feedback_message <$> fb
    feedbackBlock Excellent
    feedbackBlock Good
    feedbackBlock Improvements
    let as = ffor fs $ \f -> "class" =: (if T.null (snd f) then ("hidden " <>) else id) "feedback-block grow"
    elDynAttr "div" as $ do
      divClass "type" $ text "Additional comments:"
      dynText $ snd <$> fs
    divClass "self-reflection" $ do
      text "Self-reflection: What do you need to focus on next time?"
      replicateM_ 10 $ divClass "line" blank

headContents :: DomBuilder t m => m ()
headContents = do
  el "title" $ text appTitle
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css") blank
  el "style" $ text [r|
    @media print {
      body > * { display: none !important }
      body > .students { display: block !important }
    }
    body { padding: 1rem; }
    .students { display: none; }
    .students .student { page-break-after: always; display: flex; flex-direction: column; height: calc(100vh - 1rem); }
    .student-name { position: absolute; }
    .feedback-title { text-align: center; text-decoration: underline; }
    .feedback-block { border: 1px solid black; padding: 1rem; margin: 1rem 0; }
    .feedback-block.grow { flex-grow: 1; }
    .feedback-block .type { text-align: center; text-decoration: underline; margin-bottom: 1rem; }
    .feedback-block.hidden:not(.grow) { display: none; }
    .feedback-block.grow.hidden { visibility: hidden; }
    .self-reflection { text-align: center; font-weight: bold; text-decoration: underline; margin: 3rem 0; }
    .line { height: 1.5rem; border-bottom: 1px solid black; }
  |]

main :: IO ()
main =
#ifndef ghcjs_HOST_OS
  Warp.run 8000 $
#endif
    mainWidgetWithHead headContents app
