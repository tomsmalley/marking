(import ./reflex-platform {}).project ({ pkgs, hackGet, ... }: {
  packages = {
    marking = ./.;
  };
  overrides = self: super: with pkgs.haskell.lib; {
    semantic-reflex = dontHaddock (self.callCabal2nix
      "semantic-reflex"
      (pkgs.fetchFromGitHub {
        owner = "tomsmalley";
        repo = "semantic-reflex";
        rev = "f91f45348dc372522f56d41e1c3b64d3e1bb13c1";
        sha256 = "14x2i8ngwgmap8kw98a3rbxmzhhmdzc5qan9ld404d3pq451if35";
      } + "/semantic-reflex") {});
  };
  shells = {
    ghc = ["marking"];
    ghcjs = ["marking"];
  };
})
