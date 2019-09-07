{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
let pkgs = obelisk.nixpkgs;
in project ./. ({ ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  overrides = self: super: let
    semantic-reflex = (pkgs.fetchFromGitHub {
      owner = "tomsmalley";
      repo = "semantic-reflex";
      rev = "a354fda1f34d06b72fd99dea1206606b5210ecdd";
      sha256 = "1li8w95ibq4xm717clz5wz23kdp15j9vrqb1kq64d5ld0fjx7ln0";
    }) + "/semantic-reflex";
  in {
    semantic-reflex = pkgs.haskell.lib.dontHaddock (self.callCabal2nix "semantic-reflex" semantic-reflex {});
  };
})
