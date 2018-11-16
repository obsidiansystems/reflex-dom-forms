let
  reflex-platform = import ./dep/reflex-platform {};
in
reflex-platform.project ({ pkgs, ... }: {
  packages = {
    reflex-dom-forms = ./.;
  };

  overrides = self: super: let haskellLib = pkgs.haskell.lib; in {
    reflex-dom-core = haskellLib.overrideCabal super.reflex-dom-core (drv: {
      src = reflex-platform.hackGet ./dep/reflex-dom + "/reflex-dom-core";
    });
  };

  shells = {
    ghc = ["reflex-dom-forms"];
    ghcjs = ["reflex-dom-forms"];
  };
})
