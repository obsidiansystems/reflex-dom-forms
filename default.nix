let
  reflex-platform = import ./dep/reflex-platform {};
in
reflex-platform.project ({ pkgs, ... }: {
  packages = {
    reflex-dom-forms = ./.;
  };

  shells = {
    ghc = ["reflex-dom-forms"];
    ghcjs = ["reflex-dom-forms"];
  };
})
