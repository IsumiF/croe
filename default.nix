{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    croe-backend = ./backend;
    croe-common = ./common;
    croe-frontend = ./frontend;
  };

  shells = {
    ghc = ["croe-frontend" "croe-common" "croe-backend"];
    ghcjs = ["croe-frontend" "croe-common"];
  };

  withHoogle = false;

  overrides = self: super: {
    servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
      owner = "isumif";
      repo = "servant-reflex";
      rev = "5a81583863555ecac49a92a82b95041233fad1ac";
      sha256 = "1q413bxjjmw1sfnzhbi1iqkbv53pcxx4knj9xkm40b9n3nlk82x5";
    }) {};
    reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "b9e2965dff062a4e13140f66d487362a34fe58b3";
      sha256 = "1aa045mr82hdzzd8qlqhfrycgyhd29lad8rf7vsqykly9axpl52a";
    }) {};
    Glob = pkgs.haskell.lib.dontCheck super.Glob;
    servant = pkgs.haskell.lib.dontCheck super.servant;
  };
})