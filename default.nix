{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    croe-common = ./common;
    croe-frontend = ./frontend;
    reflex-dom-bulma = ./reflex-dom-bulma;
  };

  shells = {
    ghcjs = ["croe-frontend" "croe-common" "reflex-dom-bulma"];
  };

  withHoogle = false;

  overrides = self: super: {
    servant = pkgs.haskell.lib.dontCheck (self.callHackage "servant" "0.16.2" {});
    servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
      owner = "isumif";
      repo = "servant-reflex";
      rev = "0f29197c52b82418685765d61c784da9422dbbad";
      sha256 = "0i6j2y9nzpffphglnfvbrk07w05zb3hhbl5yshsm305nwmjjvncw";
    }) {};
    reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "b9e2965dff062a4e13140f66d487362a34fe58b3";
      sha256 = "1aa045mr82hdzzd8qlqhfrycgyhd29lad8rf7vsqykly9axpl52a";
    }) {};
    Glob = pkgs.haskell.lib.dontCheck super.Glob;
  };
})