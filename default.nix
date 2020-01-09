{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    croe-common = ./common;
    croe-frontend = ./frontend;
  };

  shells = {
    ghcjs = ["croe-frontend" "croe-common"];
  };

  withHoogle = false;

  overrides = self: super: {
    servant = pkgs.haskell.lib.dontCheck (self.callHackage "servant" "0.16.2" {});
    servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
      owner = "isumif";
      repo = "servant-reflex";
      rev = "618dc840fda8b790ac450c097f5b3a5b3148aeea";
      sha256 = "1mdfnx9md6c0f04q74h8yrjhvmxbpm5cpkkvlzmpqr6rbhzm8ajk";
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