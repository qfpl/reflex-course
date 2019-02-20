{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: let 
  sources = {
    reflex-dom-contrib = pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "bc1b3c0ea689621a0d04b162c9c5ce83497b3ff5";
      sha256 = "0fflq23sr5gxnf2piy3qxdq2aj7bhqacy3skxnw83m01xxq7clnl";
    };
  };
in {
  overrides = self: super: {
    reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" sources.reflex-dom-contrib {};
  };
})
