{
  description = "Containers that may not be empty, by construction";

  inputs = {
    nixpkgs.url          = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url      = "github:sixears/flake-build-utils/r1.0.0.12";

    monaderror-io.url    = "github:sixears/monaderror-io/r1.2.5.13";
    more-unicode.url     = "github:sixears/more-unicode/r0.0.17.9";
    parsec-plus-base.url = "github:sixears/parsec-plus-base/r1.0.5.15";
    tfmt.url             = github:sixears/tfmt/r0.2.7.16;
  };

  outputs = { self, nixpkgs, build-utils
            , monaderror-io, more-unicode, parsec-plus-base, tfmt }:
    build-utils.lib.hOutputs self nixpkgs "quasiquoting" {
      deps = {
        inherit monaderror-io more-unicode parsec-plus-base tfmt;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
