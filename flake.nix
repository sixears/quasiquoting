{
  description = "Containers that may not be empty, by construction";

  inputs = {
    nixpkgs.url          = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url      = github:sixears/flake-build-utils/r1.0.0.13;

    monaderror-io.url    = github:sixears/monaderror-io/r1.2.5.20;
    more-unicode.url     = github:sixears/more-unicode/r0.0.17.12;
    parsec-plus-base.url = github:sixears/parsec-plus-base/r1.0.5.23;
    tfmt.url             = github:sixears/tfmt/r0.2.7.25;
  };

  outputs = { self, nixpkgs, build-utils
            , monaderror-io, more-unicode, parsec-plus-base, tfmt }:
    build-utils.lib.hOutputs self nixpkgs "quasiquoting" {
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, data-default, lens
                    , template-haskell, text
                    }:
        mkDerivation {
          pname = "quasiquoting";
          version = "1.0.1.32";
          src = ./.;
          libraryHaskellDepends = [
            base base-unicode-symbols data-default lens template-haskell text
          ] ++ mapPkg [ monaderror-io more-unicode parsec-plus-base tfmt ] ;
          description = "manage info.yaml";
          license = lib.licenses.mit;
        };
    };
}
