{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, hspec, hspec-discover
      , stdenv
      }:
      mkDerivation {
        pname = "multi-containers";
        version = "0.1.1";
        src = ./.;
        libraryHaskellDepends = [ base containers ];
        testHaskellDepends = [ base containers hspec ];
        testToolDepends = [ hspec-discover ];
        homepage = "https://github.com/zliu41/multi-containers#readme";
        description = "A few multimap variants";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
