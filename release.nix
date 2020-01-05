{ compiler ? "default" }:

let
  pkgs = import <nixpkgs> { };

in
  { multi-containers = pkgs.haskellPackages.callPackage ./default.nix { inherit compiler; };
  }
