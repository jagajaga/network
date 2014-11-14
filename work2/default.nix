{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./work2.nix {}
