{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./work1.nix {}
