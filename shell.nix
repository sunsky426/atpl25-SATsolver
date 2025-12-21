{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.blas
    pkgs.lapack
    pkgs.gfortran
  ];
}

