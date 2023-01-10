let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz";
    sha256 = "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
  }) {};
in
  with pkgs;

  mkShell rec {
    buildInputs = [
      act
      cabal-install
      haskell.compiler.ghc902
      haskellPackages.cabal-fmt
      haskellPackages.ormolu
      haskellPackages.tasty-discover
      hlint
      gmp
      pandoc
      pkgconfig
      stylish-haskell
      zlib
    ];

    # Ensure that libz.so and other libraries are available to TH splices, cabal
    # repl, etc.
    LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
  }
