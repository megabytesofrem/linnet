{ pkgs ? import <nixpkgs> {} }:

let 
  haskellPackages = pkgs.haskell.packages.ghc96;
in
pkgs.mkShell {
  buildInputs = [
      haskellPackages.ghc
      haskellPackages.cabal-install
      haskellPackages.haskell-language-server
      pkgs.zlib
  ];

  # This helps VS Code and HLS find the library paths
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.zlib ]}:$LD_LIBRARY_PATH
  '';
}