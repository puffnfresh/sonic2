with import <nixpkgs> { };

haskellPackages.ghcWithPackages (p: [
  (haskell.lib.appendConfigureFlag (p.callPackage ./. {
    musashi = callPackage ./libmusashi.nix { };
  }) "--gcc-option=-shared")
])
