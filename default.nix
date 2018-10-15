with import <nixpkgs> { };

haskellPackages.developPackage {
  root = ./.;
  overrides = self: super: {
    bounded-array = self.callPackage ./bounded-array.nix { };
    halves = self.callPackage ./halves.nix { };
    kosinski = self.callPackage ./kosinski.nix { };
    megadrive-palette = self.callPackage ./megadrive-palette.nix { };
    sdl2 = haskell.lib.dontCheck super.sdl2;
    musashi = haskell.lib.appendConfigureFlag (self.callPackage ./musashi {
      musashi = callPackage ./musashi/libmusashi.nix { };
    }) "--gcc-option=-shared";
  };
}
