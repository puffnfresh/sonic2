with import <nixpkgs> { };

haskellPackages.developPackage {
  root = ./.;
  overrides = self: super: {
    bounded-array = self.callPackage ./bounded-array.nix { };
    halves = self.callPackage ./halves.nix { };
    kosinski = self.callPackage ./kosinski.nix { };
    megadrive-palette = self.callPackage ./megadrive-palette.nix { };
  };
}
