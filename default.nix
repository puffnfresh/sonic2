with import <nixpkgs> { };

haskellPackages.developPackage {
  root = ./.;
  overrides = self: super: {
    kosinski = self.callPackage ./kosinski.nix { };
    bounded-array = self.callPackage ./bounded-array.nix { };
    megadrive-palette = self.callPackage ./megadrive-palette.nix { };
  };
}
