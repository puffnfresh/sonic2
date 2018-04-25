with import <nixpkgs> { };

(haskellPackages.override {
  overrides = self: super: {
    kosinski = self.callPackage ./kosinski.nix { };
    megadrive-palette = self.callPackage ./megadrive-palette.nix { };
  };
}).callPackage ./sonic2.nix { }
