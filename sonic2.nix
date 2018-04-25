{ mkDerivation, array, base, bytestring, filepath, kosinski, lens
, megadrive-palette, mtl, sdl2, split, stdenv
}:
mkDerivation {
  pname = "sonic2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base bytestring filepath kosinski lens megadrive-palette mtl
    sdl2 split
  ];
  license = stdenv.lib.licenses.bsd3;
}
