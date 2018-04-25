{ mkDerivation, base, bytestring, fetchgit, stdenv }:
mkDerivation {
  pname = "megadrive-palette";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/puffnfresh/megadrive-palette.git";
    sha256 = "0fca8rra4ybksz3gfrlx2aiw1cfnppiccbzdfb3j2ma5gl1z5zxa";
    rev = "add1de034ba466c6976241c0af8588735c206635";
  };
  libraryHaskellDepends = [ base bytestring ];
  description = "Palettes for Sega Mega Drive (and Genesis)";
  license = stdenv.lib.licenses.bsd3;
}
