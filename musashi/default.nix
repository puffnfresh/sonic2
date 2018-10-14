{ mkDerivation, base, bytestring, musashi, stdenv }:
mkDerivation {
  pname = "musashi";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring ];
  libraryPkgconfigDepends = [ musashi ];
  description = "Musashi m68k emulator bindings";
  license = stdenv.lib.licenses.mit;
}
