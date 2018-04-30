{ mkDerivation, base, hedgehog, lens, stdenv }:
mkDerivation {
  pname = "halves";
  version = "0.1.0.0";
  sha256 = "b6a95a9123f1ef0fecbfe394598b2b45cce1f270d93212e73e1728e966d75d19";
  libraryHaskellDepends = [ base lens ];
  testHaskellDepends = [ base hedgehog lens ];
  description = "Splitting/combining data structures to/from halves, quarters, eighths";
  license = stdenv.lib.licenses.bsd3;
}
