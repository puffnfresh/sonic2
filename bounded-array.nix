{ mkDerivation, array, base, fetchgit, stdenv }:
mkDerivation {
  pname = "bounded-array";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/puffnfresh/bounded-array.git";
    sha256 = "09v47c3v5ncvln2asq8ijlrb4f4agh0ibdaypmls8pnwhmh32p6g";
    rev = "58990b068e8146ce29e177db0cd2cde85a0c125a";
  };
  libraryHaskellDepends = [ array base ];
  description = "Arrays with a value for every index";
  license = stdenv.lib.licenses.bsd3;
}
