{ mkDerivation, array, base, fetchgit, stdenv }:
mkDerivation {
  pname = "bounded-array";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/puffnfresh/bounded-array.git";
    sha256 = "09h41d2g0vn5ninynclc6hfhpc0vilky9rrzfjs8y64liycsrwzv";
    rev = "005130846ec90d9dd7f46ec3b9477741855a996d";
  };
  libraryHaskellDepends = [ array base ];
  description = "Arrays with a value for every index";
  license = stdenv.lib.licenses.bsd3;
}
