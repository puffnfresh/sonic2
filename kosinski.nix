{ mkDerivation, base, bytestring, fetchgit, lens, stdenv }:
mkDerivation {
  pname = "kosinski";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/puffnfresh/kosinski.git";
    sha256 = "1yfc6v5rw1mg8j6m11bspm2fh2njsnlr7qa3dzbaid004pl1cdr1";
    rev = "e745fcf519db09da6047ad72fc388e18a64b542f";
  };
  libraryHaskellDepends = [ base bytestring lens ];
  description = "Compression used in Sonic the Hedgehog";
  license = stdenv.lib.licenses.bsd3;
}
