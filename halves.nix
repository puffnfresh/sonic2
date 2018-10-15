{ mkDerivation, base, hedgehog, lens, stdenv }:
mkDerivation {
  pname = "halves";
  version = "0.1.0.1";
  sha256 = "aaf29ccf077afd3dff7ad68acb4bae002e358db9306aaa9b5765a282d5895d56";
  libraryHaskellDepends = [ base lens ];
  testHaskellDepends = [ base hedgehog lens ];
  description = "Split or combine data structures to and from halves, quarters, eighths";
  license = stdenv.lib.licenses.bsd3;
}
