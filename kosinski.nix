{ mkDerivation, base, bytestring, deepseq, fetchgit, filepath, lens
, mtl, stdenv, tasty, tasty-hunit, time
}:
mkDerivation {
  pname = "kosinski";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/puffnfresh/kosinski.git";
    sha256 = "1xfz4jh90kn6mzi1y6qvm7d7s1prhb8fis8m3b7xv1bzqpczlzml";
    rev = "96e63c1f5fc2fd4f0fc780336c8a200bd06bf069";
  };
  libraryHaskellDepends = [ base bytestring lens mtl ];
  testHaskellDepends = [ base bytestring tasty tasty-hunit ];
  benchmarkHaskellDepends = [
    base bytestring deepseq filepath time
  ];
  description = "Compression used in Sonic the Hedgehog";
  license = stdenv.lib.licenses.bsd3;
}
