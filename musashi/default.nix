{ stdenv, fetchFromGitHub }:

let
  rev = "fe93f3a823604534cf6583dfc5d57557036af5bc";
in
stdenv.mkDerivation rec {
  name = "musashi-${version}";
  version = stdenv.lib.strings.substring 0 7 rev;
  src = fetchFromGitHub {
    owner = "kstenerud";
    repo = "Musashi";
    inherit rev;
    sha256 = "192mkxplxa67h2xiid137xmf15wg083zz8kn9j7bl8dcr921dqmr";
  };
  patchPhase = ''
    # The MAME headers are not packaged, so exclude by default
    sed -i 's/M68K_COMPILE_FOR_MAME      OPT_ON/M68K_COMPILE_FOR_MAME      OPT_OFF/' m68kconf.h
  '';
  buildPhase = ''
    $CC -o m68kmake m68kmake.c
    mkdir src
    ./m68kmake src
    cp {m68k.h,m68kconf.h,m68kcpu.*} src

    $CC -shared -o libmusashi.so src/*.c
  '';
  installPhase = ''
    mkdir -p $out/lib
    mv libmusashi.so $out/lib
    mv src $out
  '';
}
