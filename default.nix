{ mkDerivation, base, brick, lens, random-shuffle, stdenv, vty }:
mkDerivation {
  pname = "srs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base brick lens random-shuffle vty ];
  license = stdenv.lib.licenses.publicDomain;
}
