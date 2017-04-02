{ mkDerivation, ansi-wl-pprint, base, binary, bytestring, cairo
, Chart, Chart-cairo, Chart-diagrams, colour, composition
, data-binary-ieee754, data-default, directory, filepath, hspec
, lens, monad-loops, optparse-applicative, stdenv, transformers
}:
mkDerivation {
  pname = "hgis";
  version = "0.1.3.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base binary bytestring Chart Chart-cairo
    Chart-diagrams colour composition data-binary-ieee754 data-default
    directory filepath lens monad-loops optparse-applicative
    transformers
  ];
  libraryPkgconfigDepends = [ cairo ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/vmchale/hgis#readme";
  description = "Package and command-line for GIS with Haskell";
  license = stdenv.lib.licenses.bsd3;
}
