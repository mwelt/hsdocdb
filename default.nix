{ mkDerivation, aeson, async-pool, base, binary, bytestring
, connection, containers, directory, exception-transformers, hexpat
, http-client, lens, lens-aeson, mtl, say, stdenv
, string-conversions, text, transformers, unordered-containers
, utf8-string, vector, yaml, zlib
}:
mkDerivation {
  pname = "hsrelex-docdb";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async-pool base binary bytestring connection containers
    directory exception-transformers hexpat http-client lens lens-aeson
    mtl say string-conversions text transformers unordered-containers
    utf8-string vector yaml zlib
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
