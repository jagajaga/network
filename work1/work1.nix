{ cabal, networkInfo, network, bytestring, transformers }:

cabal.mkDerivation (self: {
  pname = "jid";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    network networkInfo bytestring transformers
  ];
})
