{ cabal, networkInfo, network, transformers, time, binary }:

cabal.mkDerivation (self: {
  pname = "jid";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    network networkInfo transformers time binary
  ];
})
