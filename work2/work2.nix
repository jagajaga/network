{ cabal, networkInfo, network, transformers, time, binary, ncurses, ansiTerminal, lens, stm }:

cabal.mkDerivation (self: {
  pname = "work2";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    network networkInfo transformers time binary ncurses ansiTerminal lens stm
  ];
})
