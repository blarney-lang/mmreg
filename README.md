# AXI4 memory-mapped register

This library provides a memory-mapped register (MMReg) with an AXI4
interface, implemented in
[Blarney](https://github.com/blarney-lang/blarney). It serves as a
simple example of how to develop an AXI4 component in Blarney.

## Quick start

After meeting [Blarney's
prerequisites](https://github.com/blarney-lang/blarney#prerequisites)
and cloning the repo

```
git clone https://github.com/blarney-lang/mmreg
```

you can generate Verilog for an MMReg:

```
cabal run blarney-mmreg-gen
```

This will produce the file `MMReg.v`.  To run the test suite, use `cabal
test`.

## Overview

The library comprises:

  * [MMReg.hs](src/Blarney/MMReg.hs): Haskell source file
    implementing the MMReg component.

  * [Main.hs](gen/Main.hs): Verilog generator for an MMReg with
    a default configuration.

  * [Tests.hs](tests/Tests.hs): Simple test bench for the MMReg.

  * [blarney-mmreg.cabal](blarney-mmreg.cabal): Cabal package file,
    listing dependencies, exposed modules, compiler flags etc.

  * [cabal.project](blarney-mmreg.cabal): Cabal project file, telling
    cabal where to find the dependencies (github in our case).
