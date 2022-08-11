# ada-air

## Overview

This is an experimental front-end for Ada analysis in [Rascal](https://www.rascal-mpl.org/).
It uses the [Libadalang](https://github.com/AdaCore/libadalang) library to create abstract syntax trees for Ada source files and maps those to convenient `data` type in Rascal.

## How to setup ada-air

### Dependencies

- [Python >=3.10](https://www.python.org/)
- [Libadalang (automatically installed)](https://github.com/AdaCore/libadalang)
- [Langkit (automatically installed)](https://github.com/AdaCore/langkit)
- [alire](https://github.com/alire-project/alire)
- [maven](https://maven.apache.org/)
- [Rascal Command line REPL](https://www.rascal-mpl.org/start/)
- JDK 11

### Building

Clone this repository: `git clone --recurse-submodules https://github.com/cwi-swat/ada-air.git`

Then run the install script.

To ease the use of this new rascal module, I invite you to add the directory containing the compiled Ada library into your PATH (windows)/LD_LIBRARY_PATH (linux).
