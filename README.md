# ada-air

## Overview

This is an experimental front-end for Ada analysis in [Rascal](https://www.rascal-mpl.org/).
It uses the [Libadalang](https://github.com/AdaCore/libadalang) library to create abstract syntax trees for Ada source files and maps those to convenient `data` type in Rascal.

## How to use ada-air

### Dependencies

- [Libadalang](https://github.com/AdaCore/libadalang)
- [Langkit](https://github.com/AdaCore/langkit)

### Generate source code

Be careful to use the same Libadalang and Langkit branch.

Add the [Langkit plugin directory](./src/langkit-plugin/) into your `PYTHONPATH` and generate Libadalang source code with the plugin by adding the following command line argument `--plugin-pass=rascal_plugin.RascalPass`
