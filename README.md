# ada-air

## Overview

This is an experimental front-end for Ada analysis in [Rascal](https://www.rascal-mpl.org/).
It uses the [Libadalang](https://github.com/AdaCore/libadalang) library to create abstract syntax trees for Ada source files and maps those to convenient `data` type in Rascal.

## How to setup ada-air

### Dependencies

- [Python >=3.10](https://www.python.org/)
- [Libadalang](https://github.com/AdaCore/libadalang)
- [Langkit](https://github.com/AdaCore/langkit)
- [alire](https://github.com/alire-project/alire)
- [maven](https://maven.apache.org/)

### Generating source code

Be careful to use the Libadalang and Langkit branch mentioned in the [alire toml file](https://github.com/cwi-swat/ada-air/blob/main/src/main/ada/alire.toml).

First install Langkit with the following command: `pip install .`

Then add the [Langkit plugin directory](./src/langkit-plugin/) into your `PYTHONPATH` and generate Libadalang source code with this plugin: `./manage.py generate --plugin-pass=rascal_plugin.RascalPass`

You can now compile the generated Ada library in [src/main/ada](https://github.com/cwi-swat/ada-air/tree/main/src/main/ada) with alire: `alr build`

Last step is to compile the Java classes using maven with: `mvn compile` 

To ease the use of this new rascal module, I invite you to add the directory containing the Ada library into your PATH (windows)/LD_LIBRARY_PATH (linux).
