# ada-air

## Overview

This is an experimental front-end for Ada analysis in [Rascal](https://www.rascal-mpl.org/).
It uses the [Libadalang](https://github.com/AdaCore/libadalang) library to create abstract syntax trees for Ada source files and maps those to convenient `data` type in Rascal.

## How to setup ada-air

### Dependencies

- [Python 3.9 or 3.10](https://www.python.org/)
- [Libadalang (automatically installed)](https://github.com/AdaCore/libadalang)
- [Langkit (automatically installed)](https://github.com/AdaCore/langkit)
- [alire](https://github.com/alire-project/alire)
- [maven](https://maven.apache.org/)
- [Rascal Command line REPL](https://www.rascal-mpl.org/start/)
- JDK 11

### Building

Clone this repository: `git clone --recurse-submodules https://github.com/cwi-swat/ada-air.git`

Run the install script.

#### Advices

To ease the use of this project, you can use an alias for the following command:

`java -Xmx1g -Xss32m -Djava.library.path=absolute-path-of-the-ada-library -jar absolute-path-of-the-rascal-jar`

You can also add the directory containing the compiled Ada library into your PATH (windows)/LD_LIBRARY_PATH (linux) so that you can omit the `-Djava.library.path=` argument. Moreover that makes this project works with the vscode Rascal extension.

### Examples

There are several analysis examples in this repository.

In order to launch them you need to be inside the directory containing the rascal file.

Launching the cyclomatic complexity analysis:

`java -Xmx1g -Xss32m -Djava.library.path=../../src/main/ada/lib -jar absolute-path-to-the-rascal-jar cyclomatic_comp.rsc --args absolute-path-to-your-gpr`
