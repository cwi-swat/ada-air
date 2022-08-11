#!/bin/bash
if [[ -z "${JAVA_HOME}" ]]; then
  echo "Please set the JAVA_HOME environment variable"
  exit
fi
pip install -U git+https://github.com/AdaCore/langkit.git@22.0
PYTHONPATH=./src/langkit-plugin/ python ./Libadalang/manage.py generate --plugin-pass=rascal_plugin.RascalPass
cd src/main/ada/; alr build; cd -
mvn compile
