#! /usr/bin/env python
import subprocess
import os
import filecmp
import sys

test_dir = os.path.dirname(__file__)
os.chdir(test_dir)
dirs = os.listdir()

for file in dirs:
    if os.path.isdir(file):
        print(file)