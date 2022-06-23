#! /usr/bin/env python
import subprocess
import os
import sys
import re
import platform

if os.name == 'nt' and platform.release() == '10' and platform.version() >= '10.0.14393':
    # Fix ANSI color in Windows 10 version 10.0.14393 (Windows Anniversary Update)
    import ctypes
    kernel32 = ctypes.windll.kernel32
    kernel32.SetConsoleMode(kernel32.GetStdHandle(-11), 7)


RED = '\033[31m'
ENDC = '\033[m'
GREEN = '\033[32m'

def run_rascal(filename, args):
    rascal_jar = os.environ["RASCAL_JAR"]
    proc = subprocess.run(['java', '-Xmx1G', '-Xss32m', '-jar', rascal_jar, filename] + args,  capture_output=True, shell=True)
    out = proc.stdout.decode("utf8")
    assert proc.returncode == 0
    return out

def clean(s):
    return re.sub(r"[\n\t\s\r]*", "", s)

def run_test(dir, filename):
    out = clean(run_rascal(dir + "/" + filename, []))
    with open(dir + '/expected.txt') as f:
        expected = clean(f.read())
        assert out == expected

def print_output(name, color, res):
    max = 50 - len(name)
    padding = max * "." + " "
    print(padding + color + res + ENDC)
    sys.stdout.flush()

test_dir = os.path.dirname(__file__)
os.chdir(test_dir)
dirs = os.listdir()
succes = True
for test_dir in dirs:
    if os.path.isdir(test_dir) and test_dir.startswith("test_"):
        print(test_dir, end=" ")
        sys.stdout.flush()
        for file in os.listdir(test_dir):
            if os.path.isfile(test_dir +"/"+ file) and file.endswith(".rsc"):
                try:
                    run_test(test_dir, file)
                    print_output(test_dir, GREEN, "OK")
                except Exception:
                    print_output(test_dir, RED, "KO")
                    succes = False

if not succes:
    sys.exit(1)