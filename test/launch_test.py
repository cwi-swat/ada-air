#! /usr/bin/env python
import subprocess
import os
import sys
import re
import platform

RED = '\033[31m'
ENDC = '\033[m'
GREEN = '\033[32m'

libs = ["libadalang", "xmlada", "gnatcoll", "langkit_support", "aws"]
rascal_jar = None


if os.name == 'nt' and platform.release() == '10' and platform.version() >= '10.0.14393':
    # Fix ANSI color in Windows 10 version 10.0.14393 (Windows Anniversary Update)
    import ctypes
    kernel32 = ctypes.windll.kernel32
    kernel32.SetConsoleMode(kernel32.GetStdHandle(-11), 7)

def run_rascal(filename, args):
    proc = subprocess.run(['java', '-Xmx1G', '-Xss32m', '-jar', rascal_jar, filename, ('--args' if len(args)>0 else '')] + args,  capture_output=True, shell=True)
    out = proc.stdout.decode("utf8")
    assert proc.returncode == 0, "return code isn't 0"
    return out

def clean(s):
    return re.sub(r"[\n\t\s\r]*", "", s)

def run_test(dir, filename, args=[]):
    out = clean(run_rascal(dir + "/" + filename, args))
    with open(dir + '/expected.txt') as f:
        expected = clean(f.read())
        assert out == expected, "unexpected output"

def print_output(name, color, res):
    max = 50 - len(name)
    padding = max * "." + " "
    print(padding + color + res + ENDC)
    sys.stdout.flush()


def test(test_name, test_dir, rascal_file, args=[]):
    print(test_name, end=" ")
    sys.stdout.flush()
    try:
        run_test(test_dir, rascal_file, args)
        print_output(test_name, GREEN, "OK")
        return True
    except Exception as e:
        print_output(test_name, RED, "KO")        
        print(repr(e))
        return False

def main():
    test_dir = os.path.dirname(__file__)
    os.chdir(test_dir)
    dirs = os.listdir()
    succes = True
    if "RASCAL_JAR" not in os.environ:
        print("RASCAL_JAR not set")
        succes = False
    else:
        global rascal_jar
        rascal_jar = os.environ["RASCAL_JAR"]

    if "ADA_AIR" not in os.environ:
        succes = False
        print("ADA_AIR not set")

    if succes:
        for test_dir in dirs:
            if os.path.isdir(test_dir) and test_dir.startswith("test_"):
                if test_dir == "test_parse":                
                    for lib in libs:
                        lib_path = lib.upper()+"_PATH" 
                        if lib_path in os.environ:
                            test_name = test_dir + " " + lib
                            for file in os.listdir(test_dir):
                                if os.path.isfile(test_dir +"/"+ file) and file.endswith(".rsc"):
                                    if not test(test_name, test_dir, file, [lib_path]):
                                        succes = False
                                    break
                        else:                    
                            print(f"skipping {lib}")

                else:
                    for file in os.listdir(test_dir):
                        if os.path.isfile(test_dir +"/"+ file) and file.endswith(".rsc"):
                            if not test(test_dir, test_dir, file):
                                succes = False
                            break
    if not succes:
        sys.exit(1)


if __name__ == "__main__":
    main()