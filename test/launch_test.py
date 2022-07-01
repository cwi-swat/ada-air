#! /usr/bin/env python
import subprocess
import os
import sys
import re
import platform
import threading


RED = '\033[31m'
ENDC = '\033[m'
GREEN = '\033[32m'

libs = ["libadalang", "xmlada", "gnatcoll", "langkit_support", "aws"]
rascal_jar = None
print_lock = threading.Lock()
threads = []

if os.name == 'nt' and platform.release() == '10' and platform.version() >= '10.0.14393':
    # Fix ANSI color in Windows 10 version 10.0.14393 (Windows Anniversary Update)
    import ctypes
    kernel32 = ctypes.windll.kernel32
    kernel32.SetConsoleMode(kernel32.GetStdHandle(-11), 7)


class RascalError(Exception):

    def __init__(self, message : str) -> None:
        self.message = message
    
    def __str__(self) -> str:
        title = " Rascal error "
        return 10*"-" + title + 10*"-"+"\n"+self.message+"\n"+(len(title)+20)*"-"
    
class UnexpectedOutput(Exception):
    def __init__(self, message : str) -> None:
        self.message = message
    
    def __str__(self) -> str:
        title = " Unexpected output "
        return 10*"-" + title + 10*"-"+"\n"+self.message+"\n"+(len(title)+20)*"-"


def run_rascal(filename, args):
    proc = subprocess.run(['java', '-Xmx1G', '-Xss32m', '-jar', rascal_jar, filename, ('--args' if len(args)>0 else '')] + args,  capture_output=True, shell=True)
    out = proc.stdout.decode("utf8")
    err = proc.stderr.decode("utf8")
    if proc.returncode != 0:
        raise RascalError(err)
    return out

def clean(s):
    return re.sub(r"[\n\t\s\r]*", "", s)

def run_test(dir, filename, args=[]):
    out = run_rascal(dir + "/" + filename, args + [str(threading.get_native_id())])
    with open(dir + '/expected.txt') as f:
        expected = clean(f.read())
        if clean(out) != expected:
            raise UnexpectedOutput(out)

def print_output(name, color, res):
    max = 50 - len(name)
    padding = max * "." + " "
    with print_lock:
        print(name + " " + padding + color + res + ENDC + "\n")
        sys.stdout.flush()


def test(test_name, test_dir, rascal_file, args=[]):
    try:
        run_test(test_dir, rascal_file, args)
        print_output(test_name, GREEN, "OK")
        return True
    except (UnexpectedOutput, RascalError) as e:
        with print_lock:   
            print(str(e))
        print_output(test_name, RED, "KO")
        return False

def main():
    failledTest = 0
    successfulTest = 0
    skippedTest = 0
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
                                    t = threading.Thread(target=test, args=(test_name, test_dir, file, [lib_path]))
                                    threads.append(t)
                                    if not t.start():                                   
                                        failledTest += 1
                                    else:
                                        successfulTest += 1
                                    break
                        else:                    
                            print(f"skipping {lib}")
                            skippedTest += 1

                else:
                    for file in os.listdir(test_dir):
                        if os.path.isfile(test_dir +"/"+ file) and file.endswith(".rsc"):
                            if not test(test_dir, test_dir, file):
                                failledTest += 1
                            else:
                                successfulTest += 1
                            break
    for t in threads:
        t.join()
    total = successfulTest + failledTest
    print(10*"-" + " Result " + 10*"-")
    print(f"{successfulTest}/{total}")
    print(f"{skippedTest} tests skipped")
    if failledTest != 0:
        sys.exit(1)


if __name__ == "__main__":
    main()