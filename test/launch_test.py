#! /usr/bin/env python

"""
Copyright (c) 2022, TNO (ESI) and NWO-I Centrum Wiskunde & Informatica (CWI)
All rights reserved.

Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.

Authors:
Jurgen J. Vinju - Centrum Wiskunde & Informatica
Damien De Campos - TNO ESI
Pierre van de Laar - TNO ESI
"""

from multiprocessing.connection import wait
import subprocess
import os
import sys
import re
import platform
import threading
import time


RED = '\033[31m'
ENDC = '\033[m'
GREEN = '\033[32m'

libs = ["libadalang", "xmlada", "gnatcoll", "langkit_support", "aws", "ada_drivers_library", "Ada_Runtime", "Dependency_Graph_Extractor_Ada"]
# new lib : gnatstudio, gtkada, cudada, AdaDoom3, tamp, sdlada, ada-crypto-library, HAC
print_lock = threading.Lock()
queue_lock = threading.Lock()
tests = []

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


class TestQueue:

    def __init__(self, size) -> None:
        self.size = size
        self.all_tests= []
        self.running_tests = []
        self.waiting_tests = []

    def add(self, test):
        with queue_lock:
            self.all_tests.append(test)
            if len(self.running_tests) < self.size:
                self.running_tests.append (test)
                test.start()
            else:
                self.waiting_tests.append (test)

    def remove(self, test):
        with queue_lock:
            assert test in self.running_tests
            self.running_tests.remove(test)
            if len(self.waiting_tests) > 0:
                t = self.waiting_tests.pop(0)
                self.running_tests.append(t)
                t.start()

    def get_all_tests(self):
        return self.all_tests

    def wait(self):
        while len(self.waiting_tests) > 0 or len(self.running_tests) > 0:
            for t in self.running_tests:
                t.join()

def clean(s):
    return re.sub(r"[\n\t\s\r]*", "", s)


class Test:

    rascal_jar = None

    def __init__(self, test_name, test_dir, threadQueue, rascal_file, args) -> None:
        self.test_name = test_name
        self.test_dir = test_dir
        self.success = None
        self.th = threading.Thread(target=self.run, args=(rascal_file, args))
        self.threadQueue = threadQueue
        pass


    def __run_rascal(self, rascal_file, args):
        proc = subprocess.run(['java', '-Djava.library.path=../src/main/ada/lib/','-Xmx1G', '-Xss32m', '-jar', Test.rascal_jar, rascal_file, ('--args' if len(args)>0 else '')] + args,  capture_output=True, shell=False)
        out = proc.stdout.decode("utf8")
        err = proc.stderr.decode("utf8")
        if proc.returncode != 0:
            raise RascalError(out + "\n" + err)
        return out


    def __check_output(self, output):
        with open(self.test_dir + '/expected.txt') as f:
            expected = clean(f.read())
            if clean(output) != expected:
                raise UnexpectedOutput(output)

    def __print_output(self, color, res):
        max = 50 - len(self.test_name)
        padding = max * "." + " "
        with print_lock:
            print(self.test_name + " " + padding + color + res + ENDC + "\n")
            sys.stdout.flush()

    def run(self, rascal_file, args):
        try:
            out = self.__run_rascal(self.test_dir + "/" + rascal_file, args)
            self.__check_output(out)
            self.__print_output(GREEN, "OK")
            self.success = True
            self.threadQueue.remove(self)
        except (UnexpectedOutput, RascalError) as e:
            with print_lock:   
                print(str(e))
            self.__print_output(RED, "KO")
            self.success = False
            self.threadQueue.remove(self)
    
    def start(self):
        self.th.start()
    
    def join(self):
        self.th.join()

    def getResult(self):
        assert self.success is not None
        return self.success

def main(jobs):
    st = time.time()
    failedTest = 0
    successfulTest = 0
    skippedTest = 0
    test_dir = os.path.dirname(__file__)
    os.chdir(test_dir)
    dirs = os.listdir()
    setup = True
    if "RASCAL_JAR" not in os.environ:
        print("RASCAL_JAR not set")
        setup = False
    else:
        Test.rascal_jar = os.environ["RASCAL_JAR"]

    if "ADA_AIR" not in os.environ:
        setup = False
        print("ADA_AIR not set")

    threadQueue = TestQueue(jobs)

    if setup:
        for test_dir in dirs:
            if os.path.isdir(test_dir) and test_dir.startswith("test_"):
                if test_dir == "test_parse":                
                    for lib in libs:
                        lib_path = lib.upper()+"_PATH" 
                        if lib_path in os.environ:

                            test_name = test_dir + " " + lib
                            for file in os.listdir(test_dir):
                                if os.path.isfile(test_dir +"/"+ file) and file.endswith(".rsc"):                                
                                    test = Test(test_name, test_dir, threadQueue, file, [lib_path])
                                    threadQueue.add(test)
                        else:                    
                            print(f"skipping {lib}")
                            skippedTest += 1

                else:
                    for file in os.listdir(test_dir):
                        if os.path.isfile(test_dir +"/"+ file) and file.endswith(".rsc"):
                            test = Test(test_dir, test_dir, threadQueue, file, [])
                            threadQueue.add(test)

        threadQueue.wait()
        for t in threadQueue.get_all_tests():
            if t.getResult():
                successfulTest += 1
            else:
                failedTest += 1

        total = successfulTest + failedTest
        print(10*"-" + " Result " + 10*"-")
        print(f"{successfulTest}/{total}")
        print(f"{skippedTest} tests skipped")
        end = time.time()
        print(time.strftime("%Hh%Mm%Ss", time.gmtime(end-st)))

    if failedTest != 0 or not setup:
        sys.exit(1)


if __name__ == "__main__":
    jobs = 1
    if len(sys.argv) == 2:
        try:
            jobs = int(sys.argv[1])
        except ValueError:
            print("warning number of jobs must be an integer, default value used: 1")
            sys.stdout.flush()
    main(jobs)
