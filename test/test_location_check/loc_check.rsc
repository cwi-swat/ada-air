@license{
Copyright (c) 2022, TNO (ESI) and NWO-I Centrum Wiskunde & Informatica (CWI)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}
@author{Jurgen J. Vinju - Centrum Wiskunde & Informatica}
@author{Damien De Campos - TNO ESI}
@author{Pierre van de Laar - TNO ESI}

module test_location_check::loc_check

import IO;
import lang::ada::AST;
import lang::ada::ImportAST;
import Set;
import List;
import Node;
import util::SystemAPI;

bool allNodesHaveASource(node haystack) = (true | it && needle.src? | /node needle <- haystack);

bool allNodesAreOrdered(node haystack) {
    for(/node needle <- haystack) {
        if([*_,node a1, *_ ,node a2, * _] := getChildren(needle) && a2.src < a1.src)
            return false;
        else if ([*_,list[node] a1, *_ ,node a2, * _] := getChildren(needle) && !isEmpty(a1) && a2.src < last(a1).src)
            return false;
        else if ([*_,node a1, *_ ,list[node] a2, * _] := getChildren(needle) && !isEmpty(a2) && head(a2).src < a1.src)
            return false;
        else if ([*_,list[node] a1, *_ ,list[node] a2, * _] := getChildren(needle) && !isEmpty(a1) && ! isEmpty(a2) && head(a2).src < last(a1).src)
            return false;
        //TODO handle optional fields
    }
    return true;
}

void main(list[str] args=[]) {
    loc ada_air = |file:///| + getSystemEnvironment()["ADA_AIR"];
    Compilation_Unit U = importAdaAST(ada_air + "/test/test_location_check/test.adb", ada_air);
    println(allNodesHaveASource(U));
    println(allNodesAreOrdered(U));
}