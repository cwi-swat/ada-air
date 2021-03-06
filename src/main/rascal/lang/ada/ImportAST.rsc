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

module lang::ada::ImportAST

import lang::ada::AST;
import ValueIO;
import util::UUID;
import util::Math;
import util::SystemAPI;
import Exception;

@doc{
.Synopsis
Import an Ada ast using Libadalang.
Make sure that the environment variable TMP is set.
}
Entry_Point importAdaAST(loc file) {
    str out_file = getSystemEnvironment()["TMP"] + "/out" + toString(uuidi()) + ".txt";
    loc out = |file:///| + out_file;
    importAdaAst(file, out);
    try {
        return readTextValueFile(#Entry_Point, out);
    }
    catch IO(msg): {
        str m = msg + " while parsing " + out.path;
        throw IO(m);
    }
}

@doc{
.Synopsis
Parse an Ada file with Libadalang and print the rascal ast into a file.
}
@javaClass{lang.ada.ImportAst}
public java void importAdaAst(loc ada, loc out);
