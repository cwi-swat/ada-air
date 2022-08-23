@license{
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
import analysis::m3::Core;
import IO;

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
Import an Ada Project using Libadalang.
Make sure that the environment variable TMP is set.
}
map[loc,Entry_Point] importAdaProject(loc file) {
    str out_file = getSystemEnvironment()["TMP"] + "/out" + toString(uuidi()) + ".txt";
    loc out = |file:///| + out_file;
    importAdaProject(file, out);
    try {
        return readTextValueFile(#map[loc,Entry_Point], out);
    }
    catch IO(msg): {
        str m = msg + " while parsing " + out.path;
        throw IO(m);
    }
}



@doc{
.Synopsis
Creare M3 model
}
M3 Create_M3_Model (Entry_Point E, loc file) {
    M3 result = m3(file);
    visit(E) {
        case Ada_Node n: {
            if (n@decl? && n@src?) {
                if (loc decl := n@decl && loc src := n@src) {
                    result.declarations  += {<decl,src>};
                }      
                else if (list[loc] decls := n@decl && loc src := n@src) {
                    for (loc decl <- decls)
                        result.declarations  += {<decl,src>};
                }
            }            
            
            if (n@use? && n@src?) {
                if (loc use := n@use && loc src := n@src) {
                    result.uses += {<src,use>};
                }      
            }

            if (n@decl? && n@containment?) {
                if (loc decl := n@decl && loc containment := n@containment) {
                    result.containment += {<containment,decl>};
                }      
                else if (list[loc] decls := n@decl && loc containment := n@containment) {
                    for (loc decl <- decls)
                        result.containment += {<containment,decl>};
                }
            }     
        }
    }
    return result;
}

@doc{
.Synopsis
Creare M3 model
}
M3 Create_M3_Model (map[loc,Entry_Point] E, loc project) {
    set[M3] Models = {};
    for(file <- E) {
        Models += Create_M3_Model (E[file], file);
    }
    return composeM3(project, Models);
}

@doc{
.Synopsis
Parse an Ada file with Libadalang and print the rascal ast into a file.
}
@javaClass{lang.ada.ImportAst}
public java void importAdaAst(loc adaFile, loc out);

@doc{
.Synopsis
Parse an Ada project with Libadalang and print the rascal ast into a file.
}
@javaClass{lang.ada.ImportAst}
public java void importAdaProject(loc adaProject, loc out);
