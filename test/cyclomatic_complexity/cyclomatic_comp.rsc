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

module cyclomatic_comp

import IO;
import lang::ada::AST;
import lang::ada::ImportAST;
import util::Maybe;
import Set;
import Node;
import List;


                            // why I can't use `Decl fun` here ?
void compute_cyclomatic_complexity(Ada_Node fun) {
    int c = 1;
    visit(fun)
    {
        // why `case for_loop_stmt(_*):` doesn't work ?
        case for_loop_stmt(_,_,_):
            c += 1;
        case loop_stmt(_,_,_):
            c += 1;
        case while_loop_stmt(_,_,_):
            c += 1;
        case f: if_stmt(_,_, alt, els):
        {
            c += 1 + size(alt);
            switch(els) {
                case just(_):
                    c += 1;
            }  
        }
        case case_stmt(_,_):
            c += 1;
        case case_stmt_alternative(_,_):
            c += 1;
    }

    fun_name = "";
    // Is there a better way to access nodes fields? By their names maybe?
    if (Ada_Node spec := fun[1]) // Why I can't use `Spec spec` here ?
    {
        if(Ada_Node name := spec[1])
        {
            for(/Ada_Node n <- name) {
                switch(n){
                    case identifier(content):
                        fun_name = content;
                }
            }
        }
    }

    println("<fun_name> : <c>");
}



void main() {
    loc ada_air = |file:///Users/camposdd/Documents/ada-air|;
    Compilation_Unit U = importAdaAST(ada_air + "/test/cyclomatic_complexity/test.ads", ada_air);

    // I wanted to write `for(/subp_body n <- U)`
    // because you did somtehing similar in your papers/presentations but that doesn't work
    for(/Ada_Node n <- U) {
        switch(n) {
            case fun: subp_body(_,_,_,_,_,_):
                compute_cyclomatic_complexity(fun);
        }
    }
}