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

module test_cyclomatic_complexity::cyclomatic_comp

import IO;
import lang::ada::AST;
import lang::ada::ImportAST;
import Set;
import List;
import Node;
import util::SystemAPI;


void compute_cyclomatic_complexity(Base_Formal_Param_Holder Subp_Spec, Statement Stmts) {
    int c = 1;
    visit(Stmts)
    {
        case for_loop_stmt(_,_,_):
            c += 1;
        case loop_stmt(_,_,_):
            c += 1;
        case while_loop_stmt(_,_,_):
            c += 1;
        case f: if_stmt(_,_, alt, els):
            c += 1 + size(alt) + size(els);
        case case_stmt(_,_):
            c += 1;
        case case_stmt_alternative(_,_):
            c += 1;
    }

    str fun_name = head(Subp_Spec.F_Subp_Name).F_Name.content;
    println("<fun_name> : <c>");
}



void main(list[str] args=[]) {
    loc ada_air = |file:///| + getSystemEnvironment()["ADA_AIR"];
    Compilation_Unit U = importAdaAST(ada_air + "/test/test_cyclomatic_complexity/test.ads", ada_air);
    for(/subp_body(_, Subp_Spec, _, _, Stmts, _) <- U) {
        compute_cyclomatic_complexity(Subp_Spec, Stmts);
    }
}

