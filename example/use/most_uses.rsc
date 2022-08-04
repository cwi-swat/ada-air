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

module most_uses

import IO;
import lang::ada::AST;
import lang::ada::ImportAST;
import analysis::m3::Core;
import ListRelation;
import Relation;
import List;
import Node;
import Set;
import Map;
import util::SystemAPI;

bool isSubp(loc u) {
    return u.scheme == "ada+subprogramBody" || u.scheme == "ada+subprogramSpec" || u.scheme == "ada+subprogramInstantiation" || u.scheme == "ada+formalSubprogram";
}

void main(list[str] args=[]) {
    list[Entry_Point] Entry_Point_List = [];
    loc project = |file:///| + args[0];
    map[loc,Entry_Point] Units = importAdaProject (project);
    M3 m3 = Create_M3_Model (Units, project);

    map[loc src, set[loc] names] decls_assoc = index(invert(m3.declarations));
    map[loc declarations, int uses] decls = ();
    rel[loc,loc] uses = m3.uses o m3.declarations;
    for (i <- uses) {
        loc name = getFirstFrom(decls_assoc[i[1]]); // TODO remove duplicate in M3 declarations
        if (isSubp(name))
            if (name in decls) 
                decls[name] += 1;
            else
                decls[name] = 1;
    }
    lrel[loc,int] D = sort(toList(decls), bool (<loc _, int a>, <loc _, int b>) { return a > b; });
    int limit = 30;
    int c = 1;
    for (i <- D) {
        if (c == limit) break;
        println("<i[0]> : <i[1]>");
        c+=1;
    }
}

