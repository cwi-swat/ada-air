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

module call_graph

import lang::ada::AST;
import lang::ada::ImportAST;
import IO;
import util::SystemAPI;
import String;
import List;
import Set;
import util::FileSystem;
import Node;
import Exception;
import util::ShellExec;
import Content;

import analysis::m3::Core;

bool isCall(loc u) {
    return u.scheme == "ada+subprogramBody" || u.scheme == "ada+subprogramSpec" || u.scheme == "ada+subprogramInstantiation" || u.scheme == "ada+formalSubprogram";
}

Ada_Node Find_Decl (list[Entry_Point] Units, loc decl) {
    if (decl.scheme == "ada+subprogramSpec")
        decl = |ada+subprogramBody:///| + decl.path;
    for(Unit <- Units) {        
        for(/Ada_Node D <- Unit) {
            if (D@decl? && D.decl == decl)
            return D;
        }
    }
    return Null_Ada_Node;
}

str Package_Name (loc containment) {
    int idx1 = findFirst(containment.path, "(");
    int idx2 = findLast(containment.path[1..idx1], "/")+1;
    return replaceAll(containment.path[1..idx2], "/", ".");
}

str Subp_Name (Ada_Node Subp) {
    if(loc decl := Subp.decl)
        return decl.path;

    /*
    str package_name;
    if(loc containment := Subp.containment) {
        package_name = Package_Name (containment) +"." ;
    }

    if (Subp_Spec:subp_spec(_,_,_,_)  := Subp)
        return package_name+ head(Subp_Spec.F_Subp_Name).F_Name.content;
    else if (Subp_Body:subp_body(_,_,_,_,_,_)  := Subp)
         return package_name+ head(Subp_Body.F_Subp_Spec.F_Subp_Name).F_Name.content;
    else if (Subp_Decl:subp_decl(_,_,_)  := Subp)
         return package_name+ head(Subp_Decl.F_Subp_Spec.F_Subp_Name).F_Name.content;
    else if (Subp:expr_function(_,_,_,_)  := Subp)
        return package_name+ head(Subp.F_Subp_Spec.F_Subp_Name).F_Name.content;
    else if(generic_subp_instantiation(_, _, Name, _, _, _) := Subp)
        return package_name+ Name.F_Name.content;
    else if(concrete_formal_subp_decl(_, Spec, _, _) := Subp)
        return package_name+ head(Spec.F_Subp_Name).F_Name.content;
    throw "Subp name not found <Subp>";*/
}

Ada_Node Find_Subp_Body (list[Entry_Point] Units, loc package ,str name) {
    for(U <- Units) {
        for(/s:subp_body(_, Subp_Spec, _, _, Stmts, _) <- U) {
            Maybe[Ada_Node] N = Subp_Spec.F_Subp_Name;
            if (size(N) > 0) {
                try { // not handling dotted_name
                    str fun_name = head(N).F_Name.content;
                    if (fun_name == name && s.containment.path == package.path)
                        return s;
                }
                catch: {};
            }
        }
    }
    return Null_Ada_Node;
}

set[Ada_Node] Find_All_Calls (Ada_Node Subp_Body) {
    set[Ada_Node] l = {};
    if (subp_body(_, _, _, _, _, _) := Subp_Body) {
        visit (Subp_Body.F_Stmts) {
            case Expression exp: {
                if(exp@use? && loc u := exp@use && isCall (u))
                    l += exp;
            }
        }
        for(Ada_Node decl <- Subp_Body.F_Decls.F_Decls) {
            if(obj:object_decl(_, _, _, _, _, Maybe[Expression] Default_Expr, _, _) := decl) {
                if(size(Default_Expr) > 0) {
                    for(/Expression exp <- Default_Expr)
                        if(exp@use? && loc u := exp@use && isCall (u))
                            l += exp;
                }
            }
        }
    }
    return l;
}


void main(list[str] args=[]) {
    list[Entry_Point] Entry_Point_List = [];
    loc project = |file:///| + args[0];
    try {
        map[loc,Entry_Point] Units = importAdaProject (project);
        for(file <- Units)        
            Entry_Point_List += Units[file];
        
        M3 Model = Create_M3_Model (Units, project);

        Ada_Node Main_Subp = Find_Subp_Body (Entry_Point_List, |ada+subprogramBody:///|+args[1], args[2]);
        set[Ada_Node] next = {Main_Subp};
        set[str] output= {};
        while (!isEmpty(next)) {
            set[Ada_Node] tmp = {};
            for(Subp <- next) {
                set[Ada_Node] calls = Find_All_Calls (Subp);
                for(c <- calls) {
                    Ada_Node Subp_Called = Null_Ada_Node;
                    if(Expression e := c) 
                        Subp_Called = Find_Decl (Entry_Point_List, e.use);  
                    else
                        throw "Error not expression <c>";

                    if (Subp_Called != Null_Ada_Node) {
                        output += {"\"<Subp_Name (Subp)>\" -\> \"<Subp_Name (Subp_Called)>\""};                       
                        tmp += Subp_Called;
                    }                    
                    else if (loc u := c.use && u.scheme == "ada+subprogramSpec" && identifier(content) := c)
                        //output += {"\"<Subp_Name (Subp_Called)>\" -\> \"<Package_Name (u)>.<content>\""};  
                        output += {"\"<Subp_Name (Subp)>\" -\> \"<u.path>\""};         
                    else
                        throw "Error unhandled <c>";     
                }
            }
            next = tmp;
        }
        str content = "digraph G {\n";
        for (s <- output) content += s + "\n";        
        str AdaAir = getSystemEnvironment()["ADA_AIR"];
        writeFile (|file:///| + (AdaAir + "/example/calls/graph.gv"), content + "}");
        exec("dot", args=["-Tpdf", "graph.gv", "-o", "graph.pdf"]);
    }
    catch Java(class, msg): {
        println (class + " " + msg);
        println("failed to parse " + file.path);
    }
}
   
