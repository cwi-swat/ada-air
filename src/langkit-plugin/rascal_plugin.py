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

import os
from rascal_constructor import RascalConstructor
from rascal_data_types import RascalDataTypes
from rascal_context import RascalContext
import langkit.passes
from langkit.compile_context import CompileCtx
from mako.template import Template
from chained_constructor import Expr_Or_Assoc, Stmt_Or_Decl


class RascalPass(langkit.passes.AbstractPass):

    templates_dir = os.path.dirname(__file__) + "/templates/"
    
    inlined_prefix_nodes = {"bin_op": "",  # (Key : Inlined nodes, Value : Prefix to use e.g. add, u_add, rel_add, mem_add)
                            "un_op": "un_",
                            "relation_op": "relation_",
                            "membership_expr": "membership_"}

    def __init__(self, debug = False):
        super().__init__("rascal plugin pass")
        self.debug = debug
        RascalContext.chained_constructors.append(Expr_Or_Assoc)
        RascalContext.chained_constructors.append(Stmt_Or_Decl)


    def run(self, context: CompileCtx) -> None:
        if context.verbosity.info:
            print("Generate rascal sources...")
        self.emit_rascal_data_types(context)
        self.emit_exportation_function(context)
        return

    def emit_rascal_data_types(self, context: CompileCtx) -> None:        
        rascal_types = RascalDataTypes()
        for n in context.astnode_types:
            if n.is_root_node:
                # skipping Ada_Node, we will use rascal node
                continue
            elif n.is_list or n.is_root_list_type:
                # skipping List nodes, we will use rascal list
                continue
            elif n.base.is_bool_node:
                # skipping Present and Absent nodes, we will use their base node with "Maybe"
                continue
            elif n.abstract and not n.is_bool_node:
                continue
            elif (n.public_type.api_name.lower in RascalPass.inlined_prefix_nodes):
                lower_name = n.public_type.api_name.lower
                remaing_fields = []
                op_field = None
                fields = n.get_parse_fields(include_inherited=True, predicate=lambda f : not f.null)
                for field in fields:
                    assert field.type.is_ast_node
                    if field.api_name.lower != "f_op":
                        remaing_fields.append(field)
                    else:
                        op_field = field
                    
                for f in op_field.precise_types.minimal_matched_types:
                    rascal_name = "\\" + RascalPass.inlined_prefix_nodes[lower_name] + f.public_type.api_name.lower[3:]
                    constructor = RascalConstructor(rascal_name)
                    for rf in remaing_fields:
                        constructor.add_field(rf)
                    rascal_types.add_constructor(n, constructor)
                
            elif n.base.public_type.api_name.lower == "op":
                # inlining these nodes
                continue
            else:
                fields = n.get_parse_fields(include_inherited=True, predicate=lambda f : not f.null)
                constructor = RascalConstructor(n.public_type.api_name.lower)
                if n.is_token_node:
                    constructor.add_token_field()
                for field in fields:
                    assert field.type.is_ast_node
                    constructor.add_field(field)
                rascal_types.add_constructor(n, constructor)
            
            for p in n.get_inheritance_chain():
                if p.public_type.api_name.lower == "basic_decl":
                    RascalContext.m3_annotation["Decl"].append(n)
                    RascalContext.m3_annotation["Containment"].append(n)
                if p.public_type.api_name.lower == "name":
                    RascalContext.m3_annotation["Use"].append(n)


        rascal_types.rename_fields_redeclaration()
        output_dir = os.path.dirname(__file__) + "/../main/rascal/lang/ada/"
        with open(RascalPass.templates_dir + "rascal_ast.mako") as f:
            templateStr = f.read()
        tmp = Template(templateStr)
        with open(output_dir + 'AST.rsc', 'w') as f:
            f.write(tmp.render(types=rascal_types, RascalContext=RascalContext))

    def emit_exportation_function(self, context: CompileCtx):
        output_dir = os.path.dirname(__file__) + "/../main/ada/src/export/"
        if not os.path.isdir(output_dir):
            os.mkdir(output_dir)
        # Work-arround on windows to avoid blank lines 
        with open(RascalPass.templates_dir + "ada_main.mako") as f:
            templateStr = f.read()
        tmp = Template(templateStr)
        with open(output_dir + 'export-ast.adb', 'w') as f:
            f.write(tmp.render(ctx=context, inlined_prefix_nodes = RascalPass.inlined_prefix_nodes, RascalContext=RascalContext, debug=self.debug))


class DebugRascalPass(RascalPass):

    def __init__(self):
        super().__init__(True)

class DotPass(langkit.passes.AbstractPass):

    def __init__(self):
        super().__init__("dot plugin pass")

    def run(self, context: CompileCtx) -> None:
        DotPass.emit_dot_visualization(context)

    @staticmethod
    def emit_dot_visualization(context: CompileCtx) -> None:
        with open('lkt_dot.txt', 'w') as f:
            f.write("digraph D {\n")
            for n in context.entity_types:
                if n.base is not None:
                    child = n.api_name.lower
                    parent = n.base.api_name.lower
                    f.write("{0} -> {1}\n".format(parent, child))
            f.write("}\n")