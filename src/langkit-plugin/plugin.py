import langkit.passes
from langkit.compile_context import CompileCtx
from langkit.compiled_types import SymbolType, CompiledType, EntityType


def is_boolean_node(t: CompiledType):
    derived_type = t.derivations
    return len(derived_type) == 2 and all(is_absent_or_present_node(d) for d in derived_type)


def is_absent_or_present_node(t: CompiledType):
    return (t.api_name.lower.lower().endswith("_absent")
    or t.api_name.lower.lower().endswith("_present"))


class PluginPass(langkit.passes.AbstractPass):

    def __init__(self):
        super().__init__("Plugin Pass!!!")

    def run(self, context: CompileCtx) -> None:
        # self.emit_dot_visualization(context)
        self.emit_rascal_data_types(context)
        return

    @staticmethod
    def emit_dot_visualization(context: CompileCtx) -> None:
        ref = PluginPass.compute_nb_references(context)
        print("digraph D {")
        for n in context.entity_types:
            if n.base is not None:
                child = n.api_name if n.api_name.lower.lower() not in ref else f"{n.api_name} {ref[n.api_name.lower.lower()]}"
                parent = n.base.api_name if n.base.api_name.lower.lower() not in ref else f"{n.base.api_name} {ref[n.base.api_name.lower.lower()]}"

                print("\"{0}\" -> \"{1}\"".format(parent, child))
        print("}")

    @staticmethod
    def get_associated_rascal_type(t: EntityType) -> str:
        field_type_name = None
        inheritance_chain = t.element_type.get_inheritance_chain()
        if t.is_root_type:
            field_type_name = None
        elif any(node.api_name.lower.endswith("_decl") for node in inheritance_chain):
            field_type_name = "Decl"
        elif any(node.api_name.lower.endswith("_def") for node in inheritance_chain):
            field_type_name = "Def"
        elif any("_expr" in node.api_name.lower for node in inheritance_chain):
            field_type_name = "Expr"
        elif any("stmt" in node.api_name.lower for node in inheritance_chain):
            field_type_name = "Stmt"
        elif any("assoc" in node.api_name.lower for node in inheritance_chain):
            field_type_name = "Assoc"
        elif any(node.api_name.lower.endswith("_node") and not "ada_node" in node.api_name.lower for node in
                 inheritance_chain):
            field_type_name = "Keyword"
        elif any(node.api_name.lower.endswith("_spec") for node in inheritance_chain):
            field_type_name = "Spec"
        else:
            field_type_name = inheritance_chain[1].public_type.api_name.camel_with_underscores
        return field_type_name

    @staticmethod
    def get_rascal_field_type_name(field_type: EntityType) -> str:
        field_type_name = None
        inheritance_chain = field_type.element_type.get_inheritance_chain()
        if field_type.is_root_type:
            field_type_name = "Ada_Node"
        elif any(node.api_name.lower.endswith("_decl") for node in inheritance_chain):
            field_type_name = "Decl"
        elif any(node.api_name.lower.endswith("_def") for node in inheritance_chain):
            field_type_name = "Def"
        elif any("_expr" in node.api_name.lower for node in inheritance_chain):
            field_type_name = "Expr"
        elif any("stmt" in node.api_name.lower for node in inheritance_chain):
            field_type_name = "Stmt"
        elif any("assoc" in node.api_name.lower for node in inheritance_chain):
            field_type_name = "Assoc"
        elif any(node.api_name.lower.endswith("_node") and not "ada_node" in node.api_name.lower for node in inheritance_chain):
            field_type_name = "Keyword"
        elif any(node.api_name.lower.endswith("_spec") for node in inheritance_chain):
            field_type_name = "Spec"
        elif field_type.element_type.is_list_type:
            element_contained = field_type.element_type.element_type.entity
            field_type_name = f"list[{PluginPass.get_rascal_field_type_name(element_contained)}]"
        elif is_boolean_node(field_type.element_type):
            field_type_name = f"Maybe[{inheritance_chain[1].public_type.api_name.camel_with_underscores}]"
        else:
            field_type_name = inheritance_chain[1].public_type.api_name.camel_with_underscores
        return field_type_name

    @staticmethod
    def emit_rascal_data_types(context: CompileCtx) -> None:
        rascal_types = {}
        for n in context.entity_types:
            if n.is_root_type:
                # skipping Ada_Node, we will use rascal node
                continue
            elif n.element_type.is_list_type or n.api_name.lower == "ada_list":
                # skipping List nodes, we will use rascal list
                continue
            elif is_absent_or_present_node(n.element_type):
                # skipping Present and Absent nodes, we will use their base node with "Maybe"
                continue
            else:
                rascal_name = PluginPass.get_associated_rascal_type(n)
                if rascal_name not in rascal_types:
                    if n.astnode.abstract and not is_boolean_node(n.element_type):
                        rascal_types[rascal_name] = [f"data {rascal_name}(loc src=|unknown:///|) ="]
                        continue
                    else:
                        rascal_types[rascal_name] = [f"data {rascal_name}(loc src=|unknown:///|) = {n.api_name.lower} ("]

                else:
                    if n.astnode.abstract:
                        continue
                    # TODO find a better way to check if there is at least one constructor
                    elif not rascal_types[rascal_name][-1].endswith("="):
                        rascal_types[rascal_name].append("\n|")
                    rascal_types[rascal_name].append(f" {n.api_name.lower} (")

                fields = n.element_type.get_parse_fields(include_inherited=True)
                i = 0
                for field in fields:
                    assert field.type.is_ast_node
                    field_type = field.type.entity
                    field_type_name = PluginPass.get_rascal_field_type_name (field_type)
                    rascal_types[rascal_name].append(f"{field_type_name} {field.api_name}")
                    if i < len(fields) - 1:
                        rascal_types[rascal_name].append(", ")
                    i = i + 1
                rascal_types[rascal_name].append(")")

        # TODO use mako templates
        print("""@license{
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
module lang::ada::AST

import IO;
import List;
import util::Maybe;
alias Ada_Node = node;\n\n""")

        for value in rascal_types.values():
            print(''.join(value) + ";\n")