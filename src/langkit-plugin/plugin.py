import langkit.passes
from langkit.compile_context import CompileCtx
from langkit.compiled_types import SymbolType, CompiledType


def is_boolean_type(t: CompiledType):
    derived_type = t.derivations
    return ((len(derived_type) == 2 and all(d.api_name.lower.lower().endswith("_absent") or
                                          d.api_name.lower.lower().endswith("_present")
                                          for d in derived_type))
    or (t.api_name.lower.lower().endswith("_absent")
    or t.api_name.lower.lower().endswith("_present")))


class PluginPass(langkit.passes.AbstractPass):

    def __init__(self):
        super().__init__("Plugin Pass!!!")

    def run(self, context: CompileCtx) -> None:
        # self.emit_dot_visualization(context)
        self.emit_rascal_data_types(context)
        # self.print_leaf_types(context)
        # self.emit_only_one_child_data_types(context)
        return

    @staticmethod
    def emit_dot_visualization(context: CompileCtx) -> None:
        print("digraph D {")
        for n in context.entity_types:
            if n.base is not None:
                print(f"{n.base.api_name} -> {n.api_name}")
        print("}")

    @staticmethod
    def emit_rascal_data_types(context: CompileCtx) -> None:
        rascal_types = {}
        for n in context.entity_types:
            if n.is_root_type:
                # skipping Ada_Node root type, we will use rascal node
                continue
            elif n.element_type.is_list_type or n.api_name.lower.lower() == "ada_list":
                # skipping list types, we will use rascal list
                continue
            elif is_boolean_type(n.element_type):
                # skipping present and absent, we will use boolean
                continue
            else:
                base = n.base.api_name
                if base in rascal_types:
                    # Adding constructor in parent type
                    rascal_types[base].append(f"\n| {n.api_name.lower}({n.api_name} {n.api_name.lower})")

                    rascal_types[n.api_name] = [f"data {n.api_name}(loc src=|unknown:///|) = {n.api_name.lower}("]
                elif n.base.is_root_type:
                    rascal_types[n.api_name] = [f"data {n.api_name}(loc src=|unknown:///|) = {n.api_name.lower}("]
                else:
                    print(f"{base} not in rascal_types when processing {n.api_name}")
                    continue

                fields = n.element_type.get_parse_fields(include_inherited=True)
                i = 0
                for field in fields:
                    assert field.type.is_ast_node
                    field_type = field.type.entity
                    field_type_name = field.type.entity.api_name
                    if field_type.is_root_type:
                        field_type_name = "node"
                    elif field_type.element_type.is_list_type:
                        element_contained = field_type.element_type.element_type.entity
                        element_contained_type = "node" if element_contained.is_root_type else element_contained.api_name
                        field_type_name = f"list[{element_contained_type}]"
                    elif is_boolean_type(field_type.element_type):
                        field_type_name = "bool"

                    rascal_types[n.api_name].append(f"{field_type_name} {field.api_name}")
                    if i < len(fields) - 1:
                        rascal_types[n.api_name].append(", ")
                    i = i + 1
                rascal_types[n.api_name].append(")")

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
import List;\n\n""")
        for value in rascal_types.values():
            print(''.join(value) + ";\n")

    @staticmethod
    def emit_leaf_types(context: CompileCtx) -> None:
        print("subtype Ada_Leaf_Node is LALCO.Ada_Node_Kind_Type \nwith Static_Predicate => Ada_Leaf_Node in ")
        for cls in context.astnode_types:
            if not cls.abstract and len(cls.derivations) == 0:
                print(f"LALCO.{cls.ada_kind_name} ")
                print("| ", end="")

    @staticmethod
    def emit_only_one_child_data_types(context: CompileCtx) -> None:
        for n in context.entity_types:
            if len(n.element_type.get_parse_fields(include_inherited=True)) == 1:
                print(f"{n.api_name}")
