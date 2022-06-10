from collections import OrderedDict

import langkit.passes
from langkit.compile_context import CompileCtx
from langkit.compiled_types import SymbolType, CompiledType, EntityType


def is_boolean_node(t: CompiledType) -> bool:
    derived_types = t.derivations
    return len(derived_types) == 2 and all(is_absent_or_present_node(d) for d in derived_types)


def is_absent_or_present_node(t: CompiledType) -> bool:
    return (t.api_name.lower.endswith("_absent")
            or t.api_name.lower.endswith("_present"))


class RascalConstructor:

    def __init__(self, type_name: str):
        self._name = type_name
        self._fields = OrderedDict({})  # {Key : field name, Value : field type name}

    def add_field(self, field: EntityType):
        self._fields[field.api_name.camel_with_underscores] = self.__get_rascal_field_type_name(field.type.entity)

    @staticmethod
    def __get_rascal_field_type_name(field_type: EntityType) -> str:
        field_type_name = None
        if field_type.element_type.is_list_type:
            element_contained = field_type.element_type.element_type.entity
            field_type_name = f"list[{RascalConstructor.__get_rascal_field_type_name(element_contained)}]"
        elif is_boolean_node(field_type.element_type):
            inheritance_chain = field_type.element_type.get_inheritance_chain()
            field_type_name = f"Maybe[{RascalDataTypes.get_associated_rascal_type(field_type)}]"
        else:
            field_type_name = RascalDataTypes.get_associated_rascal_type(field_type)
        return field_type_name

    def get_fields(self) -> OrderedDict[str, str]:
        return self._fields.copy()

    def apply_renaming_rules(self, rules: dict[str, tuple[str, str]]):
        for field_name, field_type in self._fields.copy().items():
            for field_to_change, rule in rules.items():
                # rule[0] type name
                # rule[1] new field name
                if field_name == field_to_change and rule[0] == field_type:
                    del self._fields[field_name]
                    self._fields[rule[1]] = field_type

    def __str__(self) -> str:
        f = []
        # insertion order since we are using an OrderedDict
        for k, v in self._fields.items():
            f.append(f"{v} {k}")
        return f"{self._name} ({', '.join(f) if len(f) > 0 else ''})"


class RascalDataTypes:

    def __init__(self):
        self._types = dict({})  # {Key : type name (camel case with underscore), Value : List of Constructors}

    def add_constructor(self, t: CompiledType, constructor: RascalConstructor):
        type_name = RascalDataTypes.get_associated_rascal_type(t)
        if type_name not in self._types:
            self._types[type_name] = []
        self._types[type_name].append(constructor)

    @staticmethod
    def get_associated_rascal_type(t: EntityType) -> str:
        field_type_name = None
        inheritance_chain = t.element_type.get_inheritance_chain()
        if t.is_root_type:
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
        elif any(node.api_name.lower.endswith("_node") and not "ada_node" in node.api_name.lower for node in
                 inheritance_chain):
            field_type_name = "Keyword"
        elif any(node.api_name.lower.endswith("_spec") for node in inheritance_chain):
            field_type_name = "Spec"
        else:
            field_type_name = inheritance_chain[1].public_type.api_name.camel_with_underscores
        return field_type_name

    @staticmethod
    def __compute_suffix(name: str) -> str:
        underscore = name.rfind("_")
        suffix = name
        list_type = "list["
        maybe_type = "maybe["

        if name.lower().startswith(list_type):
            suffix = name[len(list_type): len(name) - 1]
        elif name.lower().startswith(maybe_type):
            suffix = name[len(maybe_type): len(name) - 1]

        if underscore != -1:
            suffix = suffix[underscore + 1: len(suffix)]
        return suffix

    @staticmethod
    def __compute_renaming_rules(redeclarations: dict) -> dict[str, tuple[str, str]]:
        renaming_rules = {}  # {key: field name, value: (old type name, new field name)}
        for field_name, types in redeclarations.items():
            if len(types) == 2:
                is_type_name_in_field_name = any(t in field_name for t in types)
                is_ada_node = any("Ada_Node" in t for t in types)

                if is_ada_node and is_type_name_in_field_name:
                    old = [t for t in types if "Ada_Node" in t]
                    renaming_rules[field_name] = (old[0], field_name + "_Node")

                elif is_ada_node:
                    old = [t for t in types if "Ada_Node" not in t]
                    old_name = old[0]
                    suffix = RascalDataTypes.__compute_suffix(old_name)
                    renaming_rules[field_name] = (old_name, field_name + "_" + suffix)

                else:
                    len_min = 999
                    min_name = None
                    for t in types:
                        if len(t) < len_min:
                            len_min = len(t)
                            min_name = t
                    suffix = RascalDataTypes.__compute_suffix(min_name)
                    renaming_rules[field_name] = (min_name, field_name + "_" + suffix)

            else:
                # TODO handle more than 2 collisions
                raise RuntimeError('Not implemented')

        return renaming_rules

    def rename_fields_redeclaration(self):
        # Postprocessing the fields name to rename redeclared fields
        # More information at : https://tutor.rascal-mpl.org/Errors/Static/RedeclaredField/RedeclaredField.html
        for constructors in self._types.values():
            redeclarations = dict({})  # {Key : field name, values : set of types}
            for constructor in constructors:
                for field_name, field_type in constructor.get_fields().items():
                    if field_name not in redeclarations:
                        redeclarations[field_name] = {field_type}
                    else:
                        redeclarations[field_name].add(field_type)

            # removing declarations and keeping only redeclarations
            for key in redeclarations.copy().keys():
                if len(redeclarations[key]) == 1:
                    del redeclarations[key]

            if len(redeclarations) > 0:
                rules = RascalDataTypes.__compute_renaming_rules(redeclarations)
                for constructor in constructors:
                    constructor.apply_renaming_rules(rules)

    def __str__(self):
        s = []
        for type_name, constructors in self._types.items():
            s.append(f"data {type_name}(loc src=|unknown:///|) = ")
            s.append('\n| '.join(str(c) for c in constructors) + ";\n\n")
        return ''.join(s)


class PluginPass(langkit.passes.AbstractPass):

    def __init__(self):
        super().__init__("rascal plugin pass")

    def run(self, context: CompileCtx) -> None:
        # self.emit_dot_visualization(context)
        self.emit_rascal_data_types(context)
        return

    @staticmethod
    def emit_dot_visualization(context: CompileCtx) -> None:
        print("digraph D {")
        for n in context.entity_types:
            if n.base is not None:
                child = n.api_name
                parent = n.base.api_name
                print("\"{0}\" -> \"{1}\"".format(parent, child))
        print("}")

    @staticmethod
    def emit_rascal_data_types(context: CompileCtx) -> None:
        rascal_types = RascalDataTypes()
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
            elif n.astnode.abstract and not is_boolean_node(n.element_type):
                continue

            fields = n.element_type.get_parse_fields(include_inherited=True)
            constructor = RascalConstructor(n.api_name.lower)
            for field in fields:
                assert field.type.is_ast_node
                constructor.add_field(field)
            rascal_types.add_constructor(n, constructor)

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
        rascal_types.rename_fields_redeclaration()
        print(rascal_types)
