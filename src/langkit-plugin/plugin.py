from collections import OrderedDict

import langkit.passes
from langkit.compile_context import CompileCtx
from langkit.compiled_types import CompiledType, EntityType, Field, ASTNodeType


class RascalConstructor:

    def __init__(self, type_name: str):
        self._name = type_name
        self._fields = OrderedDict({})  # {Key : field name, Value : field type name}

    def add_field(self, field: Field):
        self._fields[field.api_name.camel_with_underscores] = self.__get_rascal_field_type_name(field)

    def add_token_field(self):
        self._fields["content"] = "str"

    @staticmethod
    def __get_rascal_field_type_name(field: Field) -> str:
        field_type = field.type.entity.astnode
        field_type_name = None
        if field_type.is_list:
            element_contained = field_type.element_type
            field_type_name = f"list[{RascalDataTypes.get_associated_rascal_type(element_contained)}]"
        elif field_type.is_bool_node:
            inheritance_chain = field_type.get_inheritance_chain()
            field_type_name = f"Maybe[{RascalDataTypes.get_associated_rascal_type(field_type)}]"
        else:
            field_type_name = RascalDataTypes.get_associated_rascal_type(field_type)

        if field.is_optional and not field_type.is_bool_node:
            field_type_name = "Maybe[" + field_type_name + "]" # optional fields can be null

        return field_type_name

    def get_fields(self) -> OrderedDict[str, str]:
        return self._fields.copy()

    def apply_renaming_rules(self, renaming_rules: dict[str, [tuple[str, str]]]):
        for field_name, field_type in self._fields.copy().items():
            for field_to_change, rules in renaming_rules.items():
                for rule in rules:
                    # rule[0] type name
                    # rule[1] new field name
                    if field_name == field_to_change and rule[0] == field_type:
                        del self._fields[field_name]
                        self._fields[rule[1]] = field_type
                        break

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
    def get_associated_rascal_type(t: ASTNodeType) -> str:
        field_type_name = None
        inheritance_chain = t.get_inheritance_chain()
        if t.is_root_node:
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
        last_open_bracket = name.rfind("[")
        first_close_bracket = name.find("]")

        if last_open_bracket != -1 and first_close_bracket != -1:
            suffix = name[last_open_bracket + 1: first_close_bracket]

        if underscore != -1:
            suffix = suffix[underscore + 1: len(suffix)]
        return suffix

    @staticmethod
    def __compute_renaming_rules(redeclarations: dict) -> dict[str, [tuple[str, str]]]:
        renaming_rules = {}  # {key: field name, value: list of (old type name, new field name)}
        for field_name, types in redeclarations.items():
            # TODO remove this condition?
            if len(types) == 2:
                is_type_name_in_field_name = any(t in field_name for t in types)
                is_ada_node = any("Ada_Node" in t for t in types)

                if is_ada_node and is_type_name_in_field_name:
                    old = [t for t in types if "Ada_Node" in t]
                    renaming_rules[field_name] = [(old[0], field_name + "_Node")]

                elif is_ada_node:
                    old = [t for t in types if "Ada_Node" not in t]
                    old_name = old[0]
                    suffix = RascalDataTypes.__compute_suffix(old_name)
                    renaming_rules[field_name] = [(old_name, field_name + "_" + suffix)]

                else:
                    len_min = 999
                    min_name = None
                    for t in types:
                        if len(t) < len_min:
                            len_min = len(t)
                            min_name = t
                    suffix = RascalDataTypes.__compute_suffix(min_name)
                    renaming_rules[field_name] = [(min_name, field_name + "_" + suffix)]

            else:
                priority = dict({}) # {key: type name, value: priority (int 1-5)}
                # 1 (Low priority) : X F_X. e.g. Stmt F_Stmt
                # 2 : Ada_Node
                # 3 : others
                # 4 : Short name without underscore
                # 5 : Optional field e.g. Maybe[..]
                for t in types:
                    if t.startswith("Maybe["):
                        priority[t] = 5
                    elif t in field_name:
                        priority[t] = 1
                    elif t.find("_") == -1:
                        priority[t] = 4
                    elif "Ada_Node" in t:
                        priority[t] = 2
                    else:
                        priority[t] = 3
                priority_list = sorted(priority.items(), key=lambda x: x[1], reverse=True)
                sorted_priority = {k: v for k, v in priority_list}
                min_nb_renaming_needed = len(types) - 1
                nb_renaming = 0
                renaming_rules[field_name] = []
                for type_name, prio in sorted_priority.items():
                    if prio == 5:
                        renaming_rules[field_name].append((type_name, field_name + "_Maybe"))
                    elif prio == 2:
                        renaming_rules[field_name].append((type_name, field_name + "_Node"))
                    else:
                        renaming_rules[field_name].append((type_name, field_name + "_" + RascalDataTypes.__compute_suffix(type_name)))
                    nb_renaming = nb_renaming + 1
                    if nb_renaming == min_nb_renaming_needed:
                        break

        return renaming_rules

    def rename_fields_redeclaration(self):
        # Postprocessing the fields name to rename redeclared fields
        # More information at : https://tutor.rascal-mpl.org/Errors/Static/RedeclaredField/RedeclaredField.html
        for constructors in self._types.values():
            redeclarations = dict({})  # {Key : field name, values : set of types name}
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
        self.emit_exportation_function(context)
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

            fields = n.get_parse_fields(include_inherited=True)
            constructor = RascalConstructor(n.public_type.api_name.lower)
            if n.is_token_node:
                constructor.add_token_field()
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

    @staticmethod
    def emit_exportation_function(context: CompileCtx):
        # TODO use a mako template
        print("function Export_AST_To_Rascal (N : LAL.Ada_Node'Class; Indent : Natural := 0; Pretty_Print : Boolean := True; IsOptional : Boolean := False) return String is")
        print("use Ada.Strings.Fixed;")
        print("use Ada.Characters.Latin_1;")
        print("Tab : constant String := (if Pretty_Print then \"|  \" else \"\");")
        print("Prefix : constant String := (if Pretty_Print then LF & (Indent * Tab) else \"\");")
        print("Just : constant String := (if IsOptional then \"just(\" else \"\");")
        print("End_Just : constant String := (if IsOptional then \")\" else \"\");")
        print("begin")
        print("if N.Is_Null then")
        print("return Prefix & \"nothing()\";") # always Maybe
        print("end if;")
        print("case N.Kind is")
        for n in context.astnode_types:
            if n.abstract:
                continue
            else:
                print(f"when LALCO.{n.ada_kind_name} =>")
                print("-- Langkit_Support.Slocs.Image (N.Sloc_Range);")

                if n.is_list_type or n.public_type.api_name.lower == "ada_list":
                    # use rascal list
                    print("declare")
                    print("use Ada.Strings.Unbounded;")
                    print("s : Unbounded_String := To_Unbounded_String (Prefix & Just & \"[\");")
                    print("IsEmpty : Boolean := True;")
                    print("begin")
                    print(f"for node of N.As_{n.public_type.api_name.camel_with_underscores} loop")
                    print("Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & \",\");") # no list of maybe
                    print("IsEmpty := False;")
                    print("end loop;")
                    print("if not IsEmpty then")
                    print("Replace_Element (s, Length(s), ' ');")
                    print("Append (s, Prefix & \"]\" & End_Just);")
                    print("else")
                    print("Append (s, \"]\" & End_Just);")
                    print("end if;")
                    print("return To_String (s);")
                    print("end;")

                elif n.public_type.api_name.lower.endswith("_absent"):
                    print("return Prefix & \"nothing()\";")  # always Maybe

                elif n.public_type.api_name.lower.endswith("_present"):
                    print(f"return Prefix & \"just({n.base.public_type.api_name.lower}())\";") # always Maybe

                else:
                    print(f"return Prefix & Just & \"{n.public_type.api_name.lower} (\" &", end="")
                    if n.is_token_node:
                        print(f"Prefix & Tab & \"\"\"\" & Langkit_Support.Text.Image (N.Text) & \"\"\"\" & ", end ="")
                        if len(n.get_parse_fields(include_inherited=True)) != 0:
                            print("\", \" &")
                    i = 1
                    for field in n.get_parse_fields(include_inherited=True):
                        print(f"Export_AST_To_Rascal (N.As_{n.public_type.api_name.camel_with_underscores}.{field.api_name.camel_with_underscores}, Indent + 1, Pretty_Print, {field.is_optional}) & ",end ="")
                        if i != len(n.get_parse_fields(include_inherited=True)):
                            print("\", \" & ", end="")
                        i = i + 1
                    print("Prefix & \")\" & End_Just;")

        print("end case;")
        print("end Export_AST_To_Rascal;")
