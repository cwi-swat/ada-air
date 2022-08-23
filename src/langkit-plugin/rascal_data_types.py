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

from langkit.compiled_types import CompiledType, ASTNodeType
from rascal_context import RascalContext
from rascal_constructor import RascalConstructor

class RascalDataTypes:

    def __init__(self):
        self._types = dict({})  # {Key : type name (camel case with underscore), Value : List of Constructors}

    def add_constructor(self, t: CompiledType, constructor : RascalConstructor):
        type_name = RascalContext.get_associated_rascal_type(t)
        if type_name not in self._types:
            self._types[type_name] = []
        self._types[type_name].append(constructor)

    def get_types(self) -> dict:
        return self._types.copy()

    @staticmethod
    def __compute_suffix(name: str) -> str:
        underscore = name.rfind("_")
        suffix = name
        last_open_bracket = name.rfind("[")
        first_close_bracket = name.find("]")

        if last_open_bracket != -1 and first_close_bracket != -1:
            suffix = name[last_open_bracket + 1: first_close_bracket]
            suffix = suffix + "_List"

        if underscore != -1:
            suffix = suffix[underscore + 1: len(suffix)]
        return suffix

    @staticmethod
    def __compute_renaming_rules(redeclarations: dict) -> dict[str, [tuple[str, str]]]:
        renaming_rules = {}  # {key: field name, value: list of (old type name, new field name)}
        for field_name, types in redeclarations.items():
            naming_convention = {"Declaration" : "Decl", # these 3 types extends m3 types but here we need their Libadalang names
                                "Statement" : "Stmt",
                                "Expression" : "Expr"}
            priority = dict({}) # {key: type name, value: priority (int 1-5)}
            # 1 (Low priority) : X F_X. e.g. Stmt F_Stmt
            # 2 : Ada_Node
            # 3 : others
            # 4 : Short name without underscore
            # 5 : Optional field e.g. Maybe[..]
            # 6 : List
            for t in types:
                if t.startswith("Maybe["):
                    priority[t] = 5
                elif t.startswith("list["):
                    priority[t] = 6
                elif t in field_name:
                    priority[t] = 1
                elif t in naming_convention and naming_convention[t] in field_name:
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
                elif prio == 6:
                    renaming_rules[field_name].append((type_name, field_name + "_List"))
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