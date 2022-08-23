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

from collections import OrderedDict
from langkit.compiled_types import Field
from rascal_context import RascalContext

class RascalConstructor:

    def __init__(self, type_name: str):
        self._name = type_name
        self._fields = OrderedDict({})  # {Key : field name, Value : field type name}

    def add_field(self, field: Field):
        self._fields[field.api_name.camel_with_underscores] = self.__get_rascal_field_type_name(field)

    def add_token_field(self):
        self._fields["content"] = "str"

    def add_custom_field(self, field_type:str, field_name:str):
        self._fields[field_name] = field_type


    def __get_rascal_field_type_name(self, field: Field) -> str:
        field_type = field.type.entity.astnode
        field_type_name = None
        if field_type.is_list:
            # If the returned node of this function is an Ada_Node or Ada_Node_List we try to found
            if field.precise_element_types.minimal_common_type.is_root_node or field.precise_element_types.minimal_common_type.is_root_list_type:
                s = set({RascalContext.get_associated_rascal_type(n) for n in field.precise_element_types.minimal_matched_types})
                if len(s) == 1 and s != {"Ada_Node"}:
                    field_type_name = f"list[{s.pop()}]"
                else:
                    for c in RascalContext.chained_constructors:
                        if s == c.get_associated_rascal_types():
                            field_type_name = f"list[{c.get_name()}]"
                            RascalContext.field_with_chained_constructor.add(field)       
                    if field_type_name is None:
                        # This condition needs to be removed when a new version of LAL including this fix is released in Alire
                        # https://github.com/AdaCore/libadalang/issues/945
                        if self._name == "component_list" and field.api_name.lower == "f_components":
                            field_type_name = f"list[Stmt_Or_Decl]"
                            RascalContext.field_with_chained_constructor.add(field)    
                        else:
                            print(f"Warning Ada_Node not resolve for: {self._name} {field.api_name.camel_with_underscores}")
                            element_contained = field_type.element_type
                            field_type_name = f"list[{RascalContext.get_associated_rascal_type(element_contained)}]"
                            
            else:
                element_contained = field_type.element_type
                field_type_name = f"list[{RascalContext.get_associated_rascal_type(element_contained)}]"
        elif field_type.is_bool_node:
            inheritance_chain = field_type.get_inheritance_chain()
            # Maybe are just an alias of List in rascal
            field_type_name = f"Maybe[{RascalContext.get_associated_rascal_type(field_type)}]"

        else:
            if field.precise_types.minimal_common_type.is_root_node:
                s = set({RascalContext.get_associated_rascal_type(n) if not n.is_list else RascalContext.get_associated_rascal_type(n.element_type)
                         for n in field.precise_types.minimal_matched_types})
                if len(s) == 1 and s != {"Ada_Node"}:
                    field_type_name = s.pop()
                else:
                    for c in RascalContext.chained_constructors:
                        if s == c.get_associated_rascal_types():
                            field_type_name = c.get_name()
                            RascalContext.field_with_chained_constructor.add(field)       
                    if field_type_name is None:
                        # This condition needs to be removed when a new version of LAL including this fix is released in Alire
                        # https://github.com/AdaCore/libadalang/issues/945
                        if self._name == "attribute_ref" or self._name == "update_attribute_ref" and field.api_name.lower == "f_args":
                            field_type_name = "Expr_Or_Assoc"
                            RascalContext.field_with_chained_constructor.add(field)
                        else:
                            print(f"Warning Ada_Node not resolve for: {self._name} {field.api_name.camel_with_underscores}")
                            field_type_name = RascalContext.get_associated_rascal_type(field_type)
            else:
                field_type_name = RascalContext.get_associated_rascal_type(field_type)

            if field.is_optional:
                field_type_name = "Maybe[" + field_type_name + "]" # optional fields can be null

        return field_type_name

    def get_fields(self) -> OrderedDict[str, str]:
        return self._fields.copy()

    def get_name(self) -> str:
        return self._name

    def apply_renaming_rules(self, renaming_rules: dict[str, [tuple[str, str]]]):
        for field_name, field_type in self._fields.copy().items():
            for field_to_change, rules in renaming_rules.items():
                for rule in rules:
                    # rule[0] type name
                    # rule[1] new field name
                    if field_name == field_to_change and rule[0] == field_type:
                        # we rebuild a new instance of _fields because we need to preserve the insertion order
                        tmp = OrderedDict([(rule[1], field_type) if k == field_name else (k, v) for k, v in self._fields.items()])
                        self._fields = tmp
                        break
