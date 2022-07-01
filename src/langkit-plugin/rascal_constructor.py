from collections import OrderedDict
from langkit.compiled_types import Field
from type_mapping import *
from rascal_data_types import RascalDataTypes

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
            if field.precise_element_types.minimal_common_type.is_root_node:
                s = set({RascalDataTypes.get_associated_rascal_type(n) for n in field.precise_element_types.minimal_matched_types})
                if len(s) == 1:
                    field_type_name = f"list[{s.pop()}]"
                elif s == {"Declaration", "Statement"}:
                    field_type_name = f"list[Stmt_Or_Decl]"
                    field_with_chained_constructor.add(field)
                elif s == {"Expression", "Assoc"}:
                    field_type_name = f"list[Expr_Or_Assoc]"
                    field_with_chained_constructor.add(field)
                else:
                    print("Warning Ada_Node not resolve " + str(s))
                    element_contained = field_type.element_type
                    field_type_name = f"list[{RascalDataTypes.get_associated_rascal_type(element_contained)}]"
            else:
                element_contained = field_type.element_type
                field_type_name = f"list[{RascalDataTypes.get_associated_rascal_type(element_contained)}]"
        elif field_type.is_bool_node:
            inheritance_chain = field_type.get_inheritance_chain()
            # ValueIO.readTextValueFile can't read Maybe field, we are using set instead
            # To remove when the bug is fixed
            # https://github.com/usethesource/rascal/issues/1615
            field_type_name = f"Maybe[{RascalDataTypes.get_associated_rascal_type(field_type)}]"

        else:
            if field.precise_types.minimal_common_type.is_root_node:
                s = set({RascalDataTypes.get_associated_rascal_type(n) if not n.is_list else RascalDataTypes.get_associated_rascal_type(n.element_type)
                         for n in field.precise_types.minimal_matched_types})
                if len(s) == 1:
                    field_type_name = s.pop()
                elif s == {"Declaration", "Statement"}:
                    field_type_name = "Stmt_Or_Decl"
                    field_with_chained_constructor.add(field)
                elif s == {"Expression", "Assoc"}:
                    field_type_name = "Expr_Or_Assoc"
                    field_with_chained_constructor.add(field)
                else:
                    print("Warning Ada_Node not resolve " + str(s))
                    field_type_name = RascalDataTypes.get_associated_rascal_type(field_type)
            else:
                field_type_name = RascalDataTypes.get_associated_rascal_type(field_type)

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
