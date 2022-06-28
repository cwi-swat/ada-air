import os
from collections import OrderedDict

import langkit.passes
from langkit.compile_context import CompileCtx
from langkit.compiled_types import CompiledType, Field, ASTNodeType
from mako.template import Template

racal_types_mapping = {'Keyword': {'tagged_node', 'constant_node', 'abstract_node', 'aliased_node', 'abort_node', 'limited_node', 'protected_node', 'reverse_node', 'all_node', 'private_node', 'overriding_node', 'synchronized_node', 'until_node'},
                       'Array_Indices': {'array_indices'},
                       'Assoc': {'basic_assoc', 'aspect_assoc', 'base_assoc'},
                       'Spec': {'aspect_spec', 'loop_spec', 'range_spec'},
                       'Base_Formal_Param_Holder': {'base_formal_param_holder'},
                       'Def': {'task_def', 'component_def', 'type_def', 'base_record_def', 'protected_def'},
                       'Declaration': {'paren_abstract_state_decl', 'basic_decl', 'multi_abstract_state_decl', 'null_component_decl'},
                       'Statement': {'handled_stmts', 'elsif_stmt_part', 'case_stmt_alternative', 'stmt', 'pragma_node', 'component_clause', 'aspect_clause', 'with_clause', 'use_clause'},
                       'Compilation_Unit': {'compilation_unit'},
                       'Constraint': {'constraint'},
                       'Declarative_Part': {'declarative_part'},
                       'Expression': {'expr', 'elsif_expr_part', 'type_expr', 'others_designator'},
                       'Interface_Kind': {'interface_kind'},
                       'Iter_Type': {'iter_type'},
                       'Mode': {'mode'},
                       'Not_Null': {'not_null'},
                       'Params': {'params'},
                       'Quantifier': {'quantifier'},
                       'Renaming_Clause': {'renaming_clause'},
                       'Select_When_Part': {'select_when_part'},
                       'Subp_Kind': {'subp_kind'},
                       'Unconstrained_Array_Index': {'unconstrained_array_index'},
                       'Variant': {'variant'},
                       'Variant_Part': {'variant_part'},
                       'With_Private': {'with_private'},
                       'Unit': {'library_item', 'subunit'}}

types_extended_from_m3 = {"Declaration", "Statement", "Expression"}
field_with_chained_constructor = set({})


def Stmt_Or_Decl(t: ASTNodeType):
    if t.is_root_node:
        return None
    else:
        name = t.get_inheritance_chain()[1].public_type.api_name.lower
        if name in racal_types_mapping["Declaration"]:
            return "decl_kind"
        elif name in racal_types_mapping["Statement"]:
            return "stmt_kind"
        return None

def Expr_Or_Assoc(t: ASTNodeType):
    if t.is_root_node:
        return None
    else:
        name = t.get_inheritance_chain()[1].public_type.api_name.lower
        if t.is_list:
            if t.element_type.is_root_node:
                return None
            else:
                name = t.element_type.get_inheritance_chain()[1].public_type.api_name.lower
        if name in racal_types_mapping["Expression"]:
            return "expr_kind"
        elif name in racal_types_mapping["Assoc"]:
            return "assoc_kind"
        return None


chained_constructor_fun = {"Stmt_Or_Decl": Stmt_Or_Decl,
                           "Expr_Or_Assoc": Expr_Or_Assoc}

def get_chained_constructor(t: ASTNodeType):
    for n, fun in chained_constructor_fun.items():
        if fun(t) is not None:
            return fun(t)
    return None

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


class RascalDataTypes:

    def __init__(self):
        self._types = dict({})  # {Key : type name (camel case with underscore), Value : List of Constructors}

    def add_constructor(self, t: CompiledType, constructor: RascalConstructor):
        type_name = RascalDataTypes.get_associated_rascal_type(t)
        if type_name not in self._types:
            self._types[type_name] = []
        self._types[type_name].append(constructor)

    def get_types(self) -> dict:
        return self._types.copy()

    @staticmethod
    def get_associated_rascal_type(t: ASTNodeType) -> str:
        if t.is_root_node:
            return t.public_type.api_name.camel_with_underscores
        else:
            name = t.get_inheritance_chain()[1].public_type.api_name.lower
            for rascal_type_name, lal_types_name in racal_types_mapping.items():
                if name in lal_types_name:
                    return rascal_type_name
            raise RuntimeError(f"{name} not present in _rascal_types")

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
            priority = dict({}) # {key: type name, value: priority (int 1-5)}
            # 1 (Low priority) : X F_X. e.g. Stmt F_Stmt
            # 2 : Ada_Node
            # 3 : others
            # 4 : Short name without underscore
            # 5 : Optional field e.g. Maybe[..]
            for t in types:
                if t.startswith("set["):
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


class RascalPass(langkit.passes.AbstractPass):

    templates_dir = os.path.dirname(__file__) + "/templates/"
    
    inlined_prefix_nodes = {"bin_op": "",  # (Key : Inlined nodes, Value : Prefix to use e.g. add, u_add, rel_add, mem_add)
                            "un_op": "u_",
                            "relation_op": "rel_",
                            "membership_expr": "mem_"}

    def __init__(self):
        super().__init__("rascal plugin pass")


    def run(self, context: CompileCtx) -> None:
        if context.verbosity.info:
            print("Generate rascal sources...")
        self.emit_rascal_data_types(context)
        self.emit_exportation_function(context)
        return

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

        rascal_types.rename_fields_redeclaration()
        output_dir = os.path.dirname(__file__) + "/../main/rascal/lang/ada/"
        tmp = Template(filename=RascalPass.templates_dir + "rascal_ast.mako")
        with open(output_dir + 'AST.rsc', 'w') as f:
            f.write(tmp.render(types=rascal_types, types_extended_from_m3=types_extended_from_m3))

    @staticmethod
    def emit_exportation_function(context: CompileCtx):
        output_dir = os.path.dirname(__file__) + "/../main/ada/src/"
        if not os.path.isdir(output_dir):
            os.mkdir(output_dir)
        tmp = Template(filename=RascalPass.templates_dir + "ada_main.mako")
        with open(output_dir + 'main.adb', 'w') as f:
            f.write(tmp.render(ctx=context, inlined_prefix_nodes = RascalPass.inlined_prefix_nodes, chained_constructor_fun = chained_constructor_fun, field_with_chained_constructor =  field_with_chained_constructor, get_chained_constructor= get_chained_constructor))


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
                    f.write("\"{0}\" -> \"{1}\"\n".format(parent, child))
            f.write("}\n")