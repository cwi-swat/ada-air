from langkit.compiled_types import ASTNodeType

racal_types_mapping = {'Keyword': {'tagged_node', 'constant_node', 'abstract_node', 'aliased_node', 'abort_node', 'limited_node', 'protected_node', 'reverse_node', 'all_node', 'private_node', 'overriding_node', 'synchronized_node', 'until_node'},
                       'Array_Indices': {'array_indices'},
                       'Assoc': {'basic_assoc', 'aspect_assoc', 'base_assoc'},
                       'Spec': {'aspect_spec', 'loop_spec', 'range_spec'},
                       'Base_Formal_Param_Holder': {'base_formal_param_holder'},
                       'Def': {'task_def', 'component_def', 'type_def', 'base_record_def', 'protected_def'},
                       'Declaration': {'paren_abstract_state_decl', 'basic_decl', 'multi_abstract_state_decl', 'null_component_decl'},
                       'Statement': {'handled_stmts', 'elsif_stmt_part', 'case_stmt_alternative', 'stmt', 'pragma_node', 'component_clause', 'aspect_clause', 'with_clause', 'use_clause', 'value_sequence'},
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
                       'Unit': {'library_item', 'subunit'},
                       'PP_Directive' : {'pp_directive', 'pp_then_kw'}}

types_extended_from_m3 = {"Declaration", "Statement", "Expression"}
decl_functions = dict({"P_Referenced_Decl" : ["False"]})
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


