@license{
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
alias Ada_Node = node;

data Keyword(loc src=|unknown:///|) = abort_node ()
| abstract_node ()
| aliased_node ()
| all_node ()
| constant_node ()
| limited_node ()
| overriding_not_overriding ()
| overriding_overriding ()
| overriding_unspecified ()
| pragma_node (Expr F_Id, Maybe[list[Assoc]] F_Args)
| private_node ()
| protected_node ()
| reverse_node ()
| synchronized_node ()
| tagged_node ()
| until_node ()
;
data Array_Indices(loc src=|unknown:///|) = constrained_array_indices (list[Ada_Node] F_List)
| unconstrained_array_indices (list[Unconstrained_Array_Index] F_Types)
;
data Assoc(loc src=|unknown:///|) = aspect_assoc (Expr F_Id, Maybe[Expr] F_Expr)
| contract_case_assoc (Ada_Node F_Guard, Expr F_Consequence)
| pragma_argument_assoc (Maybe[Expr] F_Name, Expr F_Expr_Expr)
| aggregate_assoc (Maybe[list[Ada_Node]] F_Designators, Expr F_R_Expr)
| multi_dim_array_assoc (Maybe[list[Ada_Node]] F_Designators, Expr F_R_Expr)
| discriminant_assoc (Maybe[list[Expr]] F_Ids, Expr F_Discr_Expr)
| iterated_assoc (Spec F_Spec, Expr F_R_Expr)
| param_assoc (Maybe[Ada_Node] F_Designator, Expr F_R_Expr)
;
data Aspect_Clause(loc src=|unknown:///|) = at_clause (Expr F_Name, Expr F_Expr)
| attribute_def_clause (Expr F_Attribute_Expr, Expr F_Expr)
| enum_rep_clause (Expr F_Type_Name, Expr F_Aggregate)
| record_rep_clause (Expr F_Name, Maybe[Expr] F_At_Expr, list[Ada_Node] F_Components)
;
data Spec(loc src=|unknown:///|) = aspect_spec (list[Assoc] F_Aspect_Assocs)
| entry_spec (Expr F_Entry_Name, Maybe[Ada_Node] F_Family_Type, Maybe[Params] F_Entry_Params)
| enum_subp_spec ()
| subp_spec (Subp_Kind F_Subp_Kind, Maybe[Expr] F_Subp_Name, Maybe[Params] F_Subp_Params, Maybe[Expr] F_Subp_Returns)
| for_loop_spec (Decl F_Var_Decl, Iter_Type F_Loop_Type, Maybe[Keyword] F_Has_Reverse, Ada_Node F_Iter_Expr)
| while_loop_spec (Expr F_Expr)
| range_spec (Expr F_Range)
;
data Base_Formal_Param_Holder(loc src=|unknown:///|) = component_list (Maybe[list[Ada_Node]] F_Components, Maybe[Variant_Part] F_Variant_Part)
| known_discriminant_part (list[Decl] F_Discr_Specs)
| unknown_discriminant_part ()
| entry_completion_formal_params (Maybe[Params] F_Params)
| generic_formal_part (list[Ada_Node] F_Decls)
;
data Def(loc src=|unknown:///|) = null_record_def (Base_Formal_Param_Holder F_Components)
| record_def (Base_Formal_Param_Holder F_Components)
| component_def (Maybe[Keyword] F_Has_Aliased, Maybe[Keyword] F_Has_Constant, Expr F_Type_Expr)
| protected_def (Declarative_Part F_Public_Part, Maybe[Declarative_Part] F_Private_Part, Maybe[Expr] F_End_Name)
| task_def (Maybe[list[Expr]] F_Interfaces, Declarative_Part F_Public_Part, Maybe[Declarative_Part] F_Private_Part, Maybe[Expr] F_End_Name)
| access_to_subp_def (Maybe[Not_Null] F_Has_Not_Null, Maybe[Keyword] F_Has_Protected, Spec F_Subp_Spec)
| anonymous_type_access_def (Maybe[Not_Null] F_Has_Not_Null, Decl F_Type_Decl)
| type_access_def (Maybe[Not_Null] F_Has_Not_Null, Maybe[Keyword] F_Has_All, Maybe[Keyword] F_Has_Constant, Expr F_Subtype_Indication)
| array_type_def (Array_Indices F_Indices, Def F_Component_Type)
| derived_type_def (Maybe[Keyword] F_Has_Abstract, Maybe[Keyword] F_Has_Limited, Maybe[Keyword] F_Has_Synchronized, Expr F_Subtype_Indication, Maybe[list[Expr]] F_Interfaces, Maybe[Def] F_Record_Extension, Maybe[With_Private] F_Has_With_Private)
| enum_type_def (list[Decl] F_Enum_Literals)
| formal_discrete_type_def ()
| interface_type_def (Maybe[Interface_Kind] F_Interface_Kind, Maybe[list[Expr]] F_Interfaces)
| mod_int_type_def (Expr F_Expr)
| private_type_def (Maybe[Keyword] F_Has_Abstract, Maybe[Keyword] F_Has_Tagged, Maybe[Keyword] F_Has_Limited)
| decimal_fixed_point_def (Expr F_Delta, Expr F_Digits, Maybe[Spec] F_Range)
| floating_point_def (Expr F_Num_Digits, Maybe[Spec] F_Range)
| ordinary_fixed_point_def (Expr F_Delta, Maybe[Spec] F_Range)
| record_type_def (Maybe[Keyword] F_Has_Abstract, Maybe[Keyword] F_Has_Tagged, Maybe[Keyword] F_Has_Limited, Def F_Record_Def)
| signed_int_type_def (Spec F_Range_Spec)
;
data Decl(loc src=|unknown:///|) = abstract_state_decl (Expr F_Name_Expr, Maybe[Spec] F_Aspects)
| anonymous_expr_decl (Expr F_Expr, Spec F_Aspects_Spec)
| component_decl (list[Expr] F_Ids, Def F_Component_Def, Maybe[Expr] F_Default_Expr, Maybe[Spec] F_Aspects)
| discriminant_spec (list[Expr] F_Ids, Expr F_Type_Expr, Maybe[Expr] F_Default_Expr, Spec F_Aspects_Spec)
| generic_formal_obj_decl (Decl F_Decl, Spec F_Aspects_Spec)
| generic_formal_package (Decl F_Decl, Spec F_Aspects_Spec)
| generic_formal_subp_decl (Decl F_Decl, Spec F_Aspects_Spec)
| generic_formal_type_decl (Decl F_Decl, Spec F_Aspects_Spec)
| param_spec (list[Expr] F_Ids, Maybe[Keyword] F_Has_Aliased, Maybe[Mode] F_Mode, Expr F_Type_Expr, Maybe[Expr] F_Default_Expr, Maybe[Spec] F_Aspects)
| generic_package_internal (Expr F_Package_Name, Maybe[Spec] F_Aspects, Declarative_Part F_Public_Part, Maybe[Declarative_Part] F_Private_Part, Maybe[Expr] F_End_Name)
| package_decl (Expr F_Package_Name, Maybe[Spec] F_Aspects, Declarative_Part F_Public_Part, Maybe[Declarative_Part] F_Private_Part, Maybe[Expr] F_End_Name)
| discrete_base_subtype_decl (Maybe[Expr] F_Name, Spec F_Aspects_Spec)
| subtype_decl (Maybe[Expr] F_Name, Expr F_Subtype_Expr, Maybe[Spec] F_Aspects)
| classwide_type_decl (Maybe[Expr] F_Name, Spec F_Aspects_Spec)
| incomplete_type_decl (Maybe[Expr] F_Name, Maybe[Base_Formal_Param_Holder] F_Discriminants, Spec F_Aspects_Spec)
| incomplete_tagged_type_decl (Maybe[Expr] F_Name, Maybe[Base_Formal_Param_Holder] F_Discriminants, Spec F_Aspects_Spec, Maybe[Keyword] F_Has_Abstract)
| protected_type_decl (Maybe[Expr] F_Name, Maybe[Base_Formal_Param_Holder] F_Discriminants, Maybe[Spec] F_Aspects, Maybe[list[Expr]] F_Interfaces, Def F_Definition_Def)
| task_type_decl (Maybe[Expr] F_Name, Maybe[Base_Formal_Param_Holder] F_Discriminants, Maybe[Spec] F_Aspects, Maybe[Def] F_Definition)
| single_task_type_decl (Maybe[Expr] F_Name, Maybe[Base_Formal_Param_Holder] F_Discriminants, Maybe[Spec] F_Aspects, Maybe[Def] F_Definition)
| type_decl (Maybe[Expr] F_Name, Maybe[Base_Formal_Param_Holder] F_Discriminants, Def F_Type_Def, Maybe[Spec] F_Aspects)
| anonymous_type_decl (Maybe[Expr] F_Name, Maybe[Base_Formal_Param_Holder] F_Discriminants, Def F_Type_Def, Maybe[Spec] F_Aspects)
| synth_anonymous_type_decl (Maybe[Expr] F_Name, Maybe[Base_Formal_Param_Holder] F_Discriminants, Def F_Type_Def, Maybe[Spec] F_Aspects)
| abstract_subp_decl (Keyword F_Overriding, Spec F_Subp_Spec, Maybe[Spec] F_Aspects)
| abstract_formal_subp_decl (Keyword F_Overriding, Spec F_Subp_Spec, Maybe[Expr] F_Default_Expr, Maybe[Spec] F_Aspects)
| concrete_formal_subp_decl (Keyword F_Overriding, Spec F_Subp_Spec, Maybe[Expr] F_Default_Expr, Maybe[Spec] F_Aspects)
| subp_decl (Keyword F_Overriding, Spec F_Subp_Spec, Maybe[Spec] F_Aspects)
| entry_decl (Keyword F_Overriding, Spec F_Spec, Maybe[Spec] F_Aspects)
| enum_literal_decl (Expr F_Name_Expr, Spec F_Aspects_Spec)
| generic_subp_internal (Spec F_Subp_Spec, Maybe[Spec] F_Aspects)
| expr_function (Keyword F_Overriding, Spec F_Subp_Spec, Expr F_Expr, Maybe[Spec] F_Aspects)
| null_subp_decl (Keyword F_Overriding, Spec F_Subp_Spec, Maybe[Spec] F_Aspects)
| subp_body (Keyword F_Overriding, Spec F_Subp_Spec, Maybe[Spec] F_Aspects, Declarative_Part F_Decls_Part, Stmt F_Stmts, Maybe[Expr] F_End_Name)
| subp_renaming_decl (Keyword F_Overriding, Spec F_Subp_Spec, Renaming_Clause F_Renames, Maybe[Spec] F_Aspects)
| package_body_stub (Expr F_Name_Expr, Maybe[Spec] F_Aspects)
| protected_body_stub (Expr F_Name_Expr, Maybe[Spec] F_Aspects)
| subp_body_stub (Keyword F_Overriding, Spec F_Subp_Spec, Maybe[Spec] F_Aspects)
| task_body_stub (Expr F_Name_Expr, Maybe[Spec] F_Aspects)
| entry_body (Expr F_Entry_Name, Maybe[Decl] F_Index_Spec, Base_Formal_Param_Holder F_Params, Maybe[Spec] F_Aspects, Expr F_Barrier, Declarative_Part F_Decls_Part, Stmt F_Stmts, Maybe[Expr] F_End_Name)
| package_body (Expr F_Package_Name, Maybe[Spec] F_Aspects, Declarative_Part F_Decls_Part, Maybe[Stmt] F_Stmts_Maybe, Maybe[Expr] F_End_Name)
| protected_body (Expr F_Name_Expr, Maybe[Spec] F_Aspects, Declarative_Part F_Decls_Part, Maybe[Expr] F_End_Name)
| task_body (Expr F_Name_Expr, Maybe[Spec] F_Aspects, Declarative_Part F_Decls_Part, Stmt F_Stmts, Maybe[Expr] F_End_Name)
| entry_index_spec (Expr F_Id, Ada_Node F_Subtype, Spec F_Aspects_Spec)
| error_decl (Spec F_Aspects_Spec)
| exception_decl (list[Expr] F_Ids, Maybe[Renaming_Clause] F_Renames_Maybe, Maybe[Spec] F_Aspects)
| exception_handler (Maybe[Expr] F_Exception_Name, list[Ada_Node] F_Handled_Exceptions, list[Ada_Node] F_Stmts_Node, Spec F_Aspects_Spec)
| for_loop_var_decl (Expr F_Id, Maybe[Expr] F_Id_Type, Spec F_Aspects_Spec)
| generic_package_decl (Base_Formal_Param_Holder F_Formal_Part, Decl F_Package_Decl, Spec F_Aspects_Spec)
| generic_subp_decl (Base_Formal_Param_Holder F_Formal_Part, Decl F_Subp_Decl, Spec F_Aspects_Spec)
| generic_package_instantiation (Expr F_Name_Expr, Expr F_Generic_Pkg_Name, Maybe[list[Assoc]] F_Params_Assoc, Maybe[Spec] F_Aspects)
| generic_subp_instantiation (Keyword F_Overriding, Subp_Kind F_Kind, Expr F_Subp_Name, Expr F_Generic_Subp_Name, Maybe[list[Assoc]] F_Params_Assoc, Maybe[Spec] F_Aspects)
| generic_package_renaming_decl (Expr F_Name_Expr, Expr F_Renames_Expr, Maybe[Spec] F_Aspects)
| generic_subp_renaming_decl (Subp_Kind F_Kind, Expr F_Name_Expr, Expr F_Renames_Expr, Maybe[Spec] F_Aspects)
| label_decl (Expr F_Name_Expr, Spec F_Aspects_Spec)
| named_stmt_decl (Expr F_Name_Expr, Spec F_Aspects_Spec)
| number_decl (list[Expr] F_Ids, Expr F_Expr, Spec F_Aspects_Spec)
| object_decl (list[Expr] F_Ids, Maybe[Keyword] F_Has_Aliased, Maybe[Keyword] F_Has_Constant, Maybe[Mode] F_Mode, Expr F_Type_Expr, Maybe[Expr] F_Default_Expr, Maybe[Renaming_Clause] F_Renaming_Clause, Maybe[Spec] F_Aspects)
| extended_return_stmt_object_decl (list[Expr] F_Ids, Maybe[Keyword] F_Has_Aliased, Maybe[Keyword] F_Has_Constant, Maybe[Mode] F_Mode, Expr F_Type_Expr, Maybe[Expr] F_Default_Expr, Maybe[Renaming_Clause] F_Renaming_Clause, Maybe[Spec] F_Aspects)
| package_renaming_decl (Expr F_Name_Expr, Renaming_Clause F_Renames, Maybe[Spec] F_Aspects)
| single_protected_decl (Expr F_Name_Expr, Maybe[Spec] F_Aspects, Maybe[list[Expr]] F_Interfaces, Def F_Definition_Def)
| single_task_decl (Decl F_Task_Type, Spec F_Aspects_Spec)
| multi_abstract_state_decl (list[Ada_Node] F_Decls)
| null_component_decl ()
| paren_abstract_state_decl (Ada_Node F_Decl_Node)
;
data Stmt(loc src=|unknown:///|) = case_stmt_alternative (list[Ada_Node] F_Choices, list[Ada_Node] F_Stmts_Node)
| elsif_stmt_part (Expr F_Cond_Expr_Expr, list[Ada_Node] F_Stmts_Node)
| handled_stmts (list[Ada_Node] F_Stmts_Node, Maybe[list[Ada_Node]] F_Exceptions)
| accept_stmt (Expr F_Name, Maybe[Expr] F_Entry_Index_Expr, Base_Formal_Param_Holder F_Params)
| accept_stmt_with_stmts (Expr F_Name, Maybe[Expr] F_Entry_Index_Expr, Base_Formal_Param_Holder F_Params, Stmt F_Stmts, Maybe[Expr] F_End_Name)
| for_loop_stmt (Maybe[Spec] F_Spec, list[Ada_Node] F_Stmts_Node, Maybe[Expr] F_End_Name)
| loop_stmt (Maybe[Spec] F_Spec, list[Ada_Node] F_Stmts_Node, Maybe[Expr] F_End_Name)
| while_loop_stmt (Maybe[Spec] F_Spec, list[Ada_Node] F_Stmts_Node, Maybe[Expr] F_End_Name)
| begin_block (Stmt F_Stmts, Maybe[Expr] F_End_Name)
| decl_block (Declarative_Part F_Decls, Stmt F_Stmts, Maybe[Expr] F_End_Name)
| case_stmt (Expr F_Expr, list[Stmt] F_Alternatives)
| extended_return_stmt (Decl F_Decl, Maybe[Stmt] F_Stmts_Maybe)
| if_stmt (Expr F_Cond_Expr_Expr, list[Ada_Node] F_Then_Stmts, list[Stmt] F_Alternatives, Maybe[list[Ada_Node]] F_Else_Stmts)
| named_stmt (Decl F_Decl, Stmt F_Stmt)
| select_stmt (list[Select_When_Part] F_Guards, Maybe[list[Ada_Node]] F_Else_Stmts, Maybe[list[Ada_Node]] F_Abort_Stmts)
| error_stmt ()
| abort_stmt (list[Expr] F_Names)
| assign_stmt (Expr F_Dest, Expr F_Expr)
| call_stmt (Expr F_Call)
| delay_stmt (Maybe[Keyword] F_Has_Until, Expr F_Expr)
| exit_stmt (Maybe[Expr] F_Loop_Name, Maybe[Expr] F_Cond_Expr)
| goto_stmt (Expr F_Label_Name)
| label (Decl F_Decl)
| null_stmt ()
| raise_stmt (Maybe[Expr] F_Exception_Name, Maybe[Expr] F_Error_Message)
| requeue_stmt (Expr F_Call_Name, Maybe[Keyword] F_Has_Abort)
| return_stmt (Maybe[Expr] F_Return_Expr)
| terminate_alternative ()
;
data Compilation_Unit(loc src=|unknown:///|) = compilation_unit (list[Ada_Node] F_Prelude, Ada_Node F_Body, list[Keyword] F_Pragmas)
;
data Component_Clause(loc src=|unknown:///|) = component_clause (Expr F_Id, Expr F_Position, Spec F_Range)
;
data Constraint(loc src=|unknown:///|) = delta_constraint (Expr F_Digits, Maybe[Spec] F_Range)
| digits_constraint (Expr F_Digits, Maybe[Spec] F_Range)
| discriminant_constraint (list[Assoc] F_Constraints_Assoc)
| index_constraint (list[Ada_Node] F_Constraints)
| range_constraint (Spec F_Range_Spec)
;
data Declarative_Part(loc src=|unknown:///|) = declarative_part (list[Ada_Node] F_Decls)
| private_part (list[Ada_Node] F_Decls)
| public_part (list[Ada_Node] F_Decls)
;
data Expr(loc src=|unknown:///|) = elsif_expr_part (Expr F_Cond_Expr, Expr F_Then_Expr)
| abstract_state_decl_expr (Ada_Node F_State_Decl)
| allocator (Maybe[Expr] F_Subpool, Ada_Node F_Type_Or_Expr)
| aggregate (Maybe[Expr] F_Ancestor_Expr, Maybe[list[Assoc]] F_Assocs)
| bracket_aggregate (Maybe[Expr] F_Ancestor_Expr, Maybe[list[Assoc]] F_Assocs)
| delta_aggregate (Maybe[Expr] F_Ancestor_Expr, Maybe[list[Assoc]] F_Assocs)
| bracket_delta_aggregate (Maybe[Expr] F_Ancestor_Expr, Maybe[list[Assoc]] F_Assocs)
| null_record_aggregate (Maybe[Expr] F_Ancestor_Expr, Maybe[list[Assoc]] F_Assocs)
| box_expr ()
| case_expr_alternative (list[Ada_Node] F_Choices, Expr F_Expr)
| case_expr (Expr F_Expr, list[Expr] F_Cases)
| if_expr (Expr F_Cond_Expr, Expr F_Then_Expr, list[Expr] F_Alternatives, Maybe[Expr] F_Else_Expr)
| contract_cases (list[Assoc] F_Contract_Cases)
| decl_expr (list[Decl] F_Decls, Expr F_Expr)
| attribute_ref (Expr F_Prefix, Expr F_Attribute, Maybe[Ada_Node] F_Args)
| update_attribute_ref (Expr F_Prefix, Expr F_Attribute, Maybe[Ada_Node] F_Args)
| call_expr (Expr F_Name, Ada_Node F_Suffix)
| defining_name (Expr F_Name)
| discrete_subtype_name (Expr F_Subtype)
| dotted_name (Expr F_Prefix, Expr F_Suffix_Expr)
| end_name (Expr F_Name)
| explicit_deref (Expr F_Prefix)
| qual_expr (Expr F_Prefix, Expr F_Suffix_Expr)
| char_literal (str content)
| identifier (str content)
| \abs (Expr F_Left, Expr F_Right)
| \abs (Expr F_Expr)
| \abs (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \and (Expr F_Left, Expr F_Right)
| \and (Expr F_Expr)
| \and (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \and_then (Expr F_Left, Expr F_Right)
| \and_then (Expr F_Expr)
| \and_then (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \concat (Expr F_Left, Expr F_Right)
| \concat (Expr F_Expr)
| \concat (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \div (Expr F_Left, Expr F_Right)
| \div (Expr F_Expr)
| \div (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \double_dot (Expr F_Left, Expr F_Right)
| \double_dot (Expr F_Expr)
| \double_dot (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \eq (Expr F_Left, Expr F_Right)
| \eq (Expr F_Expr)
| \eq (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \gt (Expr F_Left, Expr F_Right)
| \gt (Expr F_Expr)
| \gt (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \gte (Expr F_Left, Expr F_Right)
| \gte (Expr F_Expr)
| \gte (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \in (Expr F_Left, Expr F_Right)
| \in (Expr F_Expr)
| \in (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \lt (Expr F_Left, Expr F_Right)
| \lt (Expr F_Expr)
| \lt (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \lte (Expr F_Left, Expr F_Right)
| \lte (Expr F_Expr)
| \lte (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \minus (Expr F_Left, Expr F_Right)
| \minus (Expr F_Expr)
| \minus (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \mod (Expr F_Left, Expr F_Right)
| \mod (Expr F_Expr)
| \mod (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \mult (Expr F_Left, Expr F_Right)
| \mult (Expr F_Expr)
| \mult (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \neq (Expr F_Left, Expr F_Right)
| \neq (Expr F_Expr)
| \neq (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \not (Expr F_Left, Expr F_Right)
| \not (Expr F_Expr)
| \not (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \not_in (Expr F_Left, Expr F_Right)
| \not_in (Expr F_Expr)
| \not_in (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \or (Expr F_Left, Expr F_Right)
| \or (Expr F_Expr)
| \or (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \or_else (Expr F_Left, Expr F_Right)
| \or_else (Expr F_Expr)
| \or_else (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \plus (Expr F_Left, Expr F_Right)
| \plus (Expr F_Expr)
| \plus (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \pow (Expr F_Left, Expr F_Right)
| \pow (Expr F_Expr)
| \pow (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \rem (Expr F_Left, Expr F_Right)
| \rem (Expr F_Expr)
| \rem (Expr F_Expr, list[Expr] F_Membership_Exprs)
| \xor (Expr F_Left, Expr F_Right)
| \xor (Expr F_Expr)
| \xor (Expr F_Expr, list[Expr] F_Membership_Exprs)
| string_literal (str content)
| null_literal (str content)
| int_literal (str content)
| real_literal (str content)
| target_name ()
| paren_expr (Expr F_Expr)
| quantified_expr (Quantifier F_Quantifier, Spec F_Loop_Spec, Expr F_Expr)
| raise_expr (Maybe[Expr] F_Exception_Name, Maybe[Expr] F_Error_Message)
| anonymous_type (Decl F_Type_Decl)
| enum_lit_synth_type_expr ()
| subtype_indication (Maybe[Not_Null] F_Has_Not_Null, Expr F_Name, Maybe[Constraint] F_Constraint)
| constrained_subtype_indication (Maybe[Not_Null] F_Has_Not_Null, Expr F_Name, Maybe[Constraint] F_Constraint)
| discrete_subtype_indication (Maybe[Not_Null] F_Has_Not_Null, Expr F_Name, Maybe[Constraint] F_Constraint)
;
data Interface_Kind(loc src=|unknown:///|) = interface_kind_limited ()
| interface_kind_protected ()
| interface_kind_synchronized ()
| interface_kind_task ()
;
data Iter_Type(loc src=|unknown:///|) = iter_type_in ()
| iter_type_of ()
;
data Library_Item(loc src=|unknown:///|) = library_item (Maybe[Keyword] F_Has_Private, Decl F_Item)
;
data Mode(loc src=|unknown:///|) = mode_default ()
| mode_in ()
| mode_in_out ()
| mode_out ()
;
data Not_Null(loc src=|unknown:///|) = not_null ()
;
data Others_Designator(loc src=|unknown:///|) = others_designator ()
;
data Params(loc src=|unknown:///|) = params (list[Decl] F_Params)
;
data Quantifier(loc src=|unknown:///|) = quantifier_all ()
| quantifier_some ()
;
data Renaming_Clause(loc src=|unknown:///|) = renaming_clause (Expr F_Renamed_Object)
| synthetic_renaming_clause (Expr F_Renamed_Object)
;
data Select_When_Part(loc src=|unknown:///|) = select_when_part (Maybe[Expr] F_Cond_Expr, list[Ada_Node] F_Stmts)
;
data Subp_Kind(loc src=|unknown:///|) = subp_kind_function ()
| subp_kind_procedure ()
;
data Subunit(loc src=|unknown:///|) = subunit (Expr F_Name, Decl F_Body)
;
data Unconstrained_Array_Index(loc src=|unknown:///|) = unconstrained_array_index (Expr F_Subtype_Indication)
;
data Use_Clause(loc src=|unknown:///|) = use_package_clause (list[Expr] F_Packages)
| use_type_clause (Maybe[Keyword] F_Has_All, list[Expr] F_Types)
;
data Variant(loc src=|unknown:///|) = variant (list[Ada_Node] F_Choices, Base_Formal_Param_Holder F_Components)
;
data Variant_Part(loc src=|unknown:///|) = variant_part (Expr F_Discr_Name, list[Variant] F_Variant)
;
data With_Clause(loc src=|unknown:///|) = with_clause (Maybe[Keyword] F_Has_Limited, Maybe[Keyword] F_Has_Private, list[Expr] F_Packages)
;
data With_Private(loc src=|unknown:///|) = with_private ()
;
