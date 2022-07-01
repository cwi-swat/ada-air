import os
from rascal_constructor import RascalConstructor
from rascal_data_types import RascalDataTypes
from type_mapping import *
import langkit.passes
from langkit.compile_context import CompileCtx
from mako.template import Template


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
            f.write(tmp.render(ctx=context, inlined_prefix_nodes = RascalPass.inlined_prefix_nodes, chained_constructor_fun = chained_constructor_fun, field_with_chained_constructor =  field_with_chained_constructor, get_chained_constructor= get_chained_constructor, decl_functions=decl_functions))


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
                    f.write("{0} -> {1}\n".format(parent, child))
            f.write("}\n")