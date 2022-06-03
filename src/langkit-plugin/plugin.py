import langkit.passes
from langkit.compile_context import CompileCtx
from langkit.compiled_types import SymbolType, CompiledType


class PluginPass(langkit.passes.AbstractPass):

    def __init__(self):
        super().__init__("Plugin Pass!!!")

    def run(self, context: CompileCtx) -> None:
        print("----- Plugin -----")
        # self.emit_dot_visualisation(context)
        self.emit_rascal_data_types(context)

    @staticmethod
    def emit_dot_visualisation(context: CompileCtx) -> None:
        print("digraph D {")
        for n in context.entity_types:
            if n.base is not None:
                print(f"{n.base.api_name} -> {n.api_name}")
        print("}")

    @staticmethod
    def emit_rascal_data_types(context: CompileCtx) -> None:
        for n in context.entity_types:
            print(f"data {n.api_name} = (", end="")
            if n.element_type.is_root_list_type:
                # Adding a field "content" for list node
                print(f"List[{n.element_type.element_type.api_name}] content", end="")
            else:
                fields = n.element_type.get_parse_fields(include_inherited=True)
                i = 0
                for field in fields:
                    field_type_name = field.type.entity.api_name if field.type.is_ast_node else field.type.api_name
                    print(f"{field_type_name} {field.api_name}", end="")
                    if i < len(fields) - 1:
                        print(", ", end="")
                    i = i + 1
            print(")")

