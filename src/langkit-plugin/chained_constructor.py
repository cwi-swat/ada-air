from copyreg import constructor
from langkit.compiled_types import ASTNodeType
from abc import ABC, abstractmethod
from rascal_context import RascalContext
from rascal_constructor import RascalConstructor

class ChainedContructor(ABC):

    @staticmethod
    @abstractmethod
    def get_constructors() -> list[RascalConstructor]:
        pass

    @staticmethod
    @abstractmethod
    def get_name() -> str:
        pass

    @staticmethod
    @abstractmethod
    def get_constructor_name(t : ASTNodeType) -> str:
        pass

    @staticmethod
    @abstractmethod
    def get_associated_rascal_types() -> set[str]:
        pass


class Stmt_Or_Decl(ChainedContructor):

    @staticmethod
    def get_constructors() -> list[RascalConstructor]:
        decl_kind = RascalConstructor("decl_kind")
        decl_kind.add_custom_field("Declaration", "As_Decl")
        stmt_kind = RascalConstructor("stmt_kind")
        stmt_kind.add_custom_field("Statement", "As_Stmt")
        return [decl_kind, stmt_kind]

    @staticmethod
    def get_name() -> str:
        return "Stmt_Or_Decl"

    @staticmethod
    def get_constructor_name(t : ASTNodeType) -> str:
        if t.is_root_node:
            return None
        else:
            name = t.get_inheritance_chain()[1].public_type.api_name.lower
            if name in RascalContext.racal_types_mapping["Declaration"]:
                return "decl_kind"
            elif name in RascalContext.racal_types_mapping["Statement"]:
                return "stmt_kind"
            return None

    @staticmethod
    def get_associated_rascal_types() -> set[str]:
        return {"Statement", "Declaration"}


class Expr_Or_Assoc(ChainedContructor):

    @staticmethod
    def get_constructors() -> list[RascalConstructor]:
        expr_kind = RascalConstructor("expr_kind")
        expr_kind.add_custom_field("Expression", "As_Expr")
        assoc_kind = RascalConstructor("assoc_kind")
        assoc_kind.add_custom_field("list[Assoc]", "As_Assoc")
        return [expr_kind, assoc_kind]


    @staticmethod
    def get_name() -> str:
        return "Expr_Or_Assoc"

    @staticmethod
    def get_constructor_name(t : ASTNodeType) -> str:
        if t.is_root_node:
            return None
        else:
            name = t.get_inheritance_chain()[1].public_type.api_name.lower
            if t.is_list:
                if t.element_type.is_root_node:
                    return None
                else:
                    name = t.element_type.get_inheritance_chain()[1].public_type.api_name.lower
            if name in RascalContext.racal_types_mapping["Expression"]:
                return "expr_kind"
            elif name in RascalContext.racal_types_mapping["Assoc"]:
                return "assoc_kind"
            return None

    @staticmethod
    def get_associated_rascal_types() -> set[str]:
        return {"Expression", "Assoc"}