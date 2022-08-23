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