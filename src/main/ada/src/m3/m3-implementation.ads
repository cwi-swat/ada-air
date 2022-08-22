with Libadalang.Common;
with Libadalang.Analysis;
with M3.URI_Utils;

private package M3.Implementation is

   subtype Scope_Kind_Type is Libadalang.Common.Ada_Node_Kind_Type 
     with Static_Predicate => Scope_Kind_Type in
       Libadalang.Common.Ada_Decl_Block
       | Libadalang.Common.Ada_For_Loop_Stmt
       | Libadalang.Common.Ada_Exception_Handler
       | Libadalang.Common.Ada_Extended_Return_Stmt
       |Libadalang.Common.Ada_Named_Stmt;

   
   subtype Signature_Kind_Type is Libadalang.Common.Ada_Basic_Decl 
     with Static_Predicate => Signature_Kind_Type in
       Libadalang.Common.Ada_Subp_Decl 
       | Libadalang.Common.Ada_Generic_Subp_Decl
       | Libadalang.Common.Ada_Null_Subp_Decl
       | Libadalang.Common.Ada_Abstract_Subp_Decl
       | Libadalang.Common.Ada_Generic_Subp_Internal
       | Libadalang.Common.Ada_Subp_Body
       | Libadalang.Common.Ada_Expr_Function
       | Libadalang.Common.Ada_Entry_Decl;
   
   
   subtype Skipped_Kind_Type is Libadalang.Common.Ada_Basic_Decl
     with Static_Predicate => Skipped_Kind_Type in
       Libadalang.Common.Ada_Generic_Package_Internal
         | Libadalang.Common.Ada_Generic_Subp_Internal;
   --subtype Instantiation_Signature_Kind_Type is Libadalang.Common.Ada_Basic_Decl 
   --  with Static_Predicate => Subprogram_Kind_Type in

   
   
   
   function Rascal_Fully_Qualified_Name (N : Libadalang.Analysis.Defining_Name) return Wide_Wide_String;
   
   function Get_Rascal_Physical_Location (N : Libadalang.Analysis.Ada_Node'Class) return URI_Utils.URI;
   
   function Get_Rascal_Logical_Location (N : Libadalang.Analysis.Basic_Decl'Class; Name : Libadalang.Analysis.Name := Libadalang.Analysis.No_Name) return Wide_Wide_String;
       
private
   
   function Get_Subprogram_Signature (N : Libadalang.Analysis.Basic_Decl'Class) return Wide_Wide_String;
   
   function Get_Params_Signature (Params : Libadalang.Analysis.Param_Spec_Array) return Wide_Wide_String;
   
   function Get_Type_Signature (N : Libadalang.Analysis.Type_Expr'Class) return Wide_Wide_String;
   
end M3.Implementation;
