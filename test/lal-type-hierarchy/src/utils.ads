with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Iterators;
with Ada.Text_IO;
with Langkit_Support.Text;
with Ada.Command_Line;
with Ada.Strings.Equal_Case_Insensitive;

package Utils is
   
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   
   function Load_Project (Project_Filename : String) return LAL.Unit_Provider_Reference;
   
   function Simple_Name (Node_Type : LAL.Type_Decl) return String;
   
   function Get_Ada_Node_Type_Decl (Root : LAL.Ada_Node) return LAL.Type_Decl;

end Utils;
