with Libadalang.Analysis;
with Ada.Strings.Wide_Wide_Unbounded;
package M3.Analysis is

   package LAL renames Libadalang.Analysis;
   
   function Get_Src_Annotation (N : LAL.Ada_Node'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   
   function Get_Use_Annotation (N : LAL.Name'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   
   function Get_Decl_Annotation (N : LAL.Basic_Decl'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   
   function Get_Containment_Annotation (N : LAL.Ada_Node'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

end M3.Analysis;
