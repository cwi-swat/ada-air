with Libadalang.Analysis;
with Libadalang.Common;
with Ada.Wide_Wide_Text_IO;

package Export_Tools is

   subtype Entry_Point_Enum_Type is Libadalang.Common.Ada_Node_Kind_Type with Static_Predicate => 
     Entry_Point_Enum_Type in Libadalang.Common.Ada_Compilation_Unit
       | Libadalang.Common.Ada_Compilation_Unit_List
         | Libadalang.Common.Ada_Pragma_Node_List;
   
   type File_Type_Access is access all Ada.Wide_Wide_Text_IO.File_Type;

   type Print_Context_Record_Type is record
      File         : File_Type_Access;
      Indent       : Natural;  -- used only on debug mode
   end record;
   
   type Type_Context_Record_Type is record
      N                        : Libadalang.Analysis.Ada_Node;
      Is_Optional              : Boolean;
      Need_Chained_Constructor : Boolean;
   end record;
   
   function Add_Indent_Level (Print_Context : Print_Context_Record_Type) 
                              return Print_Context_Record_Type;
   
end Export_Tools;
