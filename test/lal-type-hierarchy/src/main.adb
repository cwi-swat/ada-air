with Ada.Text_IO;
with Utils;
with Libadalang.Analysis;
with Ada.Command_Line;

-- Libadalang based type hierarchy visualization using dot
-- The purpose of this tool is to compare its output with the langkit plugin output

procedure Main is
   package LAL renames Libadalang.Analysis;
   use Utils;

   Project_File : constant String := Ada.Command_Line.Argument (1);
   Ada_File     : constant String := Ada.Command_Line.Argument (2);

   Context : constant LAL.Analysis_Context := LAL.Create_Context (Unit_Provider => Load_Project (Project_File));
   Unit    : constant LAL.Analysis_Unit := Context.Get_From_File (Ada_File);
   Units   : constant LAL.Analysis_Unit_Array(1..1) := (1 => Unit);

   Derived_Types : constant LAL.Type_Decl_Array := Get_Ada_Node_Type_Decl (Unit.Root).P_Find_All_Derived_Types (Units);
begin
   Ada.Text_IO.Put_Line ("digraph D {");

   for D of Derived_Types loop
      if not D.P_Is_Private then
         declare
            Parent : constant LAL.Type_Decl := D.P_Base_Type.As_Type_Decl;
         begin
            if not Parent.Is_Null then
               Ada.Text_IO.Put_Line (Simple_Name (Parent) & " -> " & Simple_Name (D));
            end if;
         end;
      end if;
   end loop;

   Ada.Text_IO.Put_Line ("}");
end Main;
