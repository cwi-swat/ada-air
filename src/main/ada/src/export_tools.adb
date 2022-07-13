with Strings_Utils;
with Libadalang.Analysis;
with Libadalang.Common;
with Ada.Strings.Wide_Wide_Unbounded;
with Langkit_Support.Slocs;
with Ada.Characters.Conversions;

package body Export_Tools is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   function Add_Indent_Level (Print_Context : Print_Context_Record_Type) 
                              return Print_Context_Record_Type is
   begin
      return Result : Print_Context_Record_Type do
         Result.File := Print_Context.File;
         Result.Indent := Print_Context.Indent + 1;
      end return;
   end Add_Indent_Level;

   function Get_Rascal_Physical_Location (N : LAL.Ada_Node'Class) return Wide_Wide_String is
      use Ada.Strings.Wide_Wide_Unbounded;
      use Langkit_Support.Slocs;
      LAL_Sloc : Source_Location_Range := N.Sloc_Range;
   begin
      LAL_Sloc.Start_Column := LAL_Sloc.Start_Column - 1;
      LAL_Sloc.End_Column := LAL_Sloc.End_Column - 1;
      declare
         Rascal_Sloc : Unbounded_Wide_Wide_String := To_Unbounded_Wide_Wide_String (Image (LAL_Sloc));
         Hyphen_Index : constant Positive := Index (Rascal_Sloc, "-");
         -- work-arround Rascal doesn't allow backslash
         FileName : constant Wide_Wide_String := Strings_Utils.Replace (Ada.Characters.Conversions.To_Wide_Wide_String (N.Unit.Get_Filename), "\", "/"); 
         offset : constant Positive := LALCO.Raw_Data (N.Token_Start).Source_First;
         lenght : constant Natural := LALCO.Raw_Data (N.Token_End).Source_Last - LALCO.Raw_Data (N.Token_Start).Source_First + 1;
      begin
         Strings_Utils.Replace (S => Rascal_Sloc, Pattern => ":" , Replacement =>  ",");
         Strings_Utils.Replace (S => Rascal_Sloc, Pattern => "-" , Replacement =>  ",");
         Insert (Rascal_Sloc, Hyphen_Index+1, "<");
         Insert (Rascal_Sloc, Hyphen_Index, ">");
         Rascal_Sloc := "|file:///" & FileName & "|(" & offset'Wide_Wide_Image & "," & lenght'Wide_Wide_Image & ",<" & Rascal_Sloc & ">)";
         return To_Wide_Wide_String (Rascal_Sloc);
      end;
   end Get_Rascal_Physical_Location;
   
   
   function Get_Rascal_Logical_Location (N : LAL.Basic_Decl'Class) return Wide_Wide_String is
      use Ada.Strings.Wide_Wide_Unbounded;
      Names : LAL.Unbounded_Text_Type_Array := N.P_Fully_Qualified_Name_Array;
      Res  : Unbounded_Wide_Wide_String := To_Unbounded_Wide_Wide_String ("|ada//");
   begin
      for N of Names loop
         Append(Res, "/" & N);
      end loop;
      Append(Res, "|");
      return To_Wide_Wide_String (Res);
   end Get_Rascal_Logical_Location;

    
end Export_Tools;
