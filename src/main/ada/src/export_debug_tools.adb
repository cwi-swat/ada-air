with Ada.Text_IO;
with GNATCOLL.Utils;
with Ada.Strings.Unbounded;
with Ada.Directories;
with ada.Strings.Fixed;
  
package body Export_Debug_Tools is
   package LALCO renames Libadalang.Common;
   

   File_Name : constant String := "array.txt";

   procedure Save_Constructors_Used (A : Constructors_Used_Array) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => F, Mode => Ada.Text_IO.Out_File, Name =>  File_Name);
      for I in A'Range loop
         Ada.Text_IO.Put_line (F, Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both)
                               & ":" &
                                 Ada.Strings.Fixed.Trim (A (I)'Image, Ada.Strings.Both));        
      end loop;
      Ada.Text_IO.Close (F);      
   end Save_Constructors_Used;
   
   function Load_Constructors_Used return Constructors_Used_Array is
      Result : Constructors_Used_Array := (others => False);
      F : Ada.Text_IO.File_Type;
   begin
      if Ada.Directories.Exists (File_Name) then
         Ada.Text_IO.Open (File => F, Mode => Ada.Text_IO.In_File, Name =>  File_Name);
         while not Ada.Text_IO.End_Of_File (F) loop
            declare
               Line : constant String := Ada.Text_IO.Get_Line (F);
               Split : constant GNATCOLL.Utils.Unbounded_String_Array := GNATCOLL.Utils.Split (Str => Line, On => ':');
               Kind : constant LALCO.Ada_Node_Kind_Type := LALCO.Ada_Node_Kind_Type'Value (Ada.Strings.Unbounded.To_String (Split(Split'First)));
               Value : constant Boolean := Boolean'Value (Ada.Strings.Unbounded.To_String (Split(Split'First + 1)));
            begin
               Result (Kind) := Value;
            end;
         end loop;
      else
         Ada.Text_IO.Create (File => F, Mode => Ada.Text_IO.In_File, Name =>  File_Name);
      end if;
      Ada.Text_IO.Close (F);
      return result;
   end Load_Constructors_Used;
   

end Export_Debug_Tools;
