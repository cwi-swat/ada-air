With Ada.Strings.Wide_Wide_Fixed;
with Strings_Utils;
with Ada.Integer_Wide_Wide_Text_IO;
with Libadalang.Common;

package body M3.URI_Utils is

   package LALCO renames Libadalang.Common;

   --------------
   -- Location --
   --------------

   function Location
     (N : Libadalang.Analysis.Ada_Node'Class) return Location_Record_Type
   is
      Offset : constant Positive := LALCO.Raw_Data (N.Token_Start).Source_First;
      Length : constant Natural :=LALCO.Raw_Data (N.Token_End).Source_Last -
        LALCO.Raw_Data (N.Token_Start).Source_First + 1;
      Sloc   : constant Langkit_Support.Slocs.Source_Location_Range := N.Sloc_Range;
   begin
      return (Offset => Offset,
              Length => Length,
              Sloc => Sloc);
   end Location;

   ------------------------
   -- Espcate_Characters --
   ------------------------

   procedure Espcate_Characters (URI : in out Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
      Invalid_Character : constant Wide_Wide_String := "><""";
      Hex : Wide_Wide_String (1..6);
   begin
      for Char of Invalid_Character loop
         Ada.Integer_Wide_Wide_Text_IO.Put (To => Hex,
                                            Item => Wide_Wide_Character'Pos(Char),
                                            Base => 16);
         declare
            -- Ada Hex code to URI Hex code
            -- 16#D5# => %D5
            Hex_Code : constant Wide_Wide_String (1..3) := (1 => '%', 2 => Hex (4), 3 => Hex (5));
         begin
            Strings_Utils.Replace (S => Uri,
                                   Pattern =>  Char & "",
                                   Replacement =>  Hex_Code);
         end;
      end loop;
   end Espcate_Characters;

   -----------
   -- Image --
   -----------

   function Image
     (Sloc : Langkit_Support.Slocs.Source_Location_Range)
      return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use Langkit_Support.Slocs;
      Result : Unbounded_Wide_Wide_String :=
        To_Unbounded_Wide_Wide_String ("<" & Image (Sloc) & ">");
      Hyphen_Index : constant Positive := Index (Result, "-");
   begin
      Strings_Utils.Replace
        (S => Result, Pattern => ":", Replacement => ",");
      Strings_Utils.Replace
        (S => Result, Pattern => "-", Replacement => ",");
      Insert (Result, Hyphen_Index + 1, "<");
      Insert (Result, Hyphen_Index, ">");
      return Result;
   end Image;

   ----------------
   -- Create_URI --
   ----------------

   function Create_URI
     (Scheme   : Wide_Wide_String; Path : Wide_Wide_String;
      Location : Location_Record_Type)
      return URI
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      --use Langkit_Support.Slocs;
      use Ada.Strings.Wide_Wide_Fixed;
      Result : Unbounded_Wide_Wide_String;
   begin
      Set_Unbounded_Wide_Wide_String (Result, "|" & Scheme & "://");
      Append (Result, Path);
      Append (Result, "|(");
      Espcate_Characters (Result);
      Append (Result, Trim (Location.Offset'Wide_Wide_Image, Ada.Strings.Both));
      Append (Result, ",");
      Append (Result, Trim (Location.Length'Wide_Wide_Image, Ada.Strings.Both));
      Append (Result, ",");
      Append (Result, Image (Location.Sloc));
      Append (Result, ")");
      return To_Wide_Wide_String (Result);
   end Create_URI;

   ----------------
   -- Create_URI --
   ----------------

   function Create_URI
     (Scheme : Wide_Wide_String; Path : Wide_Wide_String)
      return URI
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Unbounded_Wide_Wide_String;
   begin
      Set_Unbounded_Wide_Wide_String (Result, "|" & Scheme & "://");
      Append (Result, Path & "|");
      Espcate_Characters (Result);
      return To_Wide_Wide_String (Result);
   end Create_URI;

end M3.URI_Utils;
