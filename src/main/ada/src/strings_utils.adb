with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Langkit_Support.Text;
with Ada.Characters.Conversions;

package body Strings_Utils is

   function Camel_Case_To_Snake_Case(S : String) return String is
      use Ada.Strings.Unbounded;
      use Ada.Characters.Handling;
      Unb : Unbounded_String := To_Unbounded_String ("" & To_Lower(S(S'First)));
   begin
      for i in S'First+1..S'Last loop
         if Is_Upper (S(i)) then
            Append (Unb, "_");
         end if;
         Append (Unb, To_Lower (S(i)));
      end loop;
      return To_String(Unb);
   end Camel_Case_To_Snake_Case;

   function Replace
     (S : String; Pattern : String; Replacement : String) return String
   is
      Idx : Natural;
   begin
      Idx := Ada.Strings.Fixed.Index (S, Pattern);

      if Idx = 0 then
         return S;
      else
         return S (S'First .. Idx - 1) & Replacement
           & Replace
           (S           => S (Idx + Pattern'Length .. S'Last),
            Pattern     => Pattern,
            Replacement => Replacement);
      end if;
   end Replace;
   
   function Replace
     (S : Wide_Wide_String; Pattern : Wide_Wide_String; Replacement : Wide_Wide_String) return Wide_Wide_String
   is
      Idx : Natural;
   begin
      Idx := Ada.Strings.Wide_Wide_Fixed.Index (S, Pattern);

      if Idx = 0 then
         return S;
      else
         return S (S'First .. Idx - 1) & Replacement
           & Replace
           (S           => S (Idx + Pattern'Length .. S'Last),
            Pattern     => Pattern,
            Replacement => Replacement);
      end if;
   end Replace;
   
   procedure Replace
     (S           : in out Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Pattern     : Wide_Wide_String;
      Replacement : Wide_Wide_String)
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Ind : Natural := Index_Non_Blank (S);
   begin
      while Ind < Length (S) loop
         Ind := Index (S, Pattern, Ind);

         exit when Ind = 0;

         S := Replace_Slice (S, Ind, Ind + Pattern'Length - 1, Replacement);
         Ind := Ind + Replacement'Length;
      end loop;
   end Replace;
   
   function Format_String (S : Wide_Wide_String) return Wide_Wide_String is
   begin
      return Ada.Characters.Conversions.To_Wide_Wide_String (Langkit_Support.Text.Image (S, True));
   end Format_String;
   
end Strings_Utils;
