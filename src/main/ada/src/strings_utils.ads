with Ada.Strings.Wide_Wide_Unbounded;
package Strings_Utils is

   function Camel_Case_To_Snake_Case (S : String) return String;
   
   function Replace (S           : String;
                     Pattern     : String;
                     Replacement : String) return String;
   
   function Replace (S           : Wide_Wide_String;
                     Pattern     : Wide_Wide_String;
                     Replacement : Wide_Wide_String) return Wide_Wide_String;
   
   procedure Replace
     (S           : in out Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Pattern     : Wide_Wide_String;
      Replacement : Wide_Wide_String);
   
   function Format_String (S : Wide_Wide_String) return Wide_Wide_String;
    pragma Inline (Format_String);  


end Strings_Utils;
