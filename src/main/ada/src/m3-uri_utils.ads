with Langkit_Support.Slocs;
with Ada.Strings.Wide_Wide_Unbounded;
with Libadalang.Analysis;

package M3.URI_Utils is

   subtype URI is Wide_Wide_String;
   
   Null_URI : URI := "";
   
   type Location_Record_Type is private;
   
   function Location (N : Libadalang.Analysis.Ada_Node'Class) 
                      return Location_Record_Type;
   
   function Create_URI (Scheme   : Wide_Wide_String;
                        Path     : Wide_Wide_String;
                        Location : Location_Record_Type) 
                        return URI;
      
   function Create_URI (Scheme     : Wide_Wide_String;
                        Path       : Wide_Wide_String) 
                        return URI;
         

private
   
   type Location_Record_Type is record
      Offset : Positive;
      Length : Natural;
      Sloc   : Langkit_Support.Slocs.Source_Location_Range;
   end record;
   
   procedure Espcate_Characters (URI : in out Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);
   
   function Image (Sloc : Langkit_Support.Slocs.Source_Location_Range) 
                   return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   
end M3.URI_Utils;
