with Export.Ast;
with Ada.Exceptions;
package body Ada_Wrapper is

   function Ada_Export_File_Wrapper (Ada_File : Interfaces.C.Strings.Chars_Ptr;
                                    Out_File : Interfaces.C.Strings.Chars_Ptr)
                                    return Interfaces.C.Strings.Chars_Ptr is
   begin
      Export.Ast.Export_File (Interfaces.C.Strings.Value (Ada_File), Interfaces.C.Strings.Value (Out_File));
      return Interfaces.C.Strings.Null_Ptr;
      exception
         when E : others =>
            return Interfaces.C.Strings.New_String (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E));
   end Ada_Export_File_Wrapper;


   function Ada_Export_Project_Wrapper (Ada_Project : Interfaces.C.Strings.Chars_Ptr;
                                        Out_File : Interfaces.C.Strings.Chars_Ptr)
                                        return Interfaces.C.Strings.Chars_Ptr is
   begin
      Export.Ast.Export_Project (Interfaces.C.Strings.Value (Ada_Project), Interfaces.C.Strings.Value (Out_File));
      return Interfaces.C.Strings.Null_Ptr;
      exception
         when E : others =>
            return Interfaces.C.Strings.New_String (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E));
   end Ada_Export_Project_Wrapper;


end Ada_Wrapper;
