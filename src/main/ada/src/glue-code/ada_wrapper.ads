with Interfaces.C.Strings;
package Ada_Wrapper is
   
   function Ada_Export_File_Wrapper (Ada_File : Interfaces.C.Strings.Chars_Ptr;
                                     Out_File : Interfaces.C.Strings.chars_Ptr) 
                                     return Interfaces.C.Strings.chars_Ptr
     with
       Export        => True,
       Convention    => C,
       External_Name => "Ada_Export_File_Wrapper";
   
   function Ada_Export_Project_Wrapper (Ada_Project : Interfaces.C.Strings.Chars_Ptr;
                                        Out_File : Interfaces.C.Strings.chars_Ptr) 
                                        return Interfaces.C.Strings.chars_Ptr
     with
       Export        => True,
       Convention    => C,
       External_Name => "Ada_Export_Project_Wrapper";

end Ada_Wrapper;
