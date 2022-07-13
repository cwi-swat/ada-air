with Interfaces.C.Strings;
package Export_AST is
   
   function Ada_Func_Wrapper (ada_file : Interfaces.C.Strings.Chars_Ptr;
                              out_file : Interfaces.C.Strings.chars_Ptr) 
                            return Interfaces.C.Strings.chars_Ptr
     with
       Export        => True,
       Convention    => C,
       External_Name => "Ada_Func_Wrapper";
 
end Export_Ast;
