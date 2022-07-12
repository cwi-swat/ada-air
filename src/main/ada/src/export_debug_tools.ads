with Libadalang.Common;
package Export_Debug_Tools is
   
   type Constructors_Used_Array is array(Libadalang.Common.Ada_Node_Kind_Type) of Boolean;
   
   procedure Save_Constructors_Used (A : Constructors_Used_Array);
   
   function Load_Constructors_Used return Constructors_Used_Array;

end Export_Debug_Tools;
