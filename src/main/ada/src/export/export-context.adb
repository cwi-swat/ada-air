package body Export.Context is

   function Add_Indent_Level
     (Print_Context : Print_Context_Record_Type)
      return Print_Context_Record_Type
   is
   begin
      return Result : Print_Context_Record_Type do
         Result.File   := Print_Context.File;
         Result.Indent := Print_Context.Indent + 1;
      end return;
   end Add_Indent_Level;


end Export.Context;
