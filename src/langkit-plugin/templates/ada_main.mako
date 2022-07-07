<%
def get_decl(n):
   hasDecl = False
   props = [p.api_name.camel_with_underscores for p in n.get_properties(predicate=lambda p: p.is_public)]
   fun = None
   for key in decl_functions:
      if key in props:
         hasDecl = True
         fun = key
         break
   if not hasDecl:
      return None
   args = decl_functions[fun]
   call = None
   if len(args) > 0:
      call = "N.As_{}.{}({})".format(n.public_type.api_name.camel_with_underscores, fun, ", ".join(args))
   else:
      call = f"N.As_{n.public_type.api_name.camel_with_underscores}.{fun}"

   return "(if not {}.Is_Null then \"decl=\" & To_Rascal_Sloc_Range({}) else \"\")".format(call,call)
%>\
with Ada.Text_IO;
with Libadalang.Analysis;
with Libadalang.Common;
with Langkit_Support.Text;
with Langkit_Support.Slocs;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with GNATCOLL.Utils;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded.Text_IO;

package body Export_Ast is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   type Constructor_Visited_Array is array(LALCO.Ada_Node_Kind_Type) of Boolean;
   Constructor_Visited : Constructor_Visited_Array := (others => False);

   function To_Rascal_Sloc_Range (N : LAL.Ada_Node'Class) return String is
      use Ada.Strings.Unbounded;
      use Langkit_Support.Slocs;
      LAL_Sloc : Source_Location_Range := N.Sloc_Range;
   begin
      LAL_Sloc.Start_Column := LAL_Sloc.Start_Column - 1;
      LAL_Sloc.End_Column := LAL_Sloc.End_Column - 1;
      declare
         Rascal_Sloc : Unbounded_String := To_Unbounded_String (Langkit_Support.Text.Image (Image (LAL_Sloc)));
         Hyphen_Index : constant Positive := Index (Rascal_Sloc, "-");
         FileName : constant String := GNATCOLL.Utils.Replace (N.Unit.Get_Filename, "\", "/"); -- work-arround Rascal doesn't allow backslash
         offset : constant Positive := LALCO.Raw_Data (N.Token_Start).Source_First;
         lenght : constant Natural := LALCO.Raw_Data (N.Token_End).Source_Last - LALCO.Raw_Data (N.Token_Start).Source_First + 1;
      begin
         GNATCOLL.Utils.Replace (S => Rascal_Sloc, Pattern => ":" , Replacement =>  ",");
         GNATCOLL.Utils.Replace (S => Rascal_Sloc, Pattern => "-" , Replacement =>  ",");
         Insert (Rascal_Sloc, Hyphen_Index+1, "<");
         Insert (Rascal_Sloc, Hyphen_Index, ">");
         Rascal_Sloc := "|file:///" & FileName & "|(" & offset'Image & "," & lenght'Image & ",<" & Rascal_Sloc & ">)";
         return To_String (Rascal_Sloc);
      end;
   end To_Rascal_Sloc_Range;

   function Escape_Quotes (S : String) return String is (GNATCOLL.Utils.Replace (s, """", "\"""));

       
   function Lower_Name_With_Underscore(S : String) return String is
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
   end Lower_Name_With_Underscore;
   
   procedure Export_Ast_To_Rascal (F : Ada.Text_IO.File_Type; N : LAL.Ada_Node'Class; Indent : Natural := 0; Pretty_Print : Boolean := True; IsOptional : Boolean := False; Need_Chained_Constructor : Boolean := False) is
      use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;
      Tab : constant string := (if Pretty_Print then "|  " else " ");
      Prefix : constant string := (if Pretty_Print then LF & (Indent * Tab) else "");
      Just : constant string := (if IsOptional then "[" else " ");
      End_Just : constant string := (if IsOptional then "]" else " ");
      src : constant string := (if not N.Is_Null then "src=" & To_Rascal_Sloc_Range(N) else " ");
   begin
      if N.Is_Null then
         Ada.Text_IO.Put (F, Prefix);
         Ada.Text_IO.Put (F, "[]");
         return;
      end if;
   case N.Kind is
      % for n in ctx.astnode_types:
         % if not n.abstract:
      when LALCO.${n.ada_kind_name} =>
         Constructor_Visited (LALCO.${n.ada_kind_name}) := True;
            % if n.is_list or n.is_root_list_type: # list can't be optional
         declare
               IsEmpty : Boolean := True;
         begin
            Ada.Text_IO.Put (F, Prefix);
               % if get_chained_constructor(n) is not None:
            if Need_Chained_Constructor then
               Ada.Text_IO.Put (F, "${get_chained_constructor(n)}(");
            end if;
               % endif
            Ada.Text_IO.Put_Line(F, "[");
            for node of N.As_${n.public_type.api_name.camel_with_underscores} loop
               if not IsEmpty then
                  Ada.Text_IO.Put (F, ","); -- no list of maybe
               end if;
               % if get_chained_constructor(n) is not None:
               Export_Ast_To_Rascal (F, node, Indent + 1, Pretty_Print, False, False);
               % else:
               Export_Ast_To_Rascal (F, node, Indent + 1, Pretty_Print, False, Need_Chained_Constructor);
               % endif
               IsEmpty := False;
            end loop;
            Ada.Text_IO.Put (F, "]");
               % if get_chained_constructor(n) is not None:
            if Need_Chained_Constructor then
               Ada.Text_IO.Put (F, ",");
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F, ")");
            end if;
               % endif
            return;
         end;
            % elif n.public_type.api_name.lower.endswith("_absent"):
         Ada.Text_IO.Put (F, Prefix);
         Ada.Text_IO.Put (F, "[]");
         return;  -- always Maybe

               % elif n.public_type.api_name.lower.endswith("_present"):          
         Ada.Text_IO.Put (F, Prefix);
         Ada.Text_IO.Put (F, "[${n.base.public_type.api_name.lower}(");
         Ada.Text_IO.Put (F, src);
         Ada.Text_IO.Put (F, ")]");
         return; --  always Maybe

               % elif n.public_type.api_name.lower in inlined_prefix_nodes:
         declare
               op_full_name  : constant String := N.As_${n.public_type.api_name.camel_with_underscores}.F_Op.Kind_Name;
               op_name       : constant String := Lower_Name_With_Underscore (op_full_name(3..op_full_name'Last));
         begin
                  % if get_chained_constructor(n) is not None:
            if Need_Chained_Constructor then
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, Just);
               Ada.Text_IO.Put (F, "${get_chained_constructor(n)}");
               Ada.Text_IO.Put (F, "(");
               Ada.Text_IO.Put (F, "${inlined_prefix_nodes[n.public_type.api_name.lower]}");
               Ada.Text_IO.Put (F, op_name);
               Ada.Text_IO.Put (F, "(");
                     % for field in n.get_parse_fields(include_inherited=True):
                        % if field.api_name.lower != "f_op":
               Export_Ast_To_Rascal (F, N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}, Indent + 1, Pretty_Print, ${field.is_optional});
               Ada.Text_IO.Put (F, ", ");
                        % endif
                     % endfor
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F, "), ");
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F, ")");
               Ada.Text_IO.Put (F, End_Just);
               return;
            else
                  % endif
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, Just);
               Ada.Text_IO.Put (F, "${inlined_prefix_nodes[n.public_type.api_name.lower]}");
               Ada.Text_IO.Put (F, op_name);
               Ada.Text_IO.Put (F, "(");
                  % for field in n.get_parse_fields(include_inherited=True):
                     % if field.api_name.lower != "f_op":
               Export_Ast_To_Rascal (F, N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}, Indent + 1, Pretty_Print, ${field.is_optional});
               Ada.Text_IO.Put (F, ", ");
                     % endif
                  % endfor
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F, ")");
               Ada.Text_IO.Put (F, End_Just);
               return;
                  % if get_chained_constructor(n) is not None:
            end if;
                  % endif
         end;

               % else:
                  % if get_chained_constructor(n) is not None:
                     % if get_decl(n) is not None:
         declare
               decl : Unbounded_String;
         begin
            begin
               decl := To_Unbounded_String (${get_decl(n)});
            exception
               -- Int_Literal has this property but it isn't implemented
               -- TODO find a better way to handle this
               when others =>
                  decl := Null_Unbounded_String;
            end;
                     % endif
         if Need_Chained_Constructor then
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, Just);
               Ada.Text_IO.Put (F, "${get_chained_constructor(n)}");
               Ada.Text_IO.Put (F, "(");
               Ada.Text_IO.Put (F, "${n.public_type.api_name.lower} (");
                     % if n.is_token_node:
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, Tab);
               Ada.Text_IO.Put (F, """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """");
               Ada.Text_IO.Put (F,  ", ");
                     % endif
                     % for field in n.get_parse_fields(include_inherited=True):                
               Export_Ast_To_Rascal (F, N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}, Indent + 1, Pretty_Print, ${field.is_optional}, ${field in field_with_chained_constructor});
               Ada.Text_IO.Put (F, ", ");
                     % endfor
                     % if get_decl(n) is not None:
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F, ", ");
               Ada.Strings.Unbounded.Text_IO.Put (F, decl);
               Ada.Text_IO.Put (F, "),");
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F, ")");
               Ada.Text_IO.Put (F, End_Just);
               return;
                     % else:
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F, "),");
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F,  ")");
               Ada.Text_IO.Put (F, End_Just);
               return;
                     % endif
         else
               % endif
            Ada.Text_IO.Put (F, Prefix);
            Ada.Text_IO.Put (F, Just);
            Ada.Text_IO.Put (F, "${n.public_type.api_name.lower} (");
               % if n.is_token_node:
            Ada.Text_IO.Put (F, Prefix);
            Ada.Text_IO.Put (F, Tab);
            Ada.Text_IO.Put (F,"""" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """");
            Ada.Text_IO.Put (F, ", ");
               % endif
               % for field in n.get_parse_fields(include_inherited=True):
            Export_Ast_To_Rascal (F, N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}, Indent + 1, Pretty_Print, ${field.is_optional}, ${field in field_with_chained_constructor});
            Ada.Text_IO.Put (F, ", ");
               % endfor
               % if get_decl(n) is not None:
            Ada.Text_IO.Put (F, Prefix);
            Ada.Text_IO.Put (F, src);
            Ada.Text_IO.Put (F,", ");
            Ada.Strings.Unbounded.Text_IO.Put (F, decl);
            Ada.Text_IO.Put (F, ")");
            Ada.Text_IO.Put (F, End_Just);
            return;
               % else:
            Ada.Text_IO.Put (F, Prefix);
            Ada.Text_IO.Put (F, src);
            Ada.Text_IO.Put (F, ")");
            Ada.Text_IO.Put (F, End_Just);
            return;
               % endif
               % if get_chained_constructor(n) is not None:
         end if;
               % endif
               % if get_decl(n) is not None:
         end;
               % endif
            % endif
         % endif
      % endfor
      end case;
   end Export_Ast_To_Rascal;

   procedure Export (File_Name : String; Out_File_Name : String; Pretty_Print : Boolean) is
      Context : constant LAL.Analysis_Context := LAL.Create_Context;
      Unit    : constant LAL.Analysis_Unit := Context.Get_From_File (File_Name);
      F       : Ada.Text_IO.File_Type;
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Unit.Format_GNU_Diagnostic (D));
         end loop;
      else
         Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Out_File_Name);
         Export_Ast_To_Rascal (F           => F,
                              N            => Unit.Root,
                              Indent       => 0,
                              Pretty_Print => Pretty_Print);
         Ada.Text_IO.Close (F);
      end if;
   end Export;


   procedure Ada_Func_Wrapper (ada_file : Interfaces.C.Strings.Chars_Ptr;
                              out_file : Interfaces.C.Strings.Chars_Ptr) is
   begin
      Export (Interfaces.C.Strings.Value (ada_file), Interfaces.C.Strings.Value (out_file), False);
   end Ada_Func_Wrapper;

end Export_Ast;