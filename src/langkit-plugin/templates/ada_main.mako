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
% if debug:
with Ada.Directories;
% endif

package body Export_Ast is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   % if debug:
   type Constructor_Visited_Array is array(LALCO.Ada_Node_Kind_Type) of Boolean;
   Constructor_Visited : Constructor_Visited_Array := (others => False);

   procedure Save_Constructor_Visited is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => F, Mode => Ada.Text_IO.Out_File, Name =>  "array.txt");
      for I in Constructor_Visited'Range loop
         Ada.Text_IO.Put_line (F, Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both)
                               & ":" &
                               Ada.Strings.Fixed.Trim (Constructor_Visited (I)'Image, Ada.Strings.Both));        
      end loop;
      Ada.Text_IO.Close (F);      
   end Save_Constructor_Visited;
   
   procedure Load_Constructor_Visited is
      F : Ada.Text_IO.File_Type;
   begin
      if Ada.Directories.Exists ("array.txt") then
         Ada.Text_IO.Open (File => F, Mode => Ada.Text_IO.In_File, Name =>  "array.txt");
      else
         Ada.Text_IO.Create (File => F, Mode => Ada.Text_IO.In_File, Name =>  "array.txt");
      end if;
      while not Ada.Text_IO.End_Of_File (F) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (F);
            Split : GNATCOLL.Utils.Unbounded_String_Array := GNATCOLL.Utils.Split (Str => Line, On => ':');
            Kind : constant LALCO.Ada_Node_Kind_Type := LALCO.Ada_Node_Kind_Type'Value (Ada.Strings.Unbounded.To_String (Split(Split'First)));
            Value : constant Boolean := Boolean'Value (Ada.Strings.Unbounded.To_String (Split(Split'First + 1)));
         begin
            Constructor_Visited (Kind) := Value;
         end;
      end loop;      
      Ada.Text_IO.Close (F);
   end Load_Constructor_Visited;
   % endif

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


   subtype Entry_Point is LALCO.Ada_Node_Kind_Type with Static_Predicate => 
      Entry_Point in LALCO.Ada_Compilation_Unit
      | LALCO.Ada_Compilation_Unit_List
      | LALCO.Ada_Pragma_Node_List;

   
   procedure Export_Ast_To_Rascal (F : Ada.Text_IO.File_Type; N : LAL.Ada_Node'Class; Indent : Natural := 0; Pretty_Print : Boolean := True; IsOptional : Boolean := False; Need_Chained_Constructor : Boolean := False) is
      use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;
      Is_Root : constant Boolean := not N.Is_Null and then N.Parent.Is_Null;
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
      
      if Is_Root then
         if N.Kind not in Entry_Point then
            raise Program_Error with N.Kind_Name & "isn't an entry point";
         else
            case Entry_Point (N.Kind) is
               when LALCO.Ada_Compilation_Unit =>
                  Ada.Text_IO.Put (F, "Compilation_Units_Kind([");
               when LALCO.Ada_Compilation_Unit_List =>
                  Ada.Text_IO.Put (F, "Compilation_Units_Kind(");
               when LALCO.Ada_Pragma_Node_List =>
                  Ada.Text_IO.Put (F, "Statements_Kind(");
            end case;
         end if;
      end if;

   case N.Kind is
      % for n in ctx.astnode_types:
         % if not n.abstract:
      when LALCO.${n.ada_kind_name} =>
         % if debug:
         Constructor_Visited (LALCO.${n.ada_kind_name}) := True;
         % endif
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
         end;
            % elif n.public_type.api_name.lower.endswith("_absent"):
         Ada.Text_IO.Put (F, Prefix);
         Ada.Text_IO.Put (F, "[]");  -- always Maybe

               % elif n.public_type.api_name.lower.endswith("_present"):          
         Ada.Text_IO.Put (F, Prefix);
         Ada.Text_IO.Put (F, "[${n.base.public_type.api_name.lower}(");
         Ada.Text_IO.Put (F, src);
         Ada.Text_IO.Put (F, ")]"); --  always Maybe

               % elif n.public_type.api_name.lower in inlined_prefix_nodes:
         declare
               op_full_name  : constant String := N.As_${n.public_type.api_name.camel_with_underscores}.F_Op.Kind_Name;
               op_name       : constant String := Lower_Name_With_Underscore (op_full_name(3..op_full_name'Last));
         begin
            % if debug: 
            Constructor_Visited (N.As_${n.public_type.api_name.camel_with_underscores}.F_Op.Kind) := True;
            % endif
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
                     % else:
               Ada.Text_IO.Put (F, Prefix);
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F, "),");
               Ada.Text_IO.Put (F, src);
               Ada.Text_IO.Put (F,  ")");
               Ada.Text_IO.Put (F, End_Just);
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
               % else:
            Ada.Text_IO.Put (F, Prefix);
            Ada.Text_IO.Put (F, src);
            Ada.Text_IO.Put (F, ")");
            Ada.Text_IO.Put (F, End_Just);
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

      if Is_Root then
         case Entry_Point (N.Kind) is
            when LALCO.Ada_Compilation_Unit =>
               Ada.Text_IO.Put (F, "])");
            when LALCO.Ada_Compilation_Unit_List =>
               Ada.Text_IO.Put (F, ")");
            when LALCO.Ada_Pragma_Node_List =>
               Ada.Text_IO.Put (F, ")");
         end case;
      end if;
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
         % if debug:
         Load_Constructor_Visited;
         % endif
         Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Out_File_Name);
         Export_Ast_To_Rascal (F           => F,
                              N            => Unit.Root,
                              Indent       => 0,
                              Pretty_Print => Pretty_Print);
         Ada.Text_IO.Close (F);
         % if debug:
         Save_Constructor_Visited;
         % endif
      end if;
   end Export;


   procedure Ada_Func_Wrapper (ada_file : Interfaces.C.Strings.Chars_Ptr;
                              out_file : Interfaces.C.Strings.Chars_Ptr) is
   begin
      Export (Interfaces.C.Strings.Value (ada_file), Interfaces.C.Strings.Value (out_file), False);
   end Ada_Func_Wrapper;

end Export_Ast;