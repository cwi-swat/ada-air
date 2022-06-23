with Ada.Text_IO;
with Libadalang.Analysis;
with Ada.Command_Line;
with Libadalang.Common;
with Langkit_Support.Text;
with Langkit_Support.Slocs;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with GNATCOLL.Utils;
with GNAT.Strings;
with Ada.Characters.Handling;
with Langkit_Support.token_data_handlers;

procedure Main is
    package LAL renames Libadalang.Analysis;
    package LALCO renames Libadalang.Common;


   function To_Rascal_Sloc_Range (N : LAL.Ada_Node'Class) return String is
      use Ada.Strings.Unbounded;
      use Langkit_Support.Slocs;
      LAL_Sloc : Source_Location_Range := N.Sloc_Range;
   begin
      LAL_Sloc.Start_Column := LAL_Sloc.Start_Column - 1;
      LAL_Sloc.End_Column := LAL_Sloc.End_Column - 1;
      declare
         
         Rascal_Sloc : Unbounded_String := To_Unbounded_String (Langkit_Support.Text.Image (Image (LAL_Sloc)));
         Hyphen_Index : Positive := Index (Rascal_Sloc, "-");
         FileName : constant String := GNATCOLL.Utils.Replace (N.Unit.Get_Filename, "\", "/"); -- work-arround Rascal doesn't allow backslash
         offset : constant Positive := LALCO.Raw_Data (N.Token_Start).Source_First;
         lenght : constant Positive := N.Unit.Text'Length;
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
   
    function Export_AST_To_Rascal (N : LAL.Ada_Node'Class; Indent : Natural := 0; Pretty_Print : Boolean := True; IsOptional : Boolean := False; Need_Chained_Constructor : Boolean := False) return String is
        use Ada.Strings.Fixed;
        use Ada.Characters.Latin_1;
        Tab : constant String := (if Pretty_Print then "|  " else "");
        Prefix : constant String := (if Pretty_Print then LF & (Indent * Tab) else "");
        Just : constant String := (if IsOptional then "{" else "");
        End_Just : constant String := (if IsOptional then "}" else "");
        src : constant String := (if not N.Is_Null then "src=" & To_Rascal_Sloc_Range(N) else "");
    begin
        if N.Is_Null then
            return Prefix & "{}";
        end if;
    case N.Kind is
        % for n in ctx.astnode_types:
            % if not n.abstract:
        when LALCO.${n.ada_kind_name} =>
                % if n.is_list or n.is_root_list_type:
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just);
                IsEmpty : Boolean := True;
            begin
                % if get_chained_constructor(n) is not None:
                if Need_Chained_Constructor then
                    Append (s, "${get_chained_constructor(n)}(");
                end if;
                % endif
                Append(s, "[");
                for node of N.As_${n.public_type.api_name.camel_with_underscores} loop
                % if get_chained_constructor(n) is not None:
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False, False) & ","); -- no list of maybe
                % else:
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False, Need_Chained_Constructor) & ","); -- no list of maybe
                % endif
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' '); -- removing last comma
                    Append (s, Prefix & "]");
                else
                    Append (s, "]");
                end if;
                % if get_chained_constructor(n) is not None:
                if Need_Chained_Constructor then
                    Append (s, "," & src & ")");
                end if;
                % endif
                Append(s, End_Just);
                return To_String (s);
            end;
                % elif n.public_type.api_name.lower.endswith("_absent"):
            return Prefix & "{}";  -- always Maybe

                % elif n.public_type.api_name.lower.endswith("_present"):          
            return Prefix & "{${n.base.public_type.api_name.lower}(" & src & ")}"; --  always Maybe

                % elif n.public_type.api_name.lower in inlined_prefix_nodes:
            declare
                op_full_name  : constant String := N.As_${n.public_type.api_name.camel_with_underscores}.F_Op.Kind_Name;
                op_name       : constant String := Lower_Name_With_Underscore (op_full_name(3..op_full_name'Last));
            begin
                return Prefix & Just & "${inlined_prefix_nodes[n.public_type.api_name.lower]}" & op_name & "(" &\
                    % for field in n.get_parse_fields(include_inherited=True):
                        % if field.api_name.lower != "f_op":
                Export_AST_To_Rascal (N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}, Indent + 1, Pretty_Print, ${field.is_optional}) & ", " &
                        % endif
                    % endfor
                Prefix & src & ")" & End_Just;
            end;

                % else:
                % if get_chained_constructor(n) is not None:
            if Need_Chained_Constructor then
                return Prefix & Just & "${get_chained_constructor(n)}" & "(" & "${n.public_type.api_name.lower} (" &
                    % if n.is_token_node:
                Prefix & Tab & """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """" & ", " &
                    % endif
                    % for field in n.get_parse_fields(include_inherited=True):                
                Export_AST_To_Rascal (N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}, Indent + 1, Pretty_Print, ${field.is_optional}, ${field in field_with_chained_constructor}) & ", " &
                    % endfor
                Prefix & src & ")," & src & ")" & End_Just;
            else
                % endif
                return Prefix & Just & "${n.public_type.api_name.lower} (" &
                    % if n.is_token_node:
                Prefix & Tab & """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """" & ", " &
                    % endif
                    % for field in n.get_parse_fields(include_inherited=True):
                Export_AST_To_Rascal (N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}, Indent + 1, Pretty_Print, ${field.is_optional}, ${field in field_with_chained_constructor}) & ", " &
                    % endfor
                Prefix & src & ")" & End_Just;
                    % if get_chained_constructor(n) is not None:
            end if;
                    % endif

                % endif
           % endif
        % endfor
        end case;
    end Export_AST_To_Rascal;

    Context : constant LAL.Analysis_Context := LAL.Create_Context;
    Unit    : constant LAL.Analysis_Unit := Context.Get_From_File (Ada.Command_Line.Argument (1));
    Out_File_Name : constant String := Ada.Command_Line.Argument (2);
    Pretty_Print : constant Boolean := Ada.Command_Line.Argument_Count > 2 and then Ada.Command_Line.Argument (3) = "-P";
    F       : Ada.Text_IO.File_Type;
begin
    if Unit.Has_Diagnostics then
        for D of Unit.Diagnostics loop
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Unit.Format_GNU_Diagnostic (D));
        end loop;
    else
        Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Out_File_Name);
        Ada.Text_IO.Put_Line (F,Export_AST_To_Rascal (N            => Unit.Root,
                                                    Indent       => 0,
                                                    Pretty_Print => Pretty_Print));

        Ada.Text_IO.Close (F);
    end if;
end main;