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

procedure Main is
    package LAL renames Libadalang.Analysis;
    package LALCO renames Libadalang.Common;


    function To_Rascal_Sloc_Range (N : LAL.Ada_Node'Class) return String is
        use Ada.Strings.Unbounded;
        Rascal_Sloc : Unbounded_String := To_Unbounded_String (Langkit_Support.Text.Image (Langkit_Support.Slocs.Image (N.Sloc_Range)));
        Hyphen_Index : Positive := Index (Rascal_Sloc, "-");
        FileName : constant String := GNATCOLL.Utils.Replace (N.Unit.Get_Filename, "\", "/"); -- work-arround Rascal doesn't allow backslash
    begin
        GNATCOLL.Utils.Replace (S => Rascal_Sloc, Pattern => ":" , Replacement =>  ",");
        GNATCOLL.Utils.Replace (S => Rascal_Sloc, Pattern => "-" , Replacement =>  ",");
        Insert (Rascal_Sloc, Hyphen_Index+1, "<");
        Insert (Rascal_Sloc, Hyphen_Index, ">");
        Rascal_Sloc := "|file:///" & FileName & "|(0,1,<" & Rascal_Sloc & ">)";
        return To_String (Rascal_Sloc);
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
   
    function Export_AST_To_Rascal (N : LAL.Ada_Node'Class; Indent : Natural := 0; Pretty_Print : Boolean := True; IsOptional : Boolean := False) return String is
        use Ada.Strings.Fixed;
        use Ada.Characters.Latin_1;
        Tab : constant String := (if Pretty_Print then "|  " else "");
        Prefix : constant String := (if Pretty_Print then LF & (Indent * Tab) else "");
        Just : constant String := (if IsOptional then "just(" else "");
        End_Just : constant String := (if IsOptional then ")" else "");
        src : constant String := (if not N.Is_Null then "src=" & To_Rascal_Sloc_Range(N) else "");
    begin
        if N.Is_Null then
            return Prefix & "nothing()";
        end if;
    case N.Kind is
        when LALCO.Ada_Abort_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Abort_Present =>
            return Prefix & "just(abort_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Abstract_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Abstract_Present =>
            return Prefix & "just(abstract_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Ada_Node_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Ada_Node_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Abstract_State_Decl_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Abstract_State_Decl_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Alternatives_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Alternatives_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Constraint_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Constraint_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Decl_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Decl_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Stmt_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Stmt_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Aspect_Assoc_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Aspect_Assoc_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Base_Assoc_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Base_Assoc_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Assoc_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Assoc_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Basic_Decl_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Basic_Decl_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Case_Expr_Alternative_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Case_Expr_Alternative_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Case_Stmt_Alternative_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Case_Stmt_Alternative_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Compilation_Unit_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Compilation_Unit_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Contract_Case_Assoc_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Contract_Case_Assoc_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Defining_Name_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Defining_Name_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Discriminant_Spec_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Discriminant_Spec_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Elsif_Expr_Part_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Elsif_Expr_Part_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Elsif_Stmt_Part_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Elsif_Stmt_Part_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Enum_Literal_Decl_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Enum_Literal_Decl_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Expr_Alternatives_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Expr_Alternatives_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Discriminant_Choice_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Discriminant_Choice_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Name_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Name_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Parent_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Parent_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Param_Spec_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Param_Spec_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Pragma_Node_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Pragma_Node_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Select_When_Part_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Select_When_Part_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Unconstrained_Array_Index_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Unconstrained_Array_Index_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Variant_List =>
            declare
                use Ada.Strings.Unbounded;
                s : Unbounded_String := To_Unbounded_String (Prefix & Just & "[");
                IsEmpty : Boolean := True;
            begin
                for node of N.As_Variant_List loop
                    Append (s, Export_AST_To_Rascal (node, Indent + 1, Pretty_Print, False) & ","); -- no list of maybe
                    IsEmpty := False;
                end loop;
                if not IsEmpty then
                    Replace_Element (s, Length(s), ' ');
                    Append (s, Prefix & "]" & End_Just);
                else
                    Append (s, "]" & End_Just);
                end if;
                return To_String (s);
            end;
        when LALCO.Ada_Aliased_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Aliased_Present =>
            return Prefix & "just(aliased_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_All_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_All_Present =>
            return Prefix & "just(all_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Constrained_Array_Indices =>
            return Prefix & Just & "constrained_array_indices (" &
                   Export_AST_To_Rascal (N.As_Constrained_Array_Indices.F_List, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Unconstrained_Array_Indices =>
            return Prefix & Just & "unconstrained_array_indices (" &
                   Export_AST_To_Rascal (N.As_Unconstrained_Array_Indices.F_Types, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Aspect_Assoc =>
            return Prefix & Just & "aspect_assoc (" &
                   Export_AST_To_Rascal (N.As_Aspect_Assoc.F_Id, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Aspect_Assoc.F_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_At_Clause =>
            return Prefix & Just & "at_clause (" &
                   Export_AST_To_Rascal (N.As_At_Clause.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_At_Clause.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Attribute_Def_Clause =>
            return Prefix & Just & "attribute_def_clause (" &
                   Export_AST_To_Rascal (N.As_Attribute_Def_Clause.F_Attribute_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Attribute_Def_Clause.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Enum_Rep_Clause =>
            return Prefix & Just & "enum_rep_clause (" &
                   Export_AST_To_Rascal (N.As_Enum_Rep_Clause.F_Type_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Enum_Rep_Clause.F_Aggregate, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Record_Rep_Clause =>
            return Prefix & Just & "record_rep_clause (" &
                   Export_AST_To_Rascal (N.As_Record_Rep_Clause.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Record_Rep_Clause.F_At_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Record_Rep_Clause.F_Components, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Aspect_Spec =>
            return Prefix & Just & "aspect_spec (" &
                   Export_AST_To_Rascal (N.As_Aspect_Spec.F_Aspect_Assocs, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Contract_Case_Assoc =>
            return Prefix & Just & "contract_case_assoc (" &
                   Export_AST_To_Rascal (N.As_Contract_Case_Assoc.F_Guard, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Contract_Case_Assoc.F_Consequence, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Pragma_Argument_Assoc =>
            return Prefix & Just & "pragma_argument_assoc (" &
                   Export_AST_To_Rascal (N.As_Pragma_Argument_Assoc.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Pragma_Argument_Assoc.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Entry_Spec =>
            return Prefix & Just & "entry_spec (" &
                   Export_AST_To_Rascal (N.As_Entry_Spec.F_Entry_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Spec.F_Family_Type, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Spec.F_Entry_Params, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Enum_Subp_Spec =>
            return Prefix & Just & "enum_subp_spec (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subp_Spec =>
            return Prefix & Just & "subp_spec (" &
                   Export_AST_To_Rascal (N.As_Subp_Spec.F_Subp_Kind, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Spec.F_Subp_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Spec.F_Subp_Params, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Spec.F_Subp_Returns, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Component_List =>
            return Prefix & Just & "component_list (" &
                   Export_AST_To_Rascal (N.As_Component_List.F_Components, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Component_List.F_Variant_Part, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Known_Discriminant_Part =>
            return Prefix & Just & "known_discriminant_part (" &
                   Export_AST_To_Rascal (N.As_Known_Discriminant_Part.F_Discr_Specs, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Unknown_Discriminant_Part =>
            return Prefix & Just & "unknown_discriminant_part (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Entry_Completion_Formal_Params =>
            return Prefix & Just & "entry_completion_formal_params (" &
                   Export_AST_To_Rascal (N.As_Entry_Completion_Formal_Params.F_Params, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Formal_Part =>
            return Prefix & Just & "generic_formal_part (" &
                   Export_AST_To_Rascal (N.As_Generic_Formal_Part.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Null_Record_Def =>
            return Prefix & Just & "null_record_def (" &
                   Export_AST_To_Rascal (N.As_Null_Record_Def.F_Components, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Record_Def =>
            return Prefix & Just & "record_def (" &
                   Export_AST_To_Rascal (N.As_Record_Def.F_Components, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Aggregate_Assoc =>
            return Prefix & Just & "aggregate_assoc (" &
                   Export_AST_To_Rascal (N.As_Aggregate_Assoc.F_Designators, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Aggregate_Assoc.F_R_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Multi_Dim_Array_Assoc =>
            return Prefix & Just & "multi_dim_array_assoc (" &
                   Export_AST_To_Rascal (N.As_Multi_Dim_Array_Assoc.F_Designators, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Multi_Dim_Array_Assoc.F_R_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Discriminant_Assoc =>
            return Prefix & Just & "discriminant_assoc (" &
                   Export_AST_To_Rascal (N.As_Discriminant_Assoc.F_Ids, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Discriminant_Assoc.F_Discr_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Iterated_Assoc =>
            return Prefix & Just & "iterated_assoc (" &
                   Export_AST_To_Rascal (N.As_Iterated_Assoc.F_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Iterated_Assoc.F_R_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Param_Assoc =>
            return Prefix & Just & "param_assoc (" &
                   Export_AST_To_Rascal (N.As_Param_Assoc.F_Designator, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Param_Assoc.F_R_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Abstract_State_Decl =>
            return Prefix & Just & "abstract_state_decl (" &
                   Export_AST_To_Rascal (N.As_Abstract_State_Decl.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Abstract_State_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Anonymous_Expr_Decl =>
            return Prefix & Just & "anonymous_expr_decl (" &
                   Export_AST_To_Rascal (N.As_Anonymous_Expr_Decl.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Anonymous_Expr_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Component_Decl =>
            return Prefix & Just & "component_decl (" &
                   Export_AST_To_Rascal (N.As_Component_Decl.F_Ids, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Component_Decl.F_Component_Def, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Component_Decl.F_Default_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Component_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Discriminant_Spec =>
            return Prefix & Just & "discriminant_spec (" &
                   Export_AST_To_Rascal (N.As_Discriminant_Spec.F_Ids, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Discriminant_Spec.F_Type_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Discriminant_Spec.F_Default_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Discriminant_Spec.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Formal_Obj_Decl =>
            return Prefix & Just & "generic_formal_obj_decl (" &
                   Export_AST_To_Rascal (N.As_Generic_Formal_Obj_Decl.F_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Formal_Obj_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Formal_Package =>
            return Prefix & Just & "generic_formal_package (" &
                   Export_AST_To_Rascal (N.As_Generic_Formal_Package.F_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Formal_Package.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Formal_Subp_Decl =>
            return Prefix & Just & "generic_formal_subp_decl (" &
                   Export_AST_To_Rascal (N.As_Generic_Formal_Subp_Decl.F_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Formal_Subp_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Formal_Type_Decl =>
            return Prefix & Just & "generic_formal_type_decl (" &
                   Export_AST_To_Rascal (N.As_Generic_Formal_Type_Decl.F_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Formal_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Param_Spec =>
            return Prefix & Just & "param_spec (" &
                   Export_AST_To_Rascal (N.As_Param_Spec.F_Ids, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Param_Spec.F_Has_Aliased, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Param_Spec.F_Mode, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Param_Spec.F_Type_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Param_Spec.F_Default_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Param_Spec.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Package_Internal =>
            return Prefix & Just & "generic_package_internal (" &
                   Export_AST_To_Rascal (N.As_Generic_Package_Internal.F_Package_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Internal.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Internal.F_Public_Part, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Internal.F_Private_Part, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Internal.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Package_Decl =>
            return Prefix & Just & "package_decl (" &
                   Export_AST_To_Rascal (N.As_Package_Decl.F_Package_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Decl.F_Public_Part, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Decl.F_Private_Part, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Decl.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Discrete_Base_Subtype_Decl =>
            return Prefix & Just & "discrete_base_subtype_decl (" &
                   Export_AST_To_Rascal (N.As_Discrete_Base_Subtype_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Discrete_Base_Subtype_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subtype_Decl =>
            return Prefix & Just & "subtype_decl (" &
                   Export_AST_To_Rascal (N.As_Subtype_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Subtype_Decl.F_Subtype, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subtype_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Classwide_Type_Decl =>
            return Prefix & Just & "classwide_type_decl (" &
                   Export_AST_To_Rascal (N.As_Classwide_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Classwide_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Incomplete_Type_Decl =>
            return Prefix & Just & "incomplete_type_decl (" &
                   Export_AST_To_Rascal (N.As_Incomplete_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Incomplete_Type_Decl.F_Discriminants, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Incomplete_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Incomplete_Tagged_Type_Decl =>
            return Prefix & Just & "incomplete_tagged_type_decl (" &
                   Export_AST_To_Rascal (N.As_Incomplete_Tagged_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Incomplete_Tagged_Type_Decl.F_Discriminants, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Incomplete_Tagged_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Incomplete_Tagged_Type_Decl.F_Has_Abstract, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Protected_Type_Decl =>
            return Prefix & Just & "protected_type_decl (" &
                   Export_AST_To_Rascal (N.As_Protected_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Type_Decl.F_Discriminants, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Type_Decl.F_Interfaces, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Type_Decl.F_Definition, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Task_Type_Decl =>
            return Prefix & Just & "task_type_decl (" &
                   Export_AST_To_Rascal (N.As_Task_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Type_Decl.F_Discriminants, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Type_Decl.F_Definition, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Single_Task_Type_Decl =>
            return Prefix & Just & "single_task_type_decl (" &
                   Export_AST_To_Rascal (N.As_Single_Task_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Single_Task_Type_Decl.F_Discriminants, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Single_Task_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Single_Task_Type_Decl.F_Definition, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Type_Decl =>
            return Prefix & Just & "type_decl (" &
                   Export_AST_To_Rascal (N.As_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Type_Decl.F_Discriminants, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Type_Decl.F_Type_Def, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Anonymous_Type_Decl =>
            return Prefix & Just & "anonymous_type_decl (" &
                   Export_AST_To_Rascal (N.As_Anonymous_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Anonymous_Type_Decl.F_Discriminants, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Anonymous_Type_Decl.F_Type_Def, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Anonymous_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Synth_Anonymous_Type_Decl =>
            return Prefix & Just & "synth_anonymous_type_decl (" &
                   Export_AST_To_Rascal (N.As_Synth_Anonymous_Type_Decl.F_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Synth_Anonymous_Type_Decl.F_Discriminants, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Synth_Anonymous_Type_Decl.F_Type_Def, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Synth_Anonymous_Type_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Abstract_Subp_Decl =>
            return Prefix & Just & "abstract_subp_decl (" &
                   Export_AST_To_Rascal (N.As_Abstract_Subp_Decl.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Abstract_Subp_Decl.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Abstract_Subp_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Abstract_Formal_Subp_Decl =>
            return Prefix & Just & "abstract_formal_subp_decl (" &
                   Export_AST_To_Rascal (N.As_Abstract_Formal_Subp_Decl.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Abstract_Formal_Subp_Decl.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Abstract_Formal_Subp_Decl.F_Default_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Abstract_Formal_Subp_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Concrete_Formal_Subp_Decl =>
            return Prefix & Just & "concrete_formal_subp_decl (" &
                   Export_AST_To_Rascal (N.As_Concrete_Formal_Subp_Decl.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Concrete_Formal_Subp_Decl.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Concrete_Formal_Subp_Decl.F_Default_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Concrete_Formal_Subp_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subp_Decl =>
            return Prefix & Just & "subp_decl (" &
                   Export_AST_To_Rascal (N.As_Subp_Decl.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Decl.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Entry_Decl =>
            return Prefix & Just & "entry_decl (" &
                   Export_AST_To_Rascal (N.As_Entry_Decl.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Decl.F_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Enum_Literal_Decl =>
            return Prefix & Just & "enum_literal_decl (" &
                   Export_AST_To_Rascal (N.As_Enum_Literal_Decl.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Enum_Literal_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Subp_Internal =>
            return Prefix & Just & "generic_subp_internal (" &
                   Export_AST_To_Rascal (N.As_Generic_Subp_Internal.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Internal.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Expr_Function =>
            return Prefix & Just & "expr_function (" &
                   Export_AST_To_Rascal (N.As_Expr_Function.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Expr_Function.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Expr_Function.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Expr_Function.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Null_Subp_Decl =>
            return Prefix & Just & "null_subp_decl (" &
                   Export_AST_To_Rascal (N.As_Null_Subp_Decl.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Null_Subp_Decl.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Null_Subp_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subp_Body =>
            return Prefix & Just & "subp_body (" &
                   Export_AST_To_Rascal (N.As_Subp_Body.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Body.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Body.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Body.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Body.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Body.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subp_Renaming_Decl =>
            return Prefix & Just & "subp_renaming_decl (" &
                   Export_AST_To_Rascal (N.As_Subp_Renaming_Decl.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Renaming_Decl.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Renaming_Decl.F_Renames, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Renaming_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Package_Body_Stub =>
            return Prefix & Just & "package_body_stub (" &
                   Export_AST_To_Rascal (N.As_Package_Body_Stub.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Body_Stub.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Protected_Body_Stub =>
            return Prefix & Just & "protected_body_stub (" &
                   Export_AST_To_Rascal (N.As_Protected_Body_Stub.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Body_Stub.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subp_Body_Stub =>
            return Prefix & Just & "subp_body_stub (" &
                   Export_AST_To_Rascal (N.As_Subp_Body_Stub.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Body_Stub.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subp_Body_Stub.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Task_Body_Stub =>
            return Prefix & Just & "task_body_stub (" &
                   Export_AST_To_Rascal (N.As_Task_Body_Stub.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Body_Stub.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Entry_Body =>
            return Prefix & Just & "entry_body (" &
                   Export_AST_To_Rascal (N.As_Entry_Body.F_Entry_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Body.F_Index_Spec, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Body.F_Params, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Body.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Body.F_Barrier, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Body.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Body.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Body.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Package_Body =>
            return Prefix & Just & "package_body (" &
                   Export_AST_To_Rascal (N.As_Package_Body.F_Package_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Body.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Body.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Body.F_Stmts, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Body.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Protected_Body =>
            return Prefix & Just & "protected_body (" &
                   Export_AST_To_Rascal (N.As_Protected_Body.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Body.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Body.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Body.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Task_Body =>
            return Prefix & Just & "task_body (" &
                   Export_AST_To_Rascal (N.As_Task_Body.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Body.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Body.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Body.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Body.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Entry_Index_Spec =>
            return Prefix & Just & "entry_index_spec (" &
                   Export_AST_To_Rascal (N.As_Entry_Index_Spec.F_Id, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Index_Spec.F_Subtype, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Entry_Index_Spec.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Error_Decl =>
            return Prefix & Just & "error_decl (" &
                   Export_AST_To_Rascal (N.As_Error_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Exception_Decl =>
            return Prefix & Just & "exception_decl (" &
                   Export_AST_To_Rascal (N.As_Exception_Decl.F_Ids, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Exception_Decl.F_Renames, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Exception_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Exception_Handler =>
            return Prefix & Just & "exception_handler (" &
                   Export_AST_To_Rascal (N.As_Exception_Handler.F_Exception_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Exception_Handler.F_Handled_Exceptions, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Exception_Handler.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Exception_Handler.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_For_Loop_Var_Decl =>
            return Prefix & Just & "for_loop_var_decl (" &
                   Export_AST_To_Rascal (N.As_For_Loop_Var_Decl.F_Id, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_For_Loop_Var_Decl.F_Id_Type, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_For_Loop_Var_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Package_Decl =>
            return Prefix & Just & "generic_package_decl (" &
                   Export_AST_To_Rascal (N.As_Generic_Package_Decl.F_Formal_Part, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Decl.F_Package_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Subp_Decl =>
            return Prefix & Just & "generic_subp_decl (" &
                   Export_AST_To_Rascal (N.As_Generic_Subp_Decl.F_Formal_Part, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Decl.F_Subp_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Package_Instantiation =>
            return Prefix & Just & "generic_package_instantiation (" &
                   Export_AST_To_Rascal (N.As_Generic_Package_Instantiation.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Instantiation.F_Generic_Pkg_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Instantiation.F_Params, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Instantiation.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Subp_Instantiation =>
            return Prefix & Just & "generic_subp_instantiation (" &
                   Export_AST_To_Rascal (N.As_Generic_Subp_Instantiation.F_Overriding, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Instantiation.F_Kind, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Instantiation.F_Subp_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Instantiation.F_Generic_Subp_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Instantiation.F_Params, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Instantiation.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Package_Renaming_Decl =>
            return Prefix & Just & "generic_package_renaming_decl (" &
                   Export_AST_To_Rascal (N.As_Generic_Package_Renaming_Decl.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Renaming_Decl.F_Renames, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Package_Renaming_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Generic_Subp_Renaming_Decl =>
            return Prefix & Just & "generic_subp_renaming_decl (" &
                   Export_AST_To_Rascal (N.As_Generic_Subp_Renaming_Decl.F_Kind, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Renaming_Decl.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Renaming_Decl.F_Renames, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Generic_Subp_Renaming_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Label_Decl =>
            return Prefix & Just & "label_decl (" &
                   Export_AST_To_Rascal (N.As_Label_Decl.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Label_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Named_Stmt_Decl =>
            return Prefix & Just & "named_stmt_decl (" &
                   Export_AST_To_Rascal (N.As_Named_Stmt_Decl.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Named_Stmt_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Number_Decl =>
            return Prefix & Just & "number_decl (" &
                   Export_AST_To_Rascal (N.As_Number_Decl.F_Ids, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Number_Decl.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Number_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Object_Decl =>
            return Prefix & Just & "object_decl (" &
                   Export_AST_To_Rascal (N.As_Object_Decl.F_Ids, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Object_Decl.F_Has_Aliased, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Object_Decl.F_Has_Constant, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Object_Decl.F_Mode, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Object_Decl.F_Type_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Object_Decl.F_Default_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Object_Decl.F_Renaming_Clause, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Object_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Extended_Return_Stmt_Object_Decl =>
            return Prefix & Just & "extended_return_stmt_object_decl (" &
                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt_Object_Decl.F_Ids, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt_Object_Decl.F_Has_Aliased, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt_Object_Decl.F_Has_Constant, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt_Object_Decl.F_Mode, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt_Object_Decl.F_Type_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt_Object_Decl.F_Default_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt_Object_Decl.F_Renaming_Clause, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt_Object_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Package_Renaming_Decl =>
            return Prefix & Just & "package_renaming_decl (" &
                   Export_AST_To_Rascal (N.As_Package_Renaming_Decl.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Renaming_Decl.F_Renames, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Package_Renaming_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Single_Protected_Decl =>
            return Prefix & Just & "single_protected_decl (" &
                   Export_AST_To_Rascal (N.As_Single_Protected_Decl.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Single_Protected_Decl.F_Aspects, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Single_Protected_Decl.F_Interfaces, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Single_Protected_Decl.F_Definition, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Single_Task_Decl =>
            return Prefix & Just & "single_task_decl (" &
                   Export_AST_To_Rascal (N.As_Single_Task_Decl.F_Task_Type, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Single_Task_Decl.F_Aspects, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Case_Stmt_Alternative =>
            return Prefix & Just & "case_stmt_alternative (" &
                   Export_AST_To_Rascal (N.As_Case_Stmt_Alternative.F_Choices, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Case_Stmt_Alternative.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Compilation_Unit =>
            return Prefix & Just & "compilation_unit (" &
                   Export_AST_To_Rascal (N.As_Compilation_Unit.F_Prelude, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Compilation_Unit.F_Body, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Compilation_Unit.F_Pragmas, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Component_Clause =>
            return Prefix & Just & "component_clause (" &
                   Export_AST_To_Rascal (N.As_Component_Clause.F_Id, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Component_Clause.F_Position, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Component_Clause.F_Range, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Component_Def =>
            return Prefix & Just & "component_def (" &
                   Export_AST_To_Rascal (N.As_Component_Def.F_Has_Aliased, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Component_Def.F_Has_Constant, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Component_Def.F_Type_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Constant_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Constant_Present =>
            return Prefix & "just(constant_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Delta_Constraint =>
            return Prefix & Just & "delta_constraint (" &
                   Export_AST_To_Rascal (N.As_Delta_Constraint.F_Digits, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Delta_Constraint.F_Range, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Digits_Constraint =>
            return Prefix & Just & "digits_constraint (" &
                   Export_AST_To_Rascal (N.As_Digits_Constraint.F_Digits, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Digits_Constraint.F_Range, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Discriminant_Constraint =>
            return Prefix & Just & "discriminant_constraint (" &
                   Export_AST_To_Rascal (N.As_Discriminant_Constraint.F_Constraints, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Index_Constraint =>
            return Prefix & Just & "index_constraint (" &
                   Export_AST_To_Rascal (N.As_Index_Constraint.F_Constraints, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Range_Constraint =>
            return Prefix & Just & "range_constraint (" &
                   Export_AST_To_Rascal (N.As_Range_Constraint.F_Range, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Declarative_Part =>
            return Prefix & Just & "declarative_part (" &
                   Export_AST_To_Rascal (N.As_Declarative_Part.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Private_Part =>
            return Prefix & Just & "private_part (" &
                   Export_AST_To_Rascal (N.As_Private_Part.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Public_Part =>
            return Prefix & Just & "public_part (" &
                   Export_AST_To_Rascal (N.As_Public_Part.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Elsif_Expr_Part =>
            return Prefix & Just & "elsif_expr_part (" &
                   Export_AST_To_Rascal (N.As_Elsif_Expr_Part.F_Cond_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Elsif_Expr_Part.F_Then_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Elsif_Stmt_Part =>
            return Prefix & Just & "elsif_stmt_part (" &
                   Export_AST_To_Rascal (N.As_Elsif_Stmt_Part.F_Cond_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Elsif_Stmt_Part.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Abstract_State_Decl_Expr =>
            return Prefix & Just & "abstract_state_decl_expr (" &
                   Export_AST_To_Rascal (N.As_Abstract_State_Decl_Expr.F_State_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Allocator =>
            return Prefix & Just & "allocator (" &
                   Export_AST_To_Rascal (N.As_Allocator.F_Subpool, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Allocator.F_Type_Or_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Aggregate =>
            return Prefix & Just & "aggregate (" &
                   Export_AST_To_Rascal (N.As_Aggregate.F_Ancestor_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Aggregate.F_Assocs, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Bracket_Aggregate =>
            return Prefix & Just & "bracket_aggregate (" &
                   Export_AST_To_Rascal (N.As_Bracket_Aggregate.F_Ancestor_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Bracket_Aggregate.F_Assocs, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Delta_Aggregate =>
            return Prefix & Just & "delta_aggregate (" &
                   Export_AST_To_Rascal (N.As_Delta_Aggregate.F_Ancestor_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Delta_Aggregate.F_Assocs, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Bracket_Delta_Aggregate =>
            return Prefix & Just & "bracket_delta_aggregate (" &
                   Export_AST_To_Rascal (N.As_Bracket_Delta_Aggregate.F_Ancestor_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Bracket_Delta_Aggregate.F_Assocs, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Null_Record_Aggregate =>
            return Prefix & Just & "null_record_aggregate (" &
                   Export_AST_To_Rascal (N.As_Null_Record_Aggregate.F_Ancestor_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Null_Record_Aggregate.F_Assocs, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Bin_Op =>
               declare
                    op_full_name  : constant String := N.As_Bin_Op.F_Op.Kind_Name;
                    op_name       : constant String := Lower_Name_With_Underscore (op_full_name(3..op_full_name'Last));
               begin
                    return Prefix & Just & op_name & "(" &                   Export_AST_To_Rascal (N.As_Bin_Op.F_Left, Indent + 1, Pretty_Print, False) & ", " &
                   Export_AST_To_Rascal (N.As_Bin_Op.F_Right, Indent + 1, Pretty_Print, False) & ", " &
                     Prefix & src & ")" & End_Just;
               end;

        when LALCO.Ada_Relation_Op =>
               declare
                    op_full_name  : constant String := N.As_Relation_Op.F_Op.Kind_Name;
                    op_name       : constant String := Lower_Name_With_Underscore (op_full_name(3..op_full_name'Last));
               begin
                    return Prefix & Just & op_name & "(" &                   Export_AST_To_Rascal (N.As_Relation_Op.F_Left, Indent + 1, Pretty_Print, False) & ", " &
                   Export_AST_To_Rascal (N.As_Relation_Op.F_Right, Indent + 1, Pretty_Print, False) & ", " &
                     Prefix & src & ")" & End_Just;
               end;

        when LALCO.Ada_Box_Expr =>
            return Prefix & Just & "box_expr (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Case_Expr_Alternative =>
            return Prefix & Just & "case_expr_alternative (" &
                   Export_AST_To_Rascal (N.As_Case_Expr_Alternative.F_Choices, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Case_Expr_Alternative.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Case_Expr =>
            return Prefix & Just & "case_expr (" &
                   Export_AST_To_Rascal (N.As_Case_Expr.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Case_Expr.F_Cases, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_If_Expr =>
            return Prefix & Just & "if_expr (" &
                   Export_AST_To_Rascal (N.As_If_Expr.F_Cond_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_If_Expr.F_Then_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_If_Expr.F_Alternatives, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_If_Expr.F_Else_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Contract_Cases =>
            return Prefix & Just & "contract_cases (" &
                   Export_AST_To_Rascal (N.As_Contract_Cases.F_Contract_Cases, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Decl_Expr =>
            return Prefix & Just & "decl_expr (" &
                   Export_AST_To_Rascal (N.As_Decl_Expr.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Decl_Expr.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Membership_Expr =>
               declare
                    op_full_name  : constant String := N.As_Membership_Expr.F_Op.Kind_Name;
                    op_name       : constant String := Lower_Name_With_Underscore (op_full_name(3..op_full_name'Last));
               begin
                    return Prefix & Just & op_name & "(" &                   Export_AST_To_Rascal (N.As_Membership_Expr.F_Expr, Indent + 1, Pretty_Print, False) & ", " &
                   Export_AST_To_Rascal (N.As_Membership_Expr.F_Membership_Exprs, Indent + 1, Pretty_Print, False) & ", " &
                     Prefix & src & ")" & End_Just;
               end;

        when LALCO.Ada_Attribute_Ref =>
            return Prefix & Just & "attribute_ref (" &
                   Export_AST_To_Rascal (N.As_Attribute_Ref.F_Prefix, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Attribute_Ref.F_Attribute, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Attribute_Ref.F_Args, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Update_Attribute_Ref =>
            return Prefix & Just & "update_attribute_ref (" &
                   Export_AST_To_Rascal (N.As_Update_Attribute_Ref.F_Prefix, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Update_Attribute_Ref.F_Attribute, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Update_Attribute_Ref.F_Args, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Call_Expr =>
            return Prefix & Just & "call_expr (" &
                   Export_AST_To_Rascal (N.As_Call_Expr.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Call_Expr.F_Suffix, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Defining_Name =>
            return Prefix & Just & "defining_name (" &
                   Export_AST_To_Rascal (N.As_Defining_Name.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Discrete_Subtype_Name =>
            return Prefix & Just & "discrete_subtype_name (" &
                   Export_AST_To_Rascal (N.As_Discrete_Subtype_Name.F_Subtype, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Dotted_Name =>
            return Prefix & Just & "dotted_name (" &
                   Export_AST_To_Rascal (N.As_Dotted_Name.F_Prefix, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Dotted_Name.F_Suffix, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_End_Name =>
            return Prefix & Just & "end_name (" &
                   Export_AST_To_Rascal (N.As_End_Name.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Explicit_Deref =>
            return Prefix & Just & "explicit_deref (" &
                   Export_AST_To_Rascal (N.As_Explicit_Deref.F_Prefix, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Qual_Expr =>
            return Prefix & Just & "qual_expr (" &
                   Export_AST_To_Rascal (N.As_Qual_Expr.F_Prefix, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Qual_Expr.F_Suffix, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Char_Literal =>
            return Prefix & Just & "char_literal (" &
                   Prefix & Tab & """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """" & ", " &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Identifier =>
            return Prefix & Just & "identifier (" &
                   Prefix & Tab & """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """" & ", " &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Abs =>
            return Prefix & Just & "op_abs (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_And =>
            return Prefix & Just & "op_and (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_And_Then =>
            return Prefix & Just & "op_and_then (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Concat =>
            return Prefix & Just & "op_concat (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Div =>
            return Prefix & Just & "op_div (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Double_Dot =>
            return Prefix & Just & "op_double_dot (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Eq =>
            return Prefix & Just & "op_eq (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Gt =>
            return Prefix & Just & "op_gt (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Gte =>
            return Prefix & Just & "op_gte (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_In =>
            return Prefix & Just & "op_in (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Lt =>
            return Prefix & Just & "op_lt (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Lte =>
            return Prefix & Just & "op_lte (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Minus =>
            return Prefix & Just & "op_minus (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Mod =>
            return Prefix & Just & "op_mod (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Mult =>
            return Prefix & Just & "op_mult (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Neq =>
            return Prefix & Just & "op_neq (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Not =>
            return Prefix & Just & "op_not (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Not_In =>
            return Prefix & Just & "op_not_in (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Or =>
            return Prefix & Just & "op_or (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Or_Else =>
            return Prefix & Just & "op_or_else (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Plus =>
            return Prefix & Just & "op_plus (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Pow =>
            return Prefix & Just & "op_pow (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Rem =>
            return Prefix & Just & "op_rem (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Op_Xor =>
            return Prefix & Just & "op_xor (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_String_Literal =>
            return Prefix & Just & "string_literal (" &
                   Prefix & Tab & """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """" & ", " &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Null_Literal =>
            return Prefix & Just & "null_literal (" &
                   Prefix & Tab & """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """" & ", " &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Int_Literal =>
            return Prefix & Just & "int_literal (" &
                   Prefix & Tab & """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """" & ", " &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Real_Literal =>
            return Prefix & Just & "real_literal (" &
                   Prefix & Tab & """" & Escape_Quotes (Langkit_Support.Text.Image (N.Text)) & """" & ", " &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Target_Name =>
            return Prefix & Just & "target_name (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Paren_Expr =>
            return Prefix & Just & "paren_expr (" &
                   Export_AST_To_Rascal (N.As_Paren_Expr.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Quantified_Expr =>
            return Prefix & Just & "quantified_expr (" &
                   Export_AST_To_Rascal (N.As_Quantified_Expr.F_Quantifier, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Quantified_Expr.F_Loop_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Quantified_Expr.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Raise_Expr =>
            return Prefix & Just & "raise_expr (" &
                   Export_AST_To_Rascal (N.As_Raise_Expr.F_Exception_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Raise_Expr.F_Error_Message, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Un_Op =>
               declare
                    op_full_name  : constant String := N.As_Un_Op.F_Op.Kind_Name;
                    op_name       : constant String := Lower_Name_With_Underscore (op_full_name(3..op_full_name'Last));
               begin
                    return Prefix & Just & op_name & "(" &                   Export_AST_To_Rascal (N.As_Un_Op.F_Expr, Indent + 1, Pretty_Print, False) & ", " &
                     Prefix & src & ")" & End_Just;
               end;

        when LALCO.Ada_Handled_Stmts =>
            return Prefix & Just & "handled_stmts (" &
                   Export_AST_To_Rascal (N.As_Handled_Stmts.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Handled_Stmts.F_Exceptions, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Interface_Kind_Limited =>
            return Prefix & Just & "interface_kind_limited (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Interface_Kind_Protected =>
            return Prefix & Just & "interface_kind_protected (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Interface_Kind_Synchronized =>
            return Prefix & Just & "interface_kind_synchronized (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Interface_Kind_Task =>
            return Prefix & Just & "interface_kind_task (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Iter_Type_In =>
            return Prefix & Just & "iter_type_in (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Iter_Type_Of =>
            return Prefix & Just & "iter_type_of (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Library_Item =>
            return Prefix & Just & "library_item (" &
                   Export_AST_To_Rascal (N.As_Library_Item.F_Has_Private, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Library_Item.F_Item, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Limited_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Limited_Present =>
            return Prefix & "just(limited_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_For_Loop_Spec =>
            return Prefix & Just & "for_loop_spec (" &
                   Export_AST_To_Rascal (N.As_For_Loop_Spec.F_Var_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_For_Loop_Spec.F_Loop_Type, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_For_Loop_Spec.F_Has_Reverse, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_For_Loop_Spec.F_Iter_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_While_Loop_Spec =>
            return Prefix & Just & "while_loop_spec (" &
                   Export_AST_To_Rascal (N.As_While_Loop_Spec.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Mode_Default =>
            return Prefix & Just & "mode_default (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Mode_In =>
            return Prefix & Just & "mode_in (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Mode_In_Out =>
            return Prefix & Just & "mode_in_out (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Mode_Out =>
            return Prefix & Just & "mode_out (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Multi_Abstract_State_Decl =>
            return Prefix & Just & "multi_abstract_state_decl (" &
                   Export_AST_To_Rascal (N.As_Multi_Abstract_State_Decl.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Not_Null_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Not_Null_Present =>
            return Prefix & "just(not_null(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Null_Component_Decl =>
            return Prefix & Just & "null_component_decl (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Others_Designator =>
            return Prefix & Just & "others_designator (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Overriding_Not_Overriding =>
            return Prefix & Just & "overriding_not_overriding (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Overriding_Overriding =>
            return Prefix & Just & "overriding_overriding (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Overriding_Unspecified =>
            return Prefix & Just & "overriding_unspecified (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Params =>
            return Prefix & Just & "params (" &
                   Export_AST_To_Rascal (N.As_Params.F_Params, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Paren_Abstract_State_Decl =>
            return Prefix & Just & "paren_abstract_state_decl (" &
                   Export_AST_To_Rascal (N.As_Paren_Abstract_State_Decl.F_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Pragma_Node =>
            return Prefix & Just & "pragma_node (" &
                   Export_AST_To_Rascal (N.As_Pragma_Node.F_Id, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Pragma_Node.F_Args, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Private_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Private_Present =>
            return Prefix & "just(private_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Protected_Def =>
            return Prefix & Just & "protected_def (" &
                   Export_AST_To_Rascal (N.As_Protected_Def.F_Public_Part, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Def.F_Private_Part, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Protected_Def.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Protected_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Protected_Present =>
            return Prefix & "just(protected_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Quantifier_All =>
            return Prefix & Just & "quantifier_all (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Quantifier_Some =>
            return Prefix & Just & "quantifier_some (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Range_Spec =>
            return Prefix & Just & "range_spec (" &
                   Export_AST_To_Rascal (N.As_Range_Spec.F_Range, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Renaming_Clause =>
            return Prefix & Just & "renaming_clause (" &
                   Export_AST_To_Rascal (N.As_Renaming_Clause.F_Renamed_Object, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Synthetic_Renaming_Clause =>
            return Prefix & Just & "synthetic_renaming_clause (" &
                   Export_AST_To_Rascal (N.As_Synthetic_Renaming_Clause.F_Renamed_Object, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Reverse_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Reverse_Present =>
            return Prefix & "just(reverse_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Select_When_Part =>
            return Prefix & Just & "select_when_part (" &
                   Export_AST_To_Rascal (N.As_Select_When_Part.F_Cond_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Select_When_Part.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Accept_Stmt =>
            return Prefix & Just & "accept_stmt (" &
                   Export_AST_To_Rascal (N.As_Accept_Stmt.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Accept_Stmt.F_Entry_Index_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Accept_Stmt.F_Params, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Accept_Stmt_With_Stmts =>
            return Prefix & Just & "accept_stmt_with_stmts (" &
                   Export_AST_To_Rascal (N.As_Accept_Stmt_With_Stmts.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Accept_Stmt_With_Stmts.F_Entry_Index_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Accept_Stmt_With_Stmts.F_Params, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Accept_Stmt_With_Stmts.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Accept_Stmt_With_Stmts.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_For_Loop_Stmt =>
            return Prefix & Just & "for_loop_stmt (" &
                   Export_AST_To_Rascal (N.As_For_Loop_Stmt.F_Spec, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_For_Loop_Stmt.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_For_Loop_Stmt.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Loop_Stmt =>
            return Prefix & Just & "loop_stmt (" &
                   Export_AST_To_Rascal (N.As_Loop_Stmt.F_Spec, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Loop_Stmt.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Loop_Stmt.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_While_Loop_Stmt =>
            return Prefix & Just & "while_loop_stmt (" &
                   Export_AST_To_Rascal (N.As_While_Loop_Stmt.F_Spec, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_While_Loop_Stmt.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_While_Loop_Stmt.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Begin_Block =>
            return Prefix & Just & "begin_block (" &
                   Export_AST_To_Rascal (N.As_Begin_Block.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Begin_Block.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Decl_Block =>
            return Prefix & Just & "decl_block (" &
                   Export_AST_To_Rascal (N.As_Decl_Block.F_Decls, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Decl_Block.F_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Decl_Block.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Case_Stmt =>
            return Prefix & Just & "case_stmt (" &
                   Export_AST_To_Rascal (N.As_Case_Stmt.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Case_Stmt.F_Alternatives, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Extended_Return_Stmt =>
            return Prefix & Just & "extended_return_stmt (" &
                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt.F_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Extended_Return_Stmt.F_Stmts, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_If_Stmt =>
            return Prefix & Just & "if_stmt (" &
                   Export_AST_To_Rascal (N.As_If_Stmt.F_Cond_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_If_Stmt.F_Then_Stmts, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_If_Stmt.F_Alternatives, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_If_Stmt.F_Else_Stmts, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Named_Stmt =>
            return Prefix & Just & "named_stmt (" &
                   Export_AST_To_Rascal (N.As_Named_Stmt.F_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Named_Stmt.F_Stmt, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Select_Stmt =>
            return Prefix & Just & "select_stmt (" &
                   Export_AST_To_Rascal (N.As_Select_Stmt.F_Guards, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Select_Stmt.F_Else_Stmts, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Select_Stmt.F_Abort_Stmts, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Error_Stmt =>
            return Prefix & Just & "error_stmt (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Abort_Stmt =>
            return Prefix & Just & "abort_stmt (" &
                   Export_AST_To_Rascal (N.As_Abort_Stmt.F_Names, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Assign_Stmt =>
            return Prefix & Just & "assign_stmt (" &
                   Export_AST_To_Rascal (N.As_Assign_Stmt.F_Dest, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Assign_Stmt.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Call_Stmt =>
            return Prefix & Just & "call_stmt (" &
                   Export_AST_To_Rascal (N.As_Call_Stmt.F_Call, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Delay_Stmt =>
            return Prefix & Just & "delay_stmt (" &
                   Export_AST_To_Rascal (N.As_Delay_Stmt.F_Has_Until, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Delay_Stmt.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Exit_Stmt =>
            return Prefix & Just & "exit_stmt (" &
                   Export_AST_To_Rascal (N.As_Exit_Stmt.F_Loop_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Exit_Stmt.F_Cond_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Goto_Stmt =>
            return Prefix & Just & "goto_stmt (" &
                   Export_AST_To_Rascal (N.As_Goto_Stmt.F_Label_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Label =>
            return Prefix & Just & "label (" &
                   Export_AST_To_Rascal (N.As_Label.F_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Null_Stmt =>
            return Prefix & Just & "null_stmt (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Raise_Stmt =>
            return Prefix & Just & "raise_stmt (" &
                   Export_AST_To_Rascal (N.As_Raise_Stmt.F_Exception_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Raise_Stmt.F_Error_Message, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Requeue_Stmt =>
            return Prefix & Just & "requeue_stmt (" &
                   Export_AST_To_Rascal (N.As_Requeue_Stmt.F_Call_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Requeue_Stmt.F_Has_Abort, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Return_Stmt =>
            return Prefix & Just & "return_stmt (" &
                   Export_AST_To_Rascal (N.As_Return_Stmt.F_Return_Expr, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Terminate_Alternative =>
            return Prefix & Just & "terminate_alternative (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subp_Kind_Function =>
            return Prefix & Just & "subp_kind_function (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subp_Kind_Procedure =>
            return Prefix & Just & "subp_kind_procedure (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subunit =>
            return Prefix & Just & "subunit (" &
                   Export_AST_To_Rascal (N.As_Subunit.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subunit.F_Body, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Synchronized_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Synchronized_Present =>
            return Prefix & "just(synchronized_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Tagged_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Tagged_Present =>
            return Prefix & "just(tagged_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Task_Def =>
            return Prefix & Just & "task_def (" &
                   Export_AST_To_Rascal (N.As_Task_Def.F_Interfaces, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Def.F_Public_Part, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Def.F_Private_Part, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Task_Def.F_End_Name, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Access_To_Subp_Def =>
            return Prefix & Just & "access_to_subp_def (" &
                   Export_AST_To_Rascal (N.As_Access_To_Subp_Def.F_Has_Not_Null, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Access_To_Subp_Def.F_Has_Protected, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Access_To_Subp_Def.F_Subp_Spec, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Anonymous_Type_Access_Def =>
            return Prefix & Just & "anonymous_type_access_def (" &
                   Export_AST_To_Rascal (N.As_Anonymous_Type_Access_Def.F_Has_Not_Null, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Anonymous_Type_Access_Def.F_Type_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Type_Access_Def =>
            return Prefix & Just & "type_access_def (" &
                   Export_AST_To_Rascal (N.As_Type_Access_Def.F_Has_Not_Null, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Type_Access_Def.F_Has_All, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Type_Access_Def.F_Has_Constant, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Type_Access_Def.F_Subtype_Indication, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Array_Type_Def =>
            return Prefix & Just & "array_type_def (" &
                   Export_AST_To_Rascal (N.As_Array_Type_Def.F_Indices, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Array_Type_Def.F_Component_Type, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Derived_Type_Def =>
            return Prefix & Just & "derived_type_def (" &
                   Export_AST_To_Rascal (N.As_Derived_Type_Def.F_Has_Abstract, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Derived_Type_Def.F_Has_Limited, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Derived_Type_Def.F_Has_Synchronized, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Derived_Type_Def.F_Subtype_Indication, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Derived_Type_Def.F_Interfaces, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Derived_Type_Def.F_Record_Extension, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Derived_Type_Def.F_Has_With_Private, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Enum_Type_Def =>
            return Prefix & Just & "enum_type_def (" &
                   Export_AST_To_Rascal (N.As_Enum_Type_Def.F_Enum_Literals, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Formal_Discrete_Type_Def =>
            return Prefix & Just & "formal_discrete_type_def (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Interface_Type_Def =>
            return Prefix & Just & "interface_type_def (" &
                   Export_AST_To_Rascal (N.As_Interface_Type_Def.F_Interface_Kind, Indent + 1, Pretty_Print, True) & ", " &

                   Export_AST_To_Rascal (N.As_Interface_Type_Def.F_Interfaces, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Mod_Int_Type_Def =>
            return Prefix & Just & "mod_int_type_def (" &
                   Export_AST_To_Rascal (N.As_Mod_Int_Type_Def.F_Expr, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Private_Type_Def =>
            return Prefix & Just & "private_type_def (" &
                   Export_AST_To_Rascal (N.As_Private_Type_Def.F_Has_Abstract, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Private_Type_Def.F_Has_Tagged, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Private_Type_Def.F_Has_Limited, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Decimal_Fixed_Point_Def =>
            return Prefix & Just & "decimal_fixed_point_def (" &
                   Export_AST_To_Rascal (N.As_Decimal_Fixed_Point_Def.F_Delta, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Decimal_Fixed_Point_Def.F_Digits, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Decimal_Fixed_Point_Def.F_Range, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Floating_Point_Def =>
            return Prefix & Just & "floating_point_def (" &
                   Export_AST_To_Rascal (N.As_Floating_Point_Def.F_Num_Digits, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Floating_Point_Def.F_Range, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Ordinary_Fixed_Point_Def =>
            return Prefix & Just & "ordinary_fixed_point_def (" &
                   Export_AST_To_Rascal (N.As_Ordinary_Fixed_Point_Def.F_Delta, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Ordinary_Fixed_Point_Def.F_Range, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Record_Type_Def =>
            return Prefix & Just & "record_type_def (" &
                   Export_AST_To_Rascal (N.As_Record_Type_Def.F_Has_Abstract, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Record_Type_Def.F_Has_Tagged, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Record_Type_Def.F_Has_Limited, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Record_Type_Def.F_Record_Def, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Signed_Int_Type_Def =>
            return Prefix & Just & "signed_int_type_def (" &
                   Export_AST_To_Rascal (N.As_Signed_Int_Type_Def.F_Range, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Anonymous_Type =>
            return Prefix & Just & "anonymous_type (" &
                   Export_AST_To_Rascal (N.As_Anonymous_Type.F_Type_Decl, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Enum_Lit_Synth_Type_Expr =>
            return Prefix & Just & "enum_lit_synth_type_expr (" &
                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Subtype_Indication =>
            return Prefix & Just & "subtype_indication (" &
                   Export_AST_To_Rascal (N.As_Subtype_Indication.F_Has_Not_Null, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subtype_Indication.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Subtype_Indication.F_Constraint, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Constrained_Subtype_Indication =>
            return Prefix & Just & "constrained_subtype_indication (" &
                   Export_AST_To_Rascal (N.As_Constrained_Subtype_Indication.F_Has_Not_Null, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Constrained_Subtype_Indication.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Constrained_Subtype_Indication.F_Constraint, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Discrete_Subtype_Indication =>
            return Prefix & Just & "discrete_subtype_indication (" &
                   Export_AST_To_Rascal (N.As_Discrete_Subtype_Indication.F_Has_Not_Null, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Discrete_Subtype_Indication.F_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Discrete_Subtype_Indication.F_Constraint, Indent + 1, Pretty_Print, True) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Unconstrained_Array_Index =>
            return Prefix & Just & "unconstrained_array_index (" &
                   Export_AST_To_Rascal (N.As_Unconstrained_Array_Index.F_Subtype_Indication, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Until_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_Until_Present =>
            return Prefix & "just(until_node(" & src & "))"; -- # always Maybe

        when LALCO.Ada_Use_Package_Clause =>
            return Prefix & Just & "use_package_clause (" &
                   Export_AST_To_Rascal (N.As_Use_Package_Clause.F_Packages, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Use_Type_Clause =>
            return Prefix & Just & "use_type_clause (" &
                   Export_AST_To_Rascal (N.As_Use_Type_Clause.F_Has_All, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Use_Type_Clause.F_Types, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Variant =>
            return Prefix & Just & "variant (" &
                   Export_AST_To_Rascal (N.As_Variant.F_Choices, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Variant.F_Components, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_Variant_Part =>
            return Prefix & Just & "variant_part (" &
                   Export_AST_To_Rascal (N.As_Variant_Part.F_Discr_Name, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_Variant_Part.F_Variant, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_With_Clause =>
            return Prefix & Just & "with_clause (" &
                   Export_AST_To_Rascal (N.As_With_Clause.F_Has_Limited, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_With_Clause.F_Has_Private, Indent + 1, Pretty_Print, False) & ", " &

                   Export_AST_To_Rascal (N.As_With_Clause.F_Packages, Indent + 1, Pretty_Print, False) & ", " &

                   Prefix & src & ")" & End_Just;

        when LALCO.Ada_With_Private_Absent =>
            return Prefix & "nothing()";  -- always Maybe

        when LALCO.Ada_With_Private_Present =>
            return Prefix & "just(with_private(" & src & "))"; -- # always Maybe

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