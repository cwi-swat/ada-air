<%
def get_decl(n):
   hasDecl = False
   props = [p.api_name.camel_with_underscores for p in n.get_properties(predicate=lambda p: p.is_public)]
   fun = None
   for key in RascalContext.decl_functions:
      if key in props:
         hasDecl = True
         fun = key
         break
   if not hasDecl:
      return None
   args = RascalContext.decl_functions[fun]
   call = None
   if len(args) > 0:
      call = "N.As_{}.{}({})".format(n.public_type.api_name.camel_with_underscores, fun, ",".join(args))
   else:
      call = f"N.As_{n.public_type.api_name.camel_with_underscores}.{fun}"

   return "(if not {}.Is_Null then \"decl=\" & Export_Tools.Get_Rascal_Physical_Location ({}) else \"\")".format(call,call)
%>\
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Libadalang.Analysis;
with Libadalang.Common;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
with Strings_Utils;
with Export_Tools;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Exceptions;
% if debug:
with Export_Debug_Tools;
% endif

package body Export_Ast is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   Skip_Index : constant := 8; -- Skipping "Ada_Op_"


   % if debug:
   Constructors_Used : Export_Debug_Tools.Constructors_Used_Array;
   Tab : constant Wide_Wide_String := "   ";
   % endif

   procedure Export_Ast_To_Rascal (Print_Context : Export_Tools.Print_Context_Record_Type;
                                   Type_Context  : Export_Tools.Type_Context_Record_Type) is

      use Ada.Characters.Wide_Wide_Latin_1;
      use Ada.Strings.Wide_Wide_Unbounded;
      use Ada.Strings.Wide_Wide_Fixed;

      N : constant LAL.Ada_Node := Type_Context.N;
      F : constant Export_Tools.File_Type_Access := Print_Context.File; 
      Is_Root : constant Boolean := not N.Is_Null and then N.Parent.Is_Null;
      % if debug:
      Prefix : constant Unbounded_Wide_Wide_String := To_Unbounded_Wide_Wide_String (LF & (Print_Context.Indent * Tab));
      % endif
      Opt : constant Wide_Wide_String := (if Type_Context.Is_Optional then "[" else "");
      End_Opt : constant Wide_Wide_String := (if Type_Context.Is_Optional then "]" else "");
      src : constant Unbounded_Wide_Wide_String := (if not N.Is_Null then To_Unbounded_Wide_Wide_String ("src=" & Export_Tools.Get_Rascal_Physical_Location(N)) 
                                                   else Null_Unbounded_Wide_Wide_String);
   begin
      if N.Is_Null then
      % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
      % endif
         Ada.Wide_Wide_Text_IO.Put (F.all, "[]");
         return;
      end if;
      
      if Is_Root then
         if N.Kind not in Export_Tools.Entry_Point_Enum_Type then
            raise Program_Error with N.Kind_Name & "isn't an entry point";
         else
            case Export_Tools.Entry_Point_Enum_Type (N.Kind) is
               when LALCO.Ada_Compilation_Unit =>
                  Ada.Wide_Wide_Text_IO.Put (F.all, "Compilation_Units_Kind([");
               when LALCO.Ada_Compilation_Unit_List =>
                  Ada.Wide_Wide_Text_IO.Put (F.all, "Compilation_Units_Kind(");
               when LALCO.Ada_Pragma_Node_List =>
                  Ada.Wide_Wide_Text_IO.Put (F.all, "Statements_Kind(");
            end case;
         end if;
      end if;

   case N.Kind is
      % for n in ctx.astnode_types:
         % if not n.abstract:
      when LALCO.${n.ada_kind_name} =>
            % if debug:
         Constructors_Used (LALCO.${n.ada_kind_name}) := True;
            % endif
            <% 
               # List nodes
            %>\
            % if n.is_list or n.is_root_list_type: # list can't be optional
         declare
               IsEmpty : Boolean := True;
         begin
               % if debug:
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
               % endif
               % if RascalContext.can_uses_chained_constructors(n):
               <% """ Handling list[Expr_Or_Assoc] 
                  assoc_kind(list[Assoc] As_Assoc) => [assoc_kind([..., ..., ...])]                      
               """ %>\
            if Type_Context.Need_Chained_Constructor then
               Ada.Wide_Wide_Text_IO.Put (F.all, "[");
               Ada.Wide_Wide_Text_IO.Put (F.all, "${RascalContext.get_chained_constructor(n)}(");
            end if;
               % endif
            Ada.Wide_Wide_Text_IO.Put (F.all, "[");
            for node of N.As_${n.public_type.api_name.camel_with_underscores} loop
               if not IsEmpty then
                  Ada.Wide_Wide_Text_IO.Put (F.all, ","); -- no list of maybe
               end if;
               % if RascalContext.can_uses_chained_constructors(n):
               Export_Ast_To_Rascal (Print_Context => Export_Tools.Add_Indent_Level (Print_Context),
                                     Type_Context => (N                      => Node.As_Ada_Node,
                                                    Is_Optional              => False,
                                                    Need_Chained_Constructor => False));
               % else:
               Export_Ast_To_Rascal (Print_Context => Export_Tools.Add_Indent_Level (Print_Context),
                                    Type_Context => (N                      => Node.As_Ada_Node,
                                                   Is_Optional              => False,
                                                   Need_Chained_Constructor => Type_Context.Need_Chained_Constructor));
               % endif
               IsEmpty := False;
            end loop;
            Ada.Wide_Wide_Text_IO.Put (F.all, "]");
               % if RascalContext.can_uses_chained_constructors(n):
            if Type_Context.Need_Chained_Constructor then
               Ada.Wide_Wide_Text_IO.Put (F.all, ",");
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, src);
               Ada.Wide_Wide_Text_IO.Put (F.all, ")");
               Ada.Wide_Wide_Text_IO.Put (F.all, "]");
            end if;
               % endif
         end;
            <% 
                  # Absent nodes
            %>\
            % elif n.public_type.api_name.lower.endswith("_absent"):
               % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
               % endif
         Ada.Wide_Wide_Text_IO.Put (F.all, "[]");  -- always Maybe
            <% 
                  # Present nodes
            %>\
            % elif n.public_type.api_name.lower.endswith("_present"):
               % if debug:          
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
               % endif
         Ada.Wide_Wide_Text_IO.Put (F.all, "[${n.base.public_type.api_name.lower}(");
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, src);
         Ada.Wide_Wide_Text_IO.Put (F.all, ")]"); --  always Maybe
            <% 
                  # Inlined nodes 
            %>\
            % elif n.public_type.api_name.lower in inlined_prefix_nodes:
         declare
               op_full_name  : constant Wide_Wide_String := N.As_${n.public_type.api_name.camel_with_underscores}.F_Op.Kind'Wide_Wide_Image;
               use Ada.Wide_Wide_Characters.Handling;
         begin
               % if debug: 
            Constructors_Used (N.As_${n.public_type.api_name.camel_with_underscores}.F_Op.Kind) := True;
               % endif         
                  % if debug:
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
                  % endif
            Ada.Wide_Wide_Text_IO.Put (F.all, Opt);
                 % if RascalContext.can_uses_chained_constructors(n):
            if Type_Context.Need_Chained_Constructor then
               Ada.Wide_Wide_Text_IO.Put (F.all, "${RascalContext.get_chained_constructor(n)}(");
            end if;
                  % endif
            Ada.Wide_Wide_Text_IO.Put (F.all, "${inlined_prefix_nodes[n.public_type.api_name.lower]}");
            Ada.Wide_Wide_Text_IO.Put (F.all, To_Lower (op_full_name(Skip_Index .. op_full_name'Last)));
            Ada.Wide_Wide_Text_IO.Put (F.all, "(");
                  % for field in n.get_parse_fields(include_inherited=True):
                     % if field.api_name.lower != "f_op":
            Export_Ast_To_Rascal (Print_Context => Export_Tools.Add_Indent_Level (Print_Context),
                                 Type_Context => (N                      => N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}.As_Ada_Node,
                                                Is_Optional              => ${field.is_optional},
                                                Need_Chained_Constructor => False));
            Ada.Wide_Wide_Text_IO.Put (F.all, ",");
                     % endif
                  % endfor
                  % if debug:
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
                  % endif
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, src);
                 % if RascalContext.can_uses_chained_constructors(n):
            if Type_Context.Need_Chained_Constructor then
               Ada.Wide_Wide_Text_IO.Put (F.all, "),");
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, src);
            end if;
               % endif
            Ada.Wide_Wide_Text_IO.Put (F.all, ")");
            Ada.Wide_Wide_Text_IO.Put (F.all, End_Opt);
         end;
            <% 
                  # All others nodes
            %>\
            % else:
               % if get_decl(n) is not None:
         declare
               decl : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
         begin
            begin
               decl := To_Unbounded_Wide_Wide_String (${get_decl(n)});
            exception
               -- Int_Literal has this property but it isn't implemented
               -- TODO find a better way to handle this
               when others =>
                  null;
            end;
               % endif
               % if debug:
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
               % endif
               Ada.Wide_Wide_Text_IO.Put (F.all, Opt);
               % if RascalContext.can_uses_chained_constructors(n):
               if Type_Context.Need_Chained_Constructor then
                  Ada.Wide_Wide_Text_IO.Put (F.all, "${RascalContext.get_chained_constructor(n)}(");
               end if;
               % endif
               Ada.Wide_Wide_Text_IO.Put (F.all, "${n.public_type.api_name.lower}(");
               % if n.is_token_node:
                  % if debug:
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
               Ada.Wide_Wide_Text_IO.Put (F.all, Tab);
                  % endif
               Ada.Wide_Wide_Text_IO.Put (F.all, Strings_Utils.Format_String (N.Text));
               Ada.Wide_Wide_Text_IO.Put (F.all,  ",");
               % endif
               % for field in n.get_parse_fields(include_inherited=True):                               
               Export_Ast_To_Rascal (Print_Context => Export_Tools.Add_Indent_Level (Print_Context),
                                    Type_Context => (N                      => N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}.As_Ada_Node,
                                                   Is_Optional              => ${field.is_optional},
                                                   Need_Chained_Constructor => ${field in RascalContext.field_with_chained_constructor}));
               Ada.Wide_Wide_Text_IO.Put (F.all, ",");
               % endfor               
               % if debug:
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, Prefix);
               % endif
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, src);
               % if get_decl(n) is not None:
               Ada.Wide_Wide_Text_IO.Put (F.all, ",");
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, decl);
               % endif
               % if RascalContext.can_uses_chained_constructors(n):
               if Type_Context.Need_Chained_Constructor then
                  Ada.Wide_Wide_Text_IO.Put (F.all, "),");
                  Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F.all, src);
               end if;
               % endif
               Ada.Wide_Wide_Text_IO.Put (F.all, ")");
               Ada.Wide_Wide_Text_IO.Put (F.all, End_Opt);            
               % if get_decl(n) is not None:
         end;
               % endif
            % endif
         % endif
      % endfor
      end case;

      if Is_Root then
         case Export_Tools.Entry_Point_Enum_Type (N.Kind) is
            when LALCO.Ada_Compilation_Unit =>
               Ada.Wide_Wide_Text_IO.Put (F.all, "])");
            when LALCO.Ada_Compilation_Unit_List =>
               Ada.Wide_Wide_Text_IO.Put (F.all, ")");
            when LALCO.Ada_Pragma_Node_List =>
               Ada.Wide_Wide_Text_IO.Put (F.all, ")");
         end case;
      end if;
   end Export_Ast_To_Rascal;

   procedure Export (File_Name : String; Out_File_Name : String) is
      Context : constant LAL.Analysis_Context := LAL.Create_Context;
      Unit    : constant LAL.Analysis_Unit := Context.Get_From_File (File_Name);
      F       : aliased Ada.Wide_Wide_Text_IO.File_Type;
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Unit.Format_GNU_Diagnostic (D));
            Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
         end loop;
      else
         % if debug:
         Constructors_Used := Export_Debug_Tools.Load_Constructors_Used;
         % endif
         Ada.Wide_Wide_Text_IO.Create (F, Ada.Wide_Wide_Text_IO.Out_File, Out_File_Name);
         Export_Ast_To_Rascal (Print_Context => (File => F'Unchecked_Access,
                                                Indent => 0),
                              Type_Context => (N => Unit.Root,
                                             Is_Optional => False,
                                             Need_Chained_Constructor => False));
         Ada.Wide_Wide_Text_IO.Close (F);
         % if debug:
         Export_Debug_Tools.Save_Constructors_Used (Constructors_Used);
         % endif
      end if;
   end Export;


   function Ada_Func_Wrapper (ada_file : Interfaces.C.Strings.Chars_Ptr;
                              out_file : Interfaces.C.Strings.Chars_Ptr) 
                              return Interfaces.C.Strings.Chars_Ptr is
   begin
      Export (Interfaces.C.Strings.Value (ada_file), Interfaces.C.Strings.Value (out_file));
      return Interfaces.C.Strings.Null_Ptr;
      exception
         when E : others =>
            return Interfaces.C.Strings.New_String (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E));
   end Ada_Func_Wrapper;

end Export_Ast;