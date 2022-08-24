-- Copyright (c) 2022, TNO (ESI) and NWO-I Centrum Wiskunde & Informatica (CWI)
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
--     modification, are permitted provided that the following conditions
--     are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
-- notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
-- WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
-- OF SUCH DAMAGE.
-- 
-- Authors:
-- Jurgen J. Vinju - Centrum Wiskunde & Informatica
-- Damien De Campos - TNO ESI
-- Pierre van de Laar - TNO ESI


<%
def get_annotation(n):
   annotations = []
   for annotation, nodes in RascalContext.m3_annotation.items():
      if n in nodes:
         annotations.append(annotation)
   annotation_functions = []
   for annotation in annotations:
      annotation_functions.append(f"M3.Analysis.Get_{annotation}_Annotation (N.As_{n.public_type.api_name.camel_with_underscores})")
   return annotation_functions
%>\
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Libadalang.Analysis;
with Libadalang.Common;
% if debug:
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Characters.Wide_Wide_Latin_1;
% endif
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
with Strings_Utils;
with Export.Context;
with Ada.Wide_Wide_Characters.Handling;
with M3.Analysis;
% if debug:
with Export.Debug;
% endif
with Libadalang.Helpers;
with GNATCOLL.Projects;
with M3.URI_Utils;
with Ada.Strings.Unbounded;
with Ada.Characters.Conversions;
with Libadalang.Project_Provider;

package body Export.Ast is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   Skip_Index : constant := 8; -- Skipping "Ada_Op_"

   % if debug:
   Constructors_Used : Debug.Constructors_Used_Array;
   Tab : constant Wide_Wide_String := "   ";
   % endif

   procedure Export_Ast_To_Rascal (Print_Context : Context.Print_Context_Record_Type;
                                   Type_Context  : Context.Type_Context_Record_Type) is
   % if debug:
      use Ada.Characters.Wide_Wide_Latin_1;
      use Ada.Strings.Wide_Wide_Unbounded;
      use Ada.Strings.Wide_Wide_Fixed;
   % endif
      N : LAL.Ada_Node renames Type_Context.N;
      F : Ada.Wide_Wide_Text_IO.File_Type renames Print_Context.File.all;

      Is_Root : constant Boolean := not N.Is_Null and then N.Parent.Is_Null;
      % if debug:
      Prefix : constant Unbounded_Wide_Wide_String := To_Unbounded_Wide_Wide_String (LF & (Print_Context.Indent * Tab));
      % endif
      Opt : constant Wide_Wide_String := (if Type_Context.Is_Optional then "[" else "");
      End_Opt : constant Wide_Wide_String := (if Type_Context.Is_Optional then "]" else "");

   begin
      if N.Is_Null then
      % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
      % endif
         Ada.Wide_Wide_Text_IO.Put (F, "[]");
         return;
      end if;
      
      if Is_Root then
         if N.Kind not in Context.Entry_Point_Enum_Type then
            raise Program_Error with N.Kind_Name & "isn't an entry point";
         else
            case Context.Entry_Point_Enum_Type (N.Kind) is
               when LALCO.Ada_Compilation_Unit =>
                  Ada.Wide_Wide_Text_IO.Put (F, "Compilation_Units_Kind([");
               when LALCO.Ada_Compilation_Unit_List =>
                  Ada.Wide_Wide_Text_IO.Put (F, "Compilation_Units_Kind(");
               when LALCO.Ada_Pragma_Node_List =>
                  Ada.Wide_Wide_Text_IO.Put (F, "Statements_Kind(");
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
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
               % endif
               % if RascalContext.can_uses_chained_constructors(n):
               <% """ Handling Maybe[Expr_Or_Assoc] 
                  Maybe[assoc_kind(list[Assoc] As_Assoc)] => [assoc_kind([..., ..., ...])]
               """ %>\
            if Type_Context.Need_Chained_Constructor then
               Ada.Wide_Wide_Text_IO.Put (F, Opt);
               Ada.Wide_Wide_Text_IO.Put (F, "${RascalContext.get_chained_constructor(n)}(");
            end if;
               % endif
            Ada.Wide_Wide_Text_IO.Put (F, "[");
            for node of N.As_${n.public_type.api_name.camel_with_underscores} loop
               if not IsEmpty then
                  Ada.Wide_Wide_Text_IO.Put (F, ","); -- no list of maybe
               end if;
               % if RascalContext.can_uses_chained_constructors(n):
               Export_Ast_To_Rascal (Print_Context => Context.Add_Indent_Level (Print_Context),
                                     Type_Context => (N                      => Node.As_Ada_Node,
                                                    Is_Optional              => False,
                                                    Need_Chained_Constructor => False));
               % else:
               Export_Ast_To_Rascal (Print_Context => Context.Add_Indent_Level (Print_Context),
                                    Type_Context => (N                      => Node.As_Ada_Node,
                                                   Is_Optional              => False,
                                                   Need_Chained_Constructor => Type_Context.Need_Chained_Constructor));
               % endif
               IsEmpty := False;
            end loop;
            Ada.Wide_Wide_Text_IO.Put (F, "]");
               % if RascalContext.can_uses_chained_constructors(n):
            if Type_Context.Need_Chained_Constructor then
               Ada.Wide_Wide_Text_IO.Put (F, ",");
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, M3.Analysis.Get_Src_Annotation(n));
               Ada.Wide_Wide_Text_IO.Put (F, ")");
               Ada.Wide_Wide_Text_IO.Put (F, End_Opt);
            end if;
               % endif
         end;
            <% 
                  # Absent nodes
            %>\
            % elif n.public_type.api_name.lower.endswith("_absent"):
               % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
               % endif
         Ada.Wide_Wide_Text_IO.Put (F, "[]");  -- always Maybe
            <% 
                  # Present nodes
            %>\
            % elif n.public_type.api_name.lower.endswith("_present"):
               % if debug:          
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
               % endif
         Ada.Wide_Wide_Text_IO.Put (F, "[${n.base.public_type.api_name.lower}(");
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, M3.Analysis.Get_Src_Annotation (N));
               % for f in get_annotation(n):
                  % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
                  % endif
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, ${f});
               % endfor
         Ada.Wide_Wide_Text_IO.Put (F, ")]"); --  always Maybe
            <% 
                  # Inlined nodes 
            %>\
            % elif n.public_type.api_name.lower in inlined_prefix_nodes:

               % if debug: 
         Constructors_Used (N.As_${n.public_type.api_name.camel_with_underscores}.F_Op.Kind) := True;
               % endif         
                  % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
                  % endif
         Ada.Wide_Wide_Text_IO.Put (F, Opt);
                 % if RascalContext.can_uses_chained_constructors(n):
         if Type_Context.Need_Chained_Constructor then
            Ada.Wide_Wide_Text_IO.Put (F, "${RascalContext.get_chained_constructor(n)}(");
         end if;
                  % endif
         Ada.Wide_Wide_Text_IO.Put (F, "${inlined_prefix_nodes[n.public_type.api_name.lower]}");
         declare
            op_full_name  : constant Wide_Wide_String := N.As_${n.public_type.api_name.camel_with_underscores}.F_Op.Kind'Wide_Wide_Image;
            use Ada.Wide_Wide_Characters.Handling;
         begin
            Ada.Wide_Wide_Text_IO.Put (F, To_Lower (op_full_name(Skip_Index .. op_full_name'Last)));
         end;
         Ada.Wide_Wide_Text_IO.Put (F, "(");
                  % for field in n.get_parse_fields(include_inherited=True):
                     % if field.api_name.lower != "f_op":
         Export_Ast_To_Rascal (Print_Context => Context.Add_Indent_Level (Print_Context),
                              Type_Context => (N                      => N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}.As_Ada_Node,
                                             Is_Optional              => ${field.is_optional},
                                             Need_Chained_Constructor => False));
         Ada.Wide_Wide_Text_IO.Put (F, ",");
                     % endif
                  % endfor
                  % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
                  % endif
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, M3.Analysis.Get_Src_Annotation (N));
                 % if RascalContext.can_uses_chained_constructors(n):
         if Type_Context.Need_Chained_Constructor then
            Ada.Wide_Wide_Text_IO.Put (F, "),");
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, M3.Analysis.Get_Src_Annotation (N));
         end if;
               % endif
               % for f in get_annotation(n):
                  % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
                  % endif
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, ${f});
               % endfor
         Ada.Wide_Wide_Text_IO.Put (F, ")");
         Ada.Wide_Wide_Text_IO.Put (F, End_Opt);

            <% 
                  # All others nodes
            %>\
            % else:

               % if debug:
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
               % endif
            Ada.Wide_Wide_Text_IO.Put (F, Opt);
               % if RascalContext.can_uses_chained_constructors(n):
            if Type_Context.Need_Chained_Constructor then
               Ada.Wide_Wide_Text_IO.Put (F, "${RascalContext.get_chained_constructor(n)}(");
            end if;
               % endif
            Ada.Wide_Wide_Text_IO.Put (F, "${n.public_type.api_name.lower}(");
               % if n.is_token_node:
                  % if debug:
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
            Ada.Wide_Wide_Text_IO.Put (F, Tab);
                  % endif
            Ada.Wide_Wide_Text_IO.Put (F, Strings_Utils.Format_String (N.Text));
            Ada.Wide_Wide_Text_IO.Put (F,  ",");
               % endif
               % for field in n.get_parse_fields(include_inherited=True):                               
            Export_Ast_To_Rascal (Print_Context => Context.Add_Indent_Level (Print_Context),
                                 Type_Context => (N                      => N.As_${n.public_type.api_name.camel_with_underscores}.${field.api_name.camel_with_underscores}.As_Ada_Node,
                                                Is_Optional              => ${field.is_optional},
                                                Need_Chained_Constructor => ${field in RascalContext.field_with_chained_constructor}));
            Ada.Wide_Wide_Text_IO.Put (F, ",");
               % endfor               
               % if debug:
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
               % endif
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, M3.Analysis.Get_Src_Annotation (N));
               % for f in get_annotation(n):
                  % if debug:
         Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, Prefix);
                  % endif               
            Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, ${f});
               % endfor
               % if RascalContext.can_uses_chained_constructors(n):
            if Type_Context.Need_Chained_Constructor then
               Ada.Wide_Wide_Text_IO.Put (F, "),");
               Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put (F, M3.Analysis.Get_Src_Annotation (N));
            end if;
               % endif
            Ada.Wide_Wide_Text_IO.Put (F, ")");
            Ada.Wide_Wide_Text_IO.Put (F, End_Opt);            
            % endif
         % endif
      % endfor
      end case;

      if Is_Root then
         case Context.Entry_Point_Enum_Type (N.Kind) is
            when LALCO.Ada_Compilation_Unit =>
               Ada.Wide_Wide_Text_IO.Put (F, "])");
            when LALCO.Ada_Compilation_Unit_List =>
               Ada.Wide_Wide_Text_IO.Put (F, ")");
            when LALCO.Ada_Pragma_Node_List =>
               Ada.Wide_Wide_Text_IO.Put (F, ")");
         end case;
      end if;
   end Export_Ast_To_Rascal;


   procedure Export_Project (Project_File_Name : String; Out_File_Name : String) is
      Project : GNATCOLL.Projects.Project_Tree_Access;
      Env     : GNATCOLL.Projects.Project_Environment_Access;
      Units    : Libadalang.Helpers.String_Vectors.Vector;
      F       : aliased Ada.Wide_Wide_Text_IO.File_Type;
      First   : Boolean := True; 
   begin
      Libadalang.Helpers.Load_Project (Project_File => Project_File_Name, Project => Project, Env => Env);
      declare
         Unit_Provider : constant LAL.Unit_Provider_Reference := Libadalang.Project_Provider.Create_Project_Unit_Provider (Project, Project.Root_Project, Env);
         Context : constant LAL.Analysis_Context := LAL.Create_Context (Unit_Provider => Unit_Provider); 
      begin
         Libadalang.Helpers.List_Sources_From_Project (Project.all, False, Units);
         % if debug:
         Constructors_Used := Debug.Load_Constructors_Used;
         % endif
         Ada.Wide_Wide_Text_IO.Create (F, Ada.Wide_Wide_Text_IO.Out_File, Out_File_Name);
         Ada.Wide_Wide_Text_IO.Put (F, "(");
         for Unit_Name of Units loop
            declare
               Unit : constant LAL.Analysis_Unit := LAL.Get_From_File (Context => Context, FileName => Ada.Strings.Unbounded.To_String (Unit_Name));
            begin
               if not First then
                  Ada.Wide_Wide_Text_IO.Put (F,",");
               end if;
               First := False;
            
               if Unit.Has_Diagnostics then
                  for D of Unit.Diagnostics loop
                     Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Unit.Format_GNU_Diagnostic (D));
                     Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
                  end loop;
                  raise Constraint_Error with Ada.Strings.Unbounded.To_String (Unit_Name) & " has diagnostics";
               else
              
                  Ada.Wide_Wide_Text_IO.Put (F,M3.URI_Utils.Create_URI ("file",  Strings_Utils.Replace
                                             (Ada.Characters.Conversions.To_Wide_Wide_String
                                                (Unit.Get_Filename),
                                                "\", "/"))  & ":");
                  Export_Ast_To_Rascal (Print_Context => (File => F'Unchecked_Access,
                                                          Indent => 0),
                                        Type_Context => (N => Unit.Root,
                                                         Is_Optional => False,
                                                         Need_Chained_Constructor => False));
              
              
               end if;
            end;
         end loop;
         % if debug:
         Debug.Save_Constructors_Used (Constructors_Used);
         % endif
         Ada.Wide_Wide_Text_IO.Put (F, ")");
         Ada.Wide_Wide_Text_IO.Close (F);
      end;
   end Export_Project;


   
   procedure Export_File (File_Name : String; Out_File_Name : String) is
      Context : constant LAL.Analysis_Context := LAL.Create_Context;
      Unit    : constant LAL.Analysis_Unit := Context.Get_From_File (File_Name);
      F       : aliased Ada.Wide_Wide_Text_IO.File_Type;
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Unit.Format_GNU_Diagnostic (D));
            Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
         end loop;
         raise Constraint_Error with File_Name & " has diagnostics";
      else
         % if debug:
         Constructors_Used := Debug.Load_Constructors_Used;
         % endif
         Ada.Wide_Wide_Text_IO.Create (F, Ada.Wide_Wide_Text_IO.Out_File, Out_File_Name);
         Export_Ast_To_Rascal (Print_Context => (File => F'Unchecked_Access,
                                                Indent => 0),
                              Type_Context => (N => Unit.Root,
                                             Is_Optional => False,
                                             Need_Chained_Constructor => False));
         Ada.Wide_Wide_Text_IO.Close (F);
         % if debug:
         Debug.Save_Constructors_Used (Constructors_Used);
         % endif
      end if;
   end Export_File;


end Export.Ast;
