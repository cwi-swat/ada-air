with M3.Implementation;
with Libadalang.Common;

package body M3.Analysis is

   package LALCO renames Libadalang.Common;
   
   ------------------------
   -- Get_Src_Annotation --
   ------------------------

   function Get_Src_Annotation
     (N : LAL.Ada_Node'Class)
      return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      if not N.Is_Null then
         return To_Unbounded_Wide_Wide_String ("src=" & Implementation.Get_Rascal_Physical_Location (N));
      else
         return Null_Unbounded_Wide_Wide_String;
      end if;
   end Get_Src_Annotation;
   
   
   function Get_Use_Annotation (N : LAL.Name'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      case LALCO.Ada_Name'(N.Kind) is
         when LALCO.Ada_Qual_Expr -- F_Prefix will be handled
            | LALCO.Ada_Explicit_Deref  -- F_Prefix will be handled
            | LALCO.Ada_End_Name  -- F_Name Should be removed
            | LALCO.Ada_Discrete_Subtype_Name  -- F_Name from subtype indication will be handled
            | LALCO.Ada_Defining_Name  -- F_Name will be handled
            | LALCO.Ada_Call_Expr  -- F_Name will be handled
            | LALCO.Ada_Attribute_Ref -- can't reference a declaration
            | LALCO.Ada_Update_Attribute_Ref -- can't reference a declaration
            | LALCO.Ada_Num_Literal -- can't reference a declaration
            | LALCO.Ada_Null_Literal -- can't reference a declaration
            | LALCO.Ada_Char_Literal -- can't reference a declaration
            | LALCO.Ada_Op => -- can't reference a declaration
            return Null_Unbounded_Wide_Wide_String;
            
         when LALCO.Ada_Identifier
            | LALCO.Ada_String_Literal -- can reference function like "=", "+" etc..
            | LALCO.Ada_Dotted_Name
            | LALCO.Ada_Target_Name =>
            declare
               Ref_Decl : constant LAL.Refd_Decl := N.P_Failsafe_Referenced_Decl;
            begin
               case LAL.Kind (Ref_Decl) is
                  when LALCO.Precise => 
                     declare
                        Decl_Node : constant LAL.Basic_Decl'Class := LAL.Decl (Ref_Decl);
                        Def_Name : constant LAL.Defining_Name := Decl_Node.P_Defining_Name;
                     begin
                        if not Def_Name.Is_Null and then not Def_Name.F_Name.Is_Null  then
                           declare
                              S : constant Wide_Wide_String := Implementation.Get_Rascal_Logical_Location (Decl_Node, Def_Name.F_Name);
                           begin
                              if S'Length > 0 then
                                 return To_Unbounded_Wide_Wide_String (",use=" & S);
                              else
                                 raise M3_Error with "Referenced location null";
                              end if;
                           end;
                        else
                           raise M3_Error with "Def_Name null";
                        end if;
                     end;
                     
                  when LALCO.Noref
                     | LALCO.Imprecise
                     | LALCO.Error => 
                     return Null_Unbounded_Wide_Wide_String;
               end case;
            end;
      end case;
   end Get_Use_Annotation;
   
   function Get_Decl_Annotation (N : LAL.Basic_Decl'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      declare
         S : constant Wide_Wide_String :=  Implementation.Get_Rascal_Logical_Location (N);
      begin
         if S'Length > 0 then
            return  To_Unbounded_Wide_Wide_String (",decl=" & S);        
         end if;
      end;
      return Null_Unbounded_Wide_Wide_String;
   end Get_Decl_Annotation;
   
   
   function Get_Containment_Annotation (N : LAL.Ada_Node'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      declare
         -- TODO doesn't work for separate body
         -- TODO add tests
         Parent_Node : constant LAL.Basic_Decl := N.P_Parent_Basic_Decl;
         use type LAL.Analysis_Unit;
      begin
         if not Parent_Node.Is_Null and then not (Parent_Node.Unit = Parent_Node.P_Standard_Unit) then -- removing contained in Standard Package
            declare
               S : constant Wide_Wide_String := Implementation.Get_Rascal_Logical_Location (Parent_Node);
            begin
               if S'Length > 0 then
                  return To_Unbounded_Wide_Wide_String (",containment=" & S);
               end if;
            end;
         end if;
         return Null_Unbounded_Wide_Wide_String;
      end;
   end Get_Containment_Annotation;

end M3.Analysis;
