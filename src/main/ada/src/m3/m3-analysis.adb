--with Ada.Exceptions;
--with Ada.Characters.Conversions;
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
      -- Handling only one once End_Name names:
      -- EndName      <- Treated
      -- |f_name:
      -- |  Id: fac   <- Passed
      -- TODO use a kind subtype with skipped node kind
      if N.Parent.Kind in LALCO.Ada_End_Name or N.Kind in LALCO.Ada_Attribute_Ref or N.Kind in LALCO.Ada_Call_Expr or N.Parent.Kind in LALCO.Ada_Dotted_Name then
        return Null_Unbounded_Wide_Wide_String; 
      end if;
      case N.Kind is
         when LALCO.Ada_End_Name 
            | LALCO.Ada_Num_Literal
            | LALCO.Ada_Op =>
            return Null_Unbounded_Wide_Wide_String;
              
         when others =>
            
            declare
               Decl : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
               Decl_Node : constant LAL.Basic_Decl := N.P_Referenced_Decl (False); -- Need to use the faillsafe function
            begin
               if not Decl_Node.Is_Null then
                  declare
                     Name : constant LAL.Defining_Name := Decl_Node.P_Defining_Name;                     
                     S : constant Wide_Wide_String := (if Name.Is_Null then "" else Implementation.Get_Rascal_Logical_Location (Decl_Node, Name.F_Name));
                  begin
                     if S'Length > 0 then
                        Decl := To_Unbounded_Wide_Wide_String (",use=" & S);
                     else
                        null;
                        --Decl := To_Unbounded_Wide_Wide_String (",use=""Returned Empty for " & N.Kind'Wide_Wide_Image & """");
                     end if;
                  end;
               else
                  null; -- ada+unknow ?
                  --Decl := To_Unbounded_Wide_Wide_String (",use=""P_Referenced_Decl Empty for " & N.Kind'Wide_Wide_Image & """");
               end if;
               return Decl;
            exception
               when others => raise Constraint_Error with "OHOHOHOHOH";
            end;
      end case;
   exception
      when others =>         
         N.Parent.Print;
         --raise;
         raise Constraint_Error with "OHOHOH";
         --return Null_Unbounded_Wide_Wide_String;
         --return To_Unbounded_Wide_Wide_String (",use=""Failed " & N.Kind'Wide_Wide_Image & " " & Ada.Characters.Conversions.To_Wide_Wide_String (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E) & """"));
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
   exception
      when others =>
         N.Print;
         raise;
         --return Null_Unbounded_Wide_Wide_String;
         --return To_Unbounded_Wide_Wide_String (",decl=""Failed " & N.Kind'Wide_Wide_Image & " " & Ada.Characters.Conversions.To_Wide_Wide_String (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E) & """"));
   end Get_Decl_Annotation;
   
   
   function Get_Containment_Annotation (N : LAL.Ada_Node'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      declare
         Parent_Node : constant LAL.Basic_Decl := N.P_Parent_Basic_Decl;
         use type LAL.Analysis_Unit;
      begin
         if not Parent_Node.Is_Null and not (Parent_Node.Unit = Parent_Node.P_Standard_Unit) then -- removing contained in Standard Package
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
   exception
      when others =>
         N.Print;
         raise;
         --return Null_Unbounded_Wide_Wide_String;
   end Get_Containment_Annotation;

end M3.Analysis;
