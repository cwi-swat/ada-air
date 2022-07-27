with Ada.Exceptions;
with Ada.Characters.Conversions;
with M3.Implementation;
package body M3.Analysis is

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
      declare
         Decl : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
         Decl_Node : constant LAL.Basic_Decl := N.P_Referenced_Decl (True);
      begin
         if not Decl_Node.Is_Null then
            declare
               S : constant Wide_Wide_String := Implementation.Get_Rascal_Logical_Location (Decl_Node, Decl_Node.P_Defining_Name.F_Name);
            begin
               if S'Length > 0 then
                  Decl := To_Unbounded_Wide_Wide_String (",use=" & S);
               end if;
            end;
         end if;
         return Decl;
      end;
   exception
      when others =>
         return Null_Unbounded_Wide_Wide_String;
   end Get_Use_Annotation;
   
   function Get_Decl_Annotation (N : LAL.Basic_Decl'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      declare
         S : constant Wide_Wide_String :=  Implementation.Get_Rascal_Logical_Location (N);
      begin
         if S'Length > 0 then
            return  To_Unbounded_Wide_Wide_String (",declaration=" & S);
         end if;
      end;
      return Null_Unbounded_Wide_Wide_String;
   exception
      when e : others =>
         return To_Unbounded_Wide_Wide_String (",declaration=""Failed " & N.Kind'Wide_Wide_Image & " " & Ada.Characters.Conversions.To_Wide_Wide_String (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E) & """"));
   end Get_Decl_Annotation;
   
   
   function Get_Containment_Annotation (N : LAL.Ada_Node'Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      declare
         Parent_Node : constant LAL.Basic_Decl := N.P_Parent_Basic_Decl;
      begin
         if not Parent_Node.Is_Null then
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
         return Null_Unbounded_Wide_Wide_String;
   end Get_Containment_Annotation;

end M3.Analysis;
