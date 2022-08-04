with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Strings_Utils;
with Ada.Characters.Conversions;
with Libadalang.Iterators;

package body M3.Implementation is
   
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package LALIT renames Libadalang.Iterators;
   
   ---------------------------------
   -- Rascal_Fully_Qualified_Name --
   ---------------------------------

   function Rascal_Fully_Qualified_Name
     (N : Libadalang.Analysis.Defining_Name) return Wide_Wide_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      function Already_In (N : Unbounded_Wide_Wide_String; S : Wide_Wide_String) return Boolean is       
      begin
         if Length (N) > 1 then
            declare
               Slash : constant Natural := Index (N , S, 2);
               Next_Char : constant Natural := Slash + S'Length + 1;
            begin
               if Slash > 0 then
                  for I in 2..Slash-1 loop
                     if Element (N, I) /= S (I - 2 + S'First) then
                        return False;
                     end if;
                  end loop;
                  -- checking that the element found is not a substring
                  return not (Length (N) > Next_Char and then Element (N, Next_Char) /= '/'); 
               end if;
            end;
         end if;
         return False;         
      end Already_In;
   begin
      If N.Is_Null then
         raise Constraint_Error with "AH";
      end if;
      declare
         
         Parents : constant LAL.Ada_Node_Array := N.Parents (With_Self => True);      
         Name : Unbounded_Wide_Wide_String;
         use type LALCO.Ada_Node_Kind_Type;
      begin
         for P of Parents loop         
            if P.Kind in Scope_Kind_Type then

               declare
                  Parent_Name : constant Wide_Wide_String := (case Scope_Kind_Type (P.Kind) is
                                                                 when LALCO.Ada_Exception_Handler => "/catch",
                                                                 when others => "/scope");
                  Scope_Idx : Natural := 0;
                  It : LAL.Ada_Node := P.Previous_Sibling;
                  use Ada.Strings.Wide_Wide_Fixed;
               begin
                  if not (P.Kind = LALCO.Ada_Named_Stmt) and not (P.Parent.Kind = LALCO.Ada_Named_Stmt) then 
                     while not It.Is_Null loop
                        if It.Kind in Scope_Kind_Type then
                           Scope_Idx := Scope_Idx + 1;
                        end if;
                        It := It.Previous_Sibling;
                     end loop;               
                     Insert (Name, 1, Parent_Name & "(" & Trim (Scope_Idx'Wide_Wide_Image, Ada.Strings.Both) & ")");
                  elsif not (P.Parent.Kind = LALCO.Ada_Named_Stmt) then
                     Insert (Name, 1, Parent_Name & "(" & Trim (P.As_Named_Stmt.F_Decl.F_Name.Text, Ada.Strings.Both) & ")");
                  end if;
               end;

            elsif	P.Kind in Signature_Kind_Type then
               declare
                  Parent_Name : constant Wide_Wide_String := Strings_Utils.Replace (Get_Subprogram_Signature(P.As_Basic_Decl), ".", "/");
               begin
                  if not Already_In (Name, Parent_Name) then
                     Insert (Name, 1, "/" & Parent_Name);
                  end if;
               end;
            elsif P.Kind in LALCO.Ada_Basic_Decl and not (N.Parent.Kind = LALCO.Ada_Named_Stmt_Decl) and not (N.Kind = LALCO.Ada_Named_Stmt_Decl) then
               declare
                  Parent_Name : constant Wide_Wide_String := Strings_Utils.Replace (P.As_Basic_Decl.P_Defining_Name.Text, ".", "/");
               begin
                  if not Already_In (Name, Parent_Name) then
                     Insert (Name, 1, "/" & Parent_Name);
                  end if;
               end;
            end if;         
         end loop;
         return To_Wide_Wide_String (Name);
      end;
   exception
      when others =>
         raise Constraint_Error with "AHAH";
   end Rascal_Fully_Qualified_Name;
   
  
   
   function Get_Rascal_Physical_Location
     (N : LAL.Ada_Node'Class) return URI_Utils.URI
   is      
   begin
      if not N.Is_Null then
         declare
            Loc : constant URI_Utils.Location_Record_Type := URI_Utils.Location (N);
            -- work-arround Rascal doesn't allow backslash
            FileName : constant Wide_Wide_String :=
              Strings_Utils.Replace
                (Ada.Characters.Conversions.To_Wide_Wide_String
                   (N.Unit.Get_Filename),
                 "\", "/");
         begin
            return URI_Utils.Create_URI ("file", FileName, Loc);
         end;
      else
         return URI_Utils.Null_URI;
      end if;
   end Get_Rascal_Physical_Location;
   

   function Get_Rascal_Logical_Location
     (N : LAL.Basic_Decl'Class; Name : LAL.Name := LAL.No_Name) return Wide_Wide_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use URI_Utils;

      Res   : Unbounded_Wide_Wide_String    := Null_Unbounded_Wide_Wide_String;
           
      procedure Classic_Annotation (Scheme : Wide_Wide_String) is         
      begin
         Append (Res, Create_URI (Scheme => Scheme,
                                  Path => Rascal_Fully_Qualified_Name (N.P_Defining_Name)));
      end Classic_Annotation;

   begin
      -- TODO removing multiple declaration
      
      if N.Kind in Skipped_Kind_Type then
         return "";
      end if;
      case N.Kind is
         when LALCO.Ada_Base_Package_Decl |
              LALCO.Ada_Generic_Package_Decl =>
            Classic_Annotation ("ada+packageSpec");
            
         when LALCO.Ada_For_Loop_Var_Decl
            | LALCO.Ada_Extended_Return_Stmt_Object_Decl =>
            Classic_Annotation ("ada+variable");

            
         when LALCO.Ada_Object_Decl =>              
            declare
               First : Boolean := True;
               Ids : constant LAL.Defining_Name_List := N.As_Object_Decl.F_Ids;
            begin
               if Ids.Children_Count > 1 and Name.Is_Null then
                  Append (Res, "[");
               end if;
               
               for Id of Ids loop 
                  if Name.Is_Null or else LAL.P_Name_Matches (Name, Id.F_Name) then
                     if not First then
                        Append (Res, ",");
                     end if;
                     First := False;                  
                     Append (Res, Create_URI (Scheme => "ada+variable",
                                              Path => Rascal_Fully_Qualified_Name  (Id.As_Defining_Name)));
                  end if;
               end loop;
               
               if Ids.Children_Count > 1 and Name.Is_Null then
                  Append (Res, "]");
               end if;
            end;
               
         when LALCO.Ada_Number_Decl =>
            Classic_Annotation ("ada+variable");
            
         when LALCO.Ada_Task_Type_Decl =>
            Classic_Annotation ("ada+taskType");

         when LALCO.Ada_Type_Decl | LALCO.Ada_Base_Subtype_Decl |
              LALCO.Ada_Classwide_Type_Decl | LALCO.Ada_Incomplete_Type_Decl |
              LALCO.Ada_Protected_Type_Decl =>
            Classic_Annotation ("ada+type");

         when LALCO.Ada_Subp_Body | LALCO.Ada_Expr_Function =>
            Classic_Annotation ("ada+subprogramBody");
        
         when LALCO.Ada_Subp_Decl | LALCO.Ada_Generic_Subp_Decl |
              LALCO.Ada_Null_Subp_Decl | LALCO.Ada_Abstract_Subp_Decl | LALCO.Ada_Generic_Subp_Internal =>
            Classic_Annotation ("ada+subprogramSpec"); 

         when LALCO.Ada_Package_Body =>
            Classic_Annotation ("ada+packageBody");

         when LALCO.Ada_Component_Decl =>
            declare
               First : Boolean := True;              
            begin
               if N.As_Component_Decl.F_Ids.Children_Count > 1 and Name.Is_Null then 
                  Res := To_Unbounded_Wide_Wide_String ("[");
               end if;
               for Id of N.As_Component_Decl.F_Ids loop
                  if Name.Is_Null or else LAL.P_Name_Matches (Name, Id.F_Name) then
                     if not First then
                        Append (Res, ",");
                     end if;
                     First := False;                     
                     Append (Res, Create_URI (Scheme => "ada+field",
                                              Path => Rascal_Fully_Qualified_Name (Id.As_Defining_Name)));
                                          
                  end if;
               end loop;
               
               if N.As_Component_Decl.F_Ids.Children_Count > 1 and Name.Is_Null then 
                  Append (Res, To_Unbounded_Wide_Wide_String ("]")); 
               end if;
            end;

         when LALCO.Ada_Param_Spec =>
            declare
               First : Boolean := True;     
            begin
               if N.As_Param_Spec.F_Ids.Children_Count > 1 and Name.Is_Null then 
                  Res := To_Unbounded_Wide_Wide_String ("["); 
               end if;
               for Arg of N.As_Param_Spec.F_Ids loop
                  if Name.Is_Null or else LAL.P_Name_Matches (Name, Arg.F_Name) then
                     if not First then
                        Append (Res, ",");
                     end if;
                     First := False;                     
                     Append (Res, create_URI (Scheme => "ada+parameter",
                                              Path => Rascal_Fully_Qualified_Name (Arg.As_Defining_Name)));                                        
                  end if;
               end loop;
               
               if N.As_Param_Spec.F_Ids.Children_Count > 1 and Name.Is_Null then 
                  Append (Res, To_Unbounded_Wide_Wide_String ("]")); 
               end if;
            end;

         when LALCO.Ada_Exception_Decl =>
            Classic_Annotation ("ada+exception");

         when LALCO.Ada_Enum_Literal_Decl =>
            Classic_Annotation ("ada+enumLiteral");

         when LALCO.Ada_Discriminant_Spec =>
            Classic_Annotation ("ada+discriminant");

         when LALCO.Ada_Generic_Package_Instantiation =>
            Classic_Annotation ("ada+packageInstantiation");

         when LALCO.Ada_Generic_Subp_Instantiation =>
            Classic_Annotation ("ada+subprogramInstantiation");        

         when LALCO.Ada_Generic_Formal_Subp_Decl |
              LALCO.Ada_Concrete_Formal_Subp_Decl   =>
            Classic_Annotation ("ada+formalSubprogram");
         
         when LALCO.Ada_Generic_Formal_Type_Decl =>
            Classic_Annotation ("ada+formalType");

         when LALCO.Ada_Generic_Formal_Obj_Decl =>
            Classic_Annotation ("ada+formalObj");
            
         when LALCO.Ada_Exception_Handler =>
            if not N.As_Exception_Handler.F_Exception_Name.Is_Null then            
               Classic_Annotation ("ada+exceptionHandler");
            end if;
            
         when LALCO.ADA_PROTECTED_BODY =>
            Classic_Annotation ("ada+protectedBody");

            
         when LALCO.ADA_SINGLE_PROTECTED_DECL =>
            Classic_Annotation ("ada+protectedObj");

            
         when LALCO.Ada_Package_Renaming_Decl 
            | LALCO.Ada_Generic_Package_Renaming_Decl=>
            Classic_Annotation ("ada+packageRenaming");

            
         when LALCO.Ada_Subp_Renaming_Decl
            | LALCO.Ada_Generic_Subp_Renaming_Decl=>
            Classic_Annotation ("ada+subprogramRenaming");
            
            
         when LALCO.Ada_Anonymous_Type_Decl =>
            Res := Null_Unbounded_Wide_Wide_String;
            
         when LALCO.Ada_Label_Decl =>
            Classic_Annotation ("ada+label");
            
         when LALCO.Ada_Named_Stmt_Decl =>
            Classic_Annotation ("ada+namedStmt");
            
         when LALCO.Ada_Task_Body =>
            Classic_Annotation ("ada+taskBody");
            
         when LALCO.Ada_Entry_Decl =>
            Classic_Annotation ("ada+entry");
            
         when LALCO.Ada_Single_Task_Decl =>
            Classic_Annotation ("ada+taskSingleDecl");
            
         when others =>
            Res :=
              To_Unbounded_Wide_Wide_String
                ("|ada+unknow:///" & N.Kind'Wide_Wide_Image & "|");

      end case;

      return To_Wide_Wide_String (Res);
   exception
      when others =>
         raise Constraint_Error with "OH";
   end Get_Rascal_Logical_Location;

   ------------------------------
   -- Get_Subprogram_Signature --
   ------------------------------

   function Get_Subprogram_Signature
     (N : Libadalang.Analysis.Basic_Decl'Class) return Wide_Wide_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Unbounded_Wide_Wide_String;
      Spec  : constant LAL.Subp_Spec := LALIT.Find_First (N, LALIT.Kind_IS (LALCO.Ada_Subp_Spec)).As_Subp_Spec;
   begin
      if not Spec.Is_Null then 
         
         Set_Unbounded_Wide_Wide_String (Result, N.P_Defining_Name.Text);
         declare
            use LALIT;
            P : constant LAL.Params := Find_First (Spec, Kind_Is (LALCO.Ada_Params)).As_Params;
         begin
            if not P.Is_Null then
               Append (Result, Get_Params_Signature (P));
            else
               Append (Result, "()");
            end if;
         end;

         case Spec.F_Subp_Kind.Kind is
            when LALCO.Ada_Subp_Kind_Function =>
               Append (Result, ":" & Get_Type_Signature (Spec.F_Subp_Returns));
            when others => null;
         end case;
      else
         raise Program_Error with N.Kind_Name;
      end if;
      return To_Wide_Wide_String (Result);
   end Get_Subprogram_Signature;
   

   --------------------------
   -- Get_Params_Signature --
   --------------------------
   
   function Get_Params_Signature (N : Libadalang.Analysis.Params'Class) return Wide_Wide_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Unbounded_Wide_Wide_String;
      Is_Empty : Boolean := True;
   begin
      for Param of N.F_Params loop
         if Is_Empty then
            Append (Result, "(");
         else
            Append (Result, ",");
         end if;
         Is_Empty := False;
         if Param.F_Has_Aliased then
            Append (Result, "aliased ");
         end if;
         Append (Result, Get_Type_Signature (Param.F_Type_Expr));
      end loop;
      
      if not Is_Empty then
         Append (Result, ")");
      else
         Append (Result, "()");
      end if;
      return To_Wide_Wide_String (Result);
   end Get_Params_Signature;
   
   
   ------------------------
   -- Get_Type_Signature --
   ------------------------
   
   function Get_Type_Signature (N : Libadalang.Analysis.Type_Expr'Class) return Wide_Wide_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Unbounded_Wide_Wide_String;
   begin
      case N.Kind is
         when LALCO.Ada_Anonymous_Type =>            
            declare                              
               Def : constant LAL.Access_Def := N.As_Anonymous_Type.F_Type_Decl.F_Type_Def.As_Access_Def;
               Has_Not_Null : constant Boolean := Def.F_Has_Not_Null;
            begin             
               if Has_Not_Null then
                  Append (Result, "not null ");
               end if;
               
               case Def.Kind is
                  
                  when LALCO.Ada_Access_To_Subp_Def =>
                     declare                        
                        Spec : constant LAL.Subp_Spec := Def.As_Access_To_Subp_Def.F_Subp_Spec;
                        Has_Protected : constant Boolean := Def.As_Access_To_Subp_Def.F_Has_Protected;
                     begin
                        if Has_Protected then
                           Append (Result, "protected ");
                        end if;
                        if not Spec.F_Subp_Params.Is_Null then
                           Append (Result, Get_Params_Signature (Spec.F_Subp_Params));
                        else
                           Append (Result, "()");
                        end if;
                        case Spec.F_Subp_Kind.Kind is
                           
                           when LALCO.Ada_Subp_Kind_Function =>
                              Append (Result, ":" & Get_Type_Signature (Spec.F_Subp_Returns));
                              
                           when others =>
                              null;
                        end case;
                     end;
                     
                  when LALCO.Ada_Type_Access_Def =>
                     declare
                        Has_Constant : constant Boolean := Def.As_Type_Access_Def.F_Has_Constant;
                        Has_All : constant Boolean := Def.As_Type_Access_Def.F_Has_All;
                     begin
                        if Has_All then
                           Append (Result, "all ");
                        end if;
                        if Has_Constant then
                           Append (Result, "constant ");
                        end if;
                        Append (Result, Def.As_Type_Access_Def.F_Subtype_Indication.Text);
                     end;
                     when others => null;
               end case;
            end;
                           
         when LALCO.Ada_Subtype_Indication =>                          
               Append (Result, N.Text);

         when others => null;
      end case;
      return To_Wide_Wide_String (Result);
   end Get_Type_Signature;
   

end M3.Implementation;
