with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Libadalang.Project_Provider;

package body Utils is

   
   function Load_Project (Project_Filename : String)return LAL.Unit_Provider_Reference is
      package GPR renames GNATCOLL.Projects;
      package LAL_GPR renames Libadalang.Project_Provider;
      use type GNATCOLL.VFS.Filesystem_String;
      Project_File     : constant GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.Create (+Project_Filename);

      Env     : GPR.Project_Environment_Access;
      Project : constant GPR.Project_Tree_Access := new GPR.Project_Tree;
   begin
      GPR.Initialize (Env);
      Project.Load (Project_File, Env);
      return LAL_GPR.Create_Project_Unit_Provider (Tree => Project, Env => Env);
   end Load_Project;
   
   
   function Simple_Name (Node_Type : LAL.Type_Decl) return String is
      package LAL renames Libadalang.Analysis;
      package LALCO renames Libadalang.Common;
      Type_Name_Array : constant LAL.Unbounded_Text_Type_Array := Node_Type.F_Name.P_Fully_Qualified_Name_Array;
      use Langkit_Support.Text;
   begin
      return Image (T => To_Text (UT => Type_Name_Array (Type_Name_Array'Last)));
   end Simple_Name;
   
   
   function Get_Ada_Node_Type_Decl (Root : LAL.Ada_Node) return LAL.Type_Decl is
   begin
      for N of Libadalang.Iterators.Find (Root      => Root,
                                          Predicate => Libadalang.Iterators.Kind_Is (Kind => LALCO.Ada_Type_Decl)).Consume
      loop
         declare
            Type_Name : constant String := Simple_Name (N.As_Type_Decl);
         begin
            if Ada.Strings.Equal_Case_Insensitive (Left => Type_Name, Right => "Ada_Node") and not N.As_Type_Decl.P_Is_Private then
               return N.As_Type_Decl;
            end if;
         end;
      end loop;
      raise Constraint_Error with "Ada_Node Type Declaration not found";
   end Get_Ada_Node_Type_Decl;

end Utils;
