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

with Libadalang.Analysis;
with Libadalang.Common;
with Ada.Wide_Wide_Text_IO;

package Export.Context is

   subtype Entry_Point_Enum_Type is Libadalang.Common.Ada_Node_Kind_Type with Static_Predicate => 
     Entry_Point_Enum_Type in Libadalang.Common.Ada_Compilation_Unit
       | Libadalang.Common.Ada_Compilation_Unit_List
         | Libadalang.Common.Ada_Pragma_Node_List;
   
   type File_Type_Access is access all Ada.Wide_Wide_Text_IO.File_Type;

   type Print_Context_Record_Type is record
      File         : File_Type_Access;
      Indent       : Natural;  -- used only on debug mode
   end record;
   
   type Type_Context_Record_Type is record
      N                        : Libadalang.Analysis.Ada_Node;
      Is_Optional              : Boolean;
      Need_Chained_Constructor : Boolean;
   end record;
   
   function Add_Indent_Level (Print_Context : Print_Context_Record_Type) 
                              return Print_Context_Record_Type;
   
end Export.Context;
