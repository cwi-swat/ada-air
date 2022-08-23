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

with Ada.Text_IO;
with GNATCOLL.Utils;
with Ada.Strings.Unbounded;
with Ada.Directories;
with ada.Strings.Fixed;
  
package body Export.Debug is
   package LALCO renames Libadalang.Common;
   

   File_Name : constant String := "array.txt";

   procedure Save_Constructors_Used (A : Constructors_Used_Array) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => F, Mode => Ada.Text_IO.Out_File, Name =>  File_Name);
      for I in A'Range loop
         Ada.Text_IO.Put_line (F, Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both)
                               & ":" &
                                 Ada.Strings.Fixed.Trim (A (I)'Image, Ada.Strings.Both));        
      end loop;
      Ada.Text_IO.Close (F);      
   end Save_Constructors_Used;
   
   function Load_Constructors_Used return Constructors_Used_Array is
      Result : Constructors_Used_Array := (others => False);
      F : Ada.Text_IO.File_Type;
   begin
      if Ada.Directories.Exists (File_Name) then
         Ada.Text_IO.Open (File => F, Mode => Ada.Text_IO.In_File, Name =>  File_Name);
         while not Ada.Text_IO.End_Of_File (F) loop
            declare
               Line : constant String := Ada.Text_IO.Get_Line (F);
               Split : constant GNATCOLL.Utils.Unbounded_String_Array := GNATCOLL.Utils.Split (Str => Line, On => ':');
               Kind : constant LALCO.Ada_Node_Kind_Type := LALCO.Ada_Node_Kind_Type'Value (Ada.Strings.Unbounded.To_String (Split(Split'First)));
               Value : constant Boolean := Boolean'Value (Ada.Strings.Unbounded.To_String (Split(Split'First + 1)));
            begin
               Result (Kind) := Value;
            end;
         end loop;
      else
         Ada.Text_IO.Create (File => F, Mode => Ada.Text_IO.In_File, Name =>  File_Name);
      end if;
      Ada.Text_IO.Close (F);
      return result;
   end Load_Constructors_Used;
   

end Export.Debug;
