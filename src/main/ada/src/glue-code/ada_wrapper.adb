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

with Export.Ast;
with Ada.Exceptions;
package body Ada_Wrapper is

   function Ada_Export_File_Wrapper (Ada_File : Interfaces.C.Strings.Chars_Ptr;
                                    Out_File : Interfaces.C.Strings.Chars_Ptr)
                                    return Interfaces.C.Strings.Chars_Ptr is
   begin
      Export.Ast.Export_File (Interfaces.C.Strings.Value (Ada_File), Interfaces.C.Strings.Value (Out_File));
      return Interfaces.C.Strings.Null_Ptr;
      exception
         when E : others =>
            return Interfaces.C.Strings.New_String (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E));
   end Ada_Export_File_Wrapper;


   function Ada_Export_Project_Wrapper (Ada_Project : Interfaces.C.Strings.Chars_Ptr;
                                        Out_File : Interfaces.C.Strings.Chars_Ptr)
                                        return Interfaces.C.Strings.Chars_Ptr is
   begin
      Export.Ast.Export_Project (Interfaces.C.Strings.Value (Ada_Project), Interfaces.C.Strings.Value (Out_File));
      return Interfaces.C.Strings.Null_Ptr;
      exception
         when E : others =>
            return Interfaces.C.Strings.New_String (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E));
   end Ada_Export_Project_Wrapper;


end Ada_Wrapper;
