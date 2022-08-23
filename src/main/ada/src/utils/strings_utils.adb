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

with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Langkit_Support.Text;
with Ada.Characters.Conversions;

package body Strings_Utils is

   function Camel_Case_To_Snake_Case(S : String) return String is
      use Ada.Strings.Unbounded;
      use Ada.Characters.Handling;
      Unb : Unbounded_String := To_Unbounded_String ("" & To_Lower(S(S'First)));
   begin
      for i in S'First+1..S'Last loop
         if Is_Upper (S(i)) then
            Append (Unb, "_");
         end if;
         Append (Unb, To_Lower (S(i)));
      end loop;
      return To_String(Unb);
   end Camel_Case_To_Snake_Case;

   function Replace
     (S : String; Pattern : String; Replacement : String) return String
   is
      Idx : Natural;
   begin
      Idx := Ada.Strings.Fixed.Index (S, Pattern);

      if Idx = 0 then
         return S;
      else
         return S (S'First .. Idx - 1) & Replacement
           & Replace
           (S           => S (Idx + Pattern'Length .. S'Last),
            Pattern     => Pattern,
            Replacement => Replacement);
      end if;
   end Replace;
   
   function Replace
     (S : Wide_Wide_String; Pattern : Wide_Wide_String; Replacement : Wide_Wide_String) return Wide_Wide_String
   is
      Idx : Natural;
   begin
      Idx := Ada.Strings.Wide_Wide_Fixed.Index (S, Pattern);

      if Idx = 0 then
         return S;
      else
         return S (S'First .. Idx - 1) & Replacement
           & Replace
           (S           => S (Idx + Pattern'Length .. S'Last),
            Pattern     => Pattern,
            Replacement => Replacement);
      end if;
   end Replace;
   
   procedure Replace
     (S           : in out Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Pattern     : Wide_Wide_String;
      Replacement : Wide_Wide_String)
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Ind : Natural := Index_Non_Blank (S);
   begin
      while Ind < Length (S) loop
         Ind := Index (S, Pattern, Ind);

         exit when Ind = 0;

         S := Replace_Slice (S, Ind, Ind + Pattern'Length - 1, Replacement);
         Ind := Ind + Replacement'Length;
      end loop;
   end Replace;
   
   function Format_String (S : Wide_Wide_String) return Wide_Wide_String is
   begin
      return Ada.Characters.Conversions.To_Wide_Wide_String (Langkit_Support.Text.Image (S, True));
   end Format_String;
   
end Strings_Utils;
