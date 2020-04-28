-----------------------------------------------------------------------
--     Strings, s package to generalize various string functions     --
--                                                                   --
--                   Copyright (C) 2019 L. Dries                     --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 3 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-----------------------------------------------------------------------

pragma License(Unrestricted);
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

package body Strings is

   package Int_IO is new Integer_IO(Integer); use Int_IO;
   package FLT_IO is new Float_IO(float)    ; use FLT_IO;

   function Convert is new Ada.Unchecked_Conversion (Source => System.Address,
                                                     Target => Long_Long_Integer);

   function Length(Source : String) return Integer is
   begin
      return Length(To_Unbounded_String(Source));
   end Length;

   -----------------------------------------------------------------------------
   -- function C_Index
   -- CIndex find a pattern somewhere in an Unbounded String and returns the
   -- position The position is 0 if not found or within a string
   -- Source     - Unbounded String
   -- Pattern    - String to be found within Source
   -- From       - Position to start with the search (Default value 0)
   --              If zero start from the start
   -- Output     - Position where the Pattern starts
   --              (0 if not found or in string)
   -----------------------------------------------------------------------------
   function CIndex(Source : in Unbounded_String; Pattern : in String; From: Integer := 0) return Integer is
      pos : Integer := 0;
   begin
      if From = 0 then
         pos := Index(Source, Pattern);
      else
         pos := Index(Source, Pattern, From);
      end if;
      return pos;
   end CIndex;

   function CIndex(Source : in Unbounded_String; Pattern : in Unbounded_String; From: Integer := 0) return Integer is
      pos : Integer := 0;
   begin
      pos := CIndex(Source, To_String(Pattern), From);
      return pos;
   end CIndex;

   function CIndex(Source : in String; Pattern : in String; From: Integer := 0) return Integer is
      pos : Integer := 0;
  begin
      pos := CIndex(To_Unbounded_String(Source), Pattern, From);
      return pos;
   end CIndex;

   function CIndex(Source : in String; Pattern : in Unbounded_String; From: Integer := 0) return Integer is
      pos : Integer := 0;
   begin
      pos := CIndex(To_Unbounded_String(Source), To_String(Pattern), From);
      return pos;
   end CIndex;

   function CIndex(Source : in Unbounded_String; Pattern : in String; From: Integer := 0) return str_Val is
      pos : str_Val;
   begin
      pos.first := CIndex(Source, Pattern, From);
      pos.last := pos.first + Length(Pattern);
      return pos;
   end CIndex;

   function CIndex(Source : in Unbounded_String; Pattern : in Unbounded_String; From: Integer := 0) return str_Val is
      pos : str_Val;
   begin
      pos := CIndex(Source, To_String(Pattern), From);
      return pos;
   end CIndex;

   function CIndex(Source : in String; Pattern : in String; From: Integer := 0) return str_Val is
      pos : str_Val;
   begin
      pos := CIndex(To_Unbounded_String(Source), Pattern, From);
      return pos;
   end CIndex;

   function CIndex(Source : in String; Pattern : in Unbounded_String; From: Integer := 0) return str_Val is
      pos : str_Val;
   begin
      pos := CIndex(To_Unbounded_String(Source), To_String(Pattern), From);
      return pos;
   end CIndex;

   function Slice(Source : in Unbounded_String; from : in integer; to : in integer) return Unbounded_String is
   begin
      return To_Unbounded_String(Slice(Source, from, to));
   end Slice;

   function Slice(Source : in String; from : in integer; to : in integer) return Unbounded_String is
   begin
      return Slice(To_Unbounded_String(Source), from, to);
   end Slice;

   function Slice(Source : in String; from : in integer; to : in integer) return String is
   begin
      return Slice(To_Unbounded_String(Source), from, to);
   end Slice;

   function Spaces(n : integer := 1) return String is
   begin
      return To_String(Spaces(n));
   end Spaces;

   function Spaces(n : integer := 1) return Unbounded_String is
      S : Unbounded_String := Empty_String;
   begin
      if n > 0 then
         for n1 in 1 .. n loop
            S := S & " ";
         end loop;
      end if;
      return S;
   end Spaces;

   function Str_Bool(B : boolean) return string is
   begin
      return boolean'image(B);
   end Str_Bool;

   function Str_Bool(B : boolean) return Unbounded_String is
   begin
      return To_Unbounded_String(boolean'image(B));
   end Str_Bool;

   function Str_Int(I : integer) return string is
   begin
      return integer'image(I);
   end Str_Int;

   function Str_Int(I : integer) return Unbounded_String is
   begin
      return To_Unbounded_String(integer'image(I));
   end Str_Int;

   function Str_Int(I : long_integer) return string is
   begin
      return long_integer'image(I);
   end Str_Int;

   function Str_Int(I : long_integer) return Unbounded_String is
   begin
      return To_Unbounded_String(long_integer'image(I));
   end Str_Int;

   function Str_Int(I : long_long_integer) return string is
   begin
      return long_long_integer'image(I);
   end Str_Int;

   function Str_Int(I : long_long_integer) return Unbounded_String is
   begin
      return To_Unbounded_String(long_long_integer'image(I));
   end Str_Int;

   function Str_Float(F : float) return string is
   begin
      return float'image(F);
   end Str_Float;

   function Str_Float(F : float) return Unbounded_String is
   begin
      return To_Unbounded_String(float'image(F));
   end Str_Float;

   function Str_Addr(A : System.Address) return string is
      I : long_long_integer := Convert(A);
   begin
      return Str_Int(I);
   end Str_Addr;

   function Str_Addr(A : System.Address) return Unbounded_String is
   begin
      return To_Unbounded_String(Str_Addr(A));
   end Str_Addr;

end Strings;
