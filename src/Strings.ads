-----------------------------------------------------------------------
--     Strings, s package to generalize various string functions     --
--                                                                   --
--                   Copyright (C) 2020 L. Dries                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with system;

package Strings is

   type str_Val is record
      first : integer := 0;
      last  : integer := 0;
   end record;

   Empty_String : constant Unbounded_String := To_Unbounded_String("");

   function Length(Source : String) return Integer;
   function CIndex(Source : in Unbounded_String; Pattern : in String; From: Integer := 0) return Integer;
   function CIndex(Source : in Unbounded_String; Pattern : in Unbounded_String; From: Integer := 0) return Integer;
   function CIndex(Source : in String; Pattern : in String; From: Integer := 0) return Integer;
   function CIndex(Source : in String; Pattern : in Unbounded_String; From: Integer := 0) return Integer;
   function CIndex(Source : in Unbounded_String; Pattern : in String; From: Integer := 0) return str_Val;
   function CIndex(Source : in Unbounded_String; Pattern : in Unbounded_String; From: Integer := 0) return str_Val;
   function CIndex(Source : in String; Pattern : in String; From: Integer := 0) return str_Val;
   function CIndex(Source : in String; Pattern : in Unbounded_String; From: Integer := 0) return str_Val;
   function Slice(Source : in Unbounded_String; from : in integer; to : in integer) return Unbounded_String;
   function Slice(Source : in String; from : in integer; to : in integer) return Unbounded_String;
   function Slice(Source : in String; from : in integer; to : in integer) return String;
   function Spaces(n : integer := 1) return String;
   function Spaces(n : integer := 1) return Unbounded_String;
   function Str_Bool(B : boolean) return String;
   function Str_Bool(B : boolean) return Unbounded_String;
   function Str_Int(I : integer) return String;
   function Str_Int(I : integer) return Unbounded_String;
   function Str_Int(I : long_integer) return String;
   function Str_Int(I : long_integer) return Unbounded_String;
   function Str_Int(I : long_long_integer) return String;
   function Str_Int(I : long_long_integer) return Unbounded_String;
   function Str_Float(F : float) return String;
   function Str_Float(F : float) return Unbounded_String;
   function Str_Addr(A : System.Address) return String;

end Strings;
