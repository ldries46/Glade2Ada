------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 2019 L. Dries                                         --
--                                                                          --
-- This program is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
-- Version 1.00 dd. 17-08-2019 created by L. Dries                          --
------------------------------------------------------------------------------
-- Created from Glade2Ada V1.00 by L. Dries on 01-06-2019                   --
------------------------------------------------------------------------------

pragma License(Unrestricted);
with Glib;                 use Glib;
with GNAT.OS_lib;          use GNAT.OS_lib;
with Gtk.Text_Buffer;      use Gtk.Text_Buffer;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Buffer;
with Strings;              use Strings;

package Glade2Ada_General is

   package Buffer_G is new Buffer(Unbounded_String);
   package Main_Pac is new Buffer(Unbounded_String);
   package Init_B is new Buffer(Unbounded_String);
   package Init_S is new Buffer(Unbounded_String);
   package CB_B is new Buffer(Unbounded_String);
   package CB_S is new Buffer(Unbounded_String);

   type File_Status is (Open, Save, Dir);

   n_ini          : integer;
   follow         : boolean := true;
   b_init         : boolean := true;
   Icon_Name      : Unbounded_String;
   CRLF           : constant Unbounded_String := Empty_String & ASCII.CR & ASCII.LF;
   Space          : constant Unbounded_String := Empty_String & " ";
   Cur_Folder     : Unbounded_String;
   Inp_Folder     : Unbounded_String;
   Out_Folder     : Unbounded_String;
   Pack_Name      : Unbounded_String;
   Project_Name   : Unbounded_String;
   Pack_Name_Init : Unbounded_String;
   Pack_Name_CB   : Unbounded_String;
   Inp_File       : Unbounded_String;
   Out_Fil        : Unbounded_String;
   Dir_Sep        : String := To_String(Empty_String & Directory_Separator);
   Ar_Text_Buf    : array (1 .. 3) of Gtk_Text_Buffer;


end Glade2Ada_General;
