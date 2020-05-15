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
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Program_Init; use Program_Init;
with Glade2Ada_Main_Init; use Glade2Ada_Main_Init;
with Glade2Ada_General; use Glade2Ada_General;
with Glade2Ada_Languages; use Glade2Ada_Languages;

procedure Glade2Ada is
begin
   n_ini := Use_File("Glade2Ada");
   Lan :=Language'Value(Get_Value(n_ini, "Languages", "Lan",
                        Language'Image(GB_USA)));
   Cur_Folder := To_Unbounded_String(Current_Directory);
   Inp_Folder := To_Unbounded_String(Get_Value(n_ini, "Directory", "Input",
                                     To_String(Cur_Folder)));
   Out_Folder := To_Unbounded_String(Get_Value(n_ini, "Directory", "Output",
                                     To_String(Inp_Folder)));
   Gtk.Main.Init;
   Gtk_New (Main_Window);
   Main_Window.Show_All;
   Gtk.Main.Main;
end Glade2Ada;
