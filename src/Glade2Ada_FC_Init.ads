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
with Gtk.Widget; use Gtk.Widget;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Button; use Gtk.Button;
with GTk.File_Chooser; use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Glade2Ada_FC_Init is

   Open_Title         : constant String := "Open";
   Save_As_Title      : constant String := "Save As";
   Dir_Title          : constant String := "Directory";
   File_Open_Dialog   : Gtk_File_Chooser_Dialog;
   File_Save_Dialog   : Gtk_File_Chooser_Dialog;
   File_Dir_Dialog    : Gtk_File_Chooser_Dialog;
   FC_Open_Cancel     : Gtk_Button;
   FC_Open_OK         : Gtk_Button;
   FC_Dir_OK          : Gtk_Button;
   FC_Save_Cancel     : Gtk_Button;
   FC_Dir_Cancel      : Gtk_Button;
   FC_Save_OK         : Gtk_Button;
   Default_Name       : constant String := "Default_Glade";
   File_Name          : Unbounded_String := To_Unbounded_String(Default_Name);
   Extension          : Unbounded_String := To_Unbounded_String("glade");

   procedure Init_Open;
   procedure Init_Save;
   procedure Init_Select_Folder;

end Glade2Ada_FC_Init;
