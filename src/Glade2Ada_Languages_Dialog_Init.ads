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
with Glib; use Glib;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Button; use Gtk.Button;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Glade2Ada_Languages_Dialog_Init is

   type Lan_Dialog_Record is new Gtk_Dialog_Record with record
      Dialog_Box   : Gtk_Box;
      Input_Box    : Gtk_Box;
      Combo_Box    : Gtk_Box;
      Combo_Label  : Gtk_Label;
      Combo_Inp    : Gtk_Combo_Box_Text;
      Button_Box   : Gtk_Box;
      OK           : Gtk_Button;
      Cancel       : Gtk_Button;
   end record;
   type Lan_Dialog_Access is access all Lan_Dialog_Record'Class;

   Dialogsize_H : Gint := 150;  -- Width of the dialog window
   Dialogsize_V : Gint := 80;   -- Height of the dialog window
   Lan_Dialog   : Lan_Dialog_Access;

   procedure Gtk_New(LanDialog : out Lan_Dialog_Access);
   procedure Init(LanDialog : access Lan_Dialog_Record'Class);

end Glade2Ada_Languages_Dialog_Init;
