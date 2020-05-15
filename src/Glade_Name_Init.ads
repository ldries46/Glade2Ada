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
with Gtk.Widget;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Glade_Name_Init is

   type Win_Dialog_Record is new Gtk_Dialog_Record with record
      Dialog_Box   : Gtk_Box;
      Input_Box    : Gtk_Box;
      --------------------------------------------------------------------------
      --                                                                      --
      -- Box for normal Entry and Label                                       --
      --                                                                      --
      --------------------------------------------------------------------------
      Input_Entry  : Gtk_Box;
      Input_Line1  : Gtk_Box;
      Input_Line2  : Gtk_Box;
      --------------------------------------------------------------------------
      --                                                                      --
      -- Label and entry                                                      --
      --                                                                      --
      --------------------------------------------------------------------------
      Label_Line1  : Gtk_Label;
      Label_Line2  : Gtk_Label;
      Str_Line1    : Gtk_Entry;
      Str_Line2    : Gtk_Entry;
      --------------------------------------------------------------------------
      --                                                                      --
      -- Box for normal Check Button                                          --
      --                                                                      --
      --------------------------------------------------------------------------
      Input_ChBut  : Gtk_Box;
      Check_But    : Gtk_Check_Button;
      Label_CH_Box : Gtk_Label;
      --------------------------------------------------------------------------
      --                                                                      --
      --  Create 3 Boxes for RadioButtons together with 3 radio buttons       --
      --                                                                      --
      --------------------------------------------------------------------------
      Input_Radio1 : Gtk_Box;
      Input_Radio2 : Gtk_Box;
      Input_Radio3 : Gtk_Box;
      Radio1_But   : Gtk_Radio_Button;
      Radio2_But   : Gtk_Radio_Button;
      Radio3_But   : Gtk_Radio_Button;
      Radio_Grp    : Gtk_Box;
      Radio_Group  : access Gtk_Radio_Button_Record'Class;
      --------------------------------------------------------------------------
      --                                                                      --
      -- Label and Combobox                                                   --
      --                                                                      --
      --------------------------------------------------------------------------
      Input_Combo  : Gtk_Box;
      Label_Combo  : Gtk_Label;
      Str_Combo    : Gtk_Combo_Box_Text;
      --------------------------------------------------------------------------
      --                                                                      --
      -- Button Box                                        --
      --                                                                      --
      --------------------------------------------------------------------------
      Button_Box   : Gtk_Box;
      --------------------------------------------------------------------------
      --                                                                      --
      -- Buttons OK and Cancel                                        --
      --                                                                      --
      --------------------------------------------------------------------------
      OK           : Gtk_Button;
      Cancel       : Gtk_Button;
   end record;

   type Win_Dialog_Access is access all Win_Dialog_Record'Class;

   Dialogsize_H : Gint := 150;  -- Width of the dialog window
   Dialogsize_V : Gint := 80;   -- Height of the dialog window
   Main_Dialog  : Win_Dialog_Access;
   Dialog_Open  : Boolean := false;
   -----------------------------------------------------------------------------
   --                                                                         --
   -- Parameters for Combobox                                                 --
   --                                                                         --
   -----------------------------------------------------------------------------
   n_ID         : Gint := 0;
   -----------------------------------------------------------------------------
   --                                                                         --
   -- Parameters for Entry box                                                --
   --                                                                         --
   -----------------------------------------------------------------------------
   last_txt1    : Unbounded_String := To_Unbounded_String("Last Input1");
   last_txt2    : Unbounded_String := To_Unbounded_String("Last Input2");
   -----------------------------------------------------------------------------
   --                                                                         --
   -- Parameters for Check button                                             --
   --                                                                         --
   -----------------------------------------------------------------------------
   mode         : boolean := false;
   -----------------------------------------------------------------------------
   --                                                                         --
   -- Parameters for Radio buttons                                            --
   --                                                                         --
   -----------------------------------------------------------------------------
   mode_Radio1  : boolean := true;
   mode_Radio2  : boolean := false;
   mode_Radio3  : boolean := false;

   procedure Gtk_New(MainDialog : out Win_Dialog_Access);
   procedure Init(MainDialog : access Win_Dialog_Record'Class);

end Glade_Name_Init;
