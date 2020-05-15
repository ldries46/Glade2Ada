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
-- Created from Glade2Ada V1.00 by L. Dries on 01-06-2019                  --
------------------------------------------------------------------------------

pragma License(Unrestricted);
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Handlers; use Gtk.Handlers;
with Glade_Name_CB; use Glade_Name_CB;
with Glade2Ada_General; use Glade2Ada_General;

package body Glade_Name_Init is

   package Dialog_CBR is new
     Gtk.Handlers.Return_Callback (Gtk_Dialog_Record, Boolean);
   package Button is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

   procedure Gtk_New(MainDialog : out Win_Dialog_Access) is
   begin
      MainDialog := new Win_Dialog_Record;
      Glade_Name_Init.Init (MainDialog);
   end Gtk_New;

   procedure Init(MainDialog : access Win_Dialog_Record'Class) is
      pragma Suppress (All_Checks);
      Pixmaps_Dir : constant String := "pixmaps/";
   begin
      Gtk.Dialog.Initialize (MainDialog);
      MainDialog.Set_Title ("Glade2Ada Dialog");
      Set_Position (MainDialog, Win_Pos_Center);
      Set_Modal (MainDialog, False);
      Set_Resizable (MainDialog, false);
      Set_Default_Size (MainDialog, Dialogsize_H, Dialogsize_V);
      if Set_Icon_From_File(MainDialog, To_String(Icon_Name)) then
         On_Dialog_End(null);
      end if;
      MainDialog.Dialog_Box := Get_Content_Area(MainDialog);
      Gtk_New (MainDialog.Input_Box, Orientation_Vertical, 0);
      ---------------------------------------------------------------
      -- First Input Box
      ---------------------------------------------------------------
      Gtk_New (MainDialog.Input_Line1, Orientation_Horizontal, 0);
      Gtk_New_with_Mnemonic (MainDialog.Label_Line1, "First entry");
      Gtk_New (MainDialog.Str_Line1);
      Pack_Start
        (MainDialog.Input_Line1,
         MainDialog.Label_Line1,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Input_Line1,
         MainDialog.Str_Line1,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Input_Box,
         MainDialog.Input_Line1,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      ---------------------------------------------------------------
      -- Second Input Box
      ---------------------------------------------------------------
      Gtk_New (MainDialog.Input_Line2, Orientation_Horizontal, 0);
      Gtk_New_with_Mnemonic (MainDialog.Label_Line2, "Second entry");
      Gtk_New (MainDialog.Str_Line2);
      Pack_Start
        (MainDialog.Input_Line2,
         MainDialog.Label_Line2,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Input_Line2,
         MainDialog.Str_Line2,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Input_Box,
         MainDialog.Input_Line2,
         Expand  => True,
         Fill    => True,
         Padding => 5);
      Pack_Start
        (MainDialog.Dialog_Box,
         MainDialog.Input_Box,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      ---------------------------------------------------------------
      -- Check Box
      ---------------------------------------------------------------
      Gtk_New (MainDialog.Label_CH_Box);
      Gtk_New (MainDialog.Input_ChBut, Orientation_Horizontal, 0);
      Gtk_New (MainDialog.Check_But, "Check Box Entry");
      Pack_Start
        (MainDialog.Input_ChBut,
         MainDialog.Check_But,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Input_ChBut,
         MainDialog.Label_CH_Box,
         Expand  => True,
         Fill    => True,
         Padding => 5);
      Pack_Start
        (MainDialog.Dialog_Box,
         MainDialog.Input_ChBut,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      ---------------------------------------------------------------
      -- Combo Box
      ---------------------------------------------------------------
      Gtk_New_with_Mnemonic (MainDialog.Label_Combo, "Combo_box entry");
      Gtk_New (MainDialog.Str_Combo);
      Gtk_New (MainDialog.Input_Combo, Orientation_Horizontal, 0);
      Pack_Start
        (MainDialog.Input_Combo,
         MainDialog.Label_Combo,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Input_Combo,
         MainDialog.Str_Combo,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Dialog_Box,
         MainDialog.Input_Combo,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      ---------------------------------------------------------------
      -- Radio Button Box
      ---------------------------------------------------------------
--      Gtk_New (MainDialog.Radio_Group, "Radio Group");
      Gtk_New (MainDialog.Radio_Grp, Orientation_Vertical, 0);
      Gtk_New (MainDialog.Input_Radio1, Orientation_Horizontal, 0);
      Gtk_New (MainDialog.Input_Radio2, Orientation_Horizontal, 0);
      Gtk_New (MainDialog.Input_Radio3, Orientation_Horizontal, 0);
      Gtk_New (MainDialog.Radio1_But, MainDialog.Radio1_But, "Radio 1");
      Gtk_New (MainDialog.Radio2_But, MainDialog.Radio1_But, "Radio 2");
      Gtk_New (MainDialog.Radio3_But, MainDialog.Radio1_But, "Radio 3");
      Pack_Start
        (MainDialog.Input_Radio1,
         MainDialog.Radio1_But,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Radio_Grp,
         MainDialog.Input_Radio1,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Input_Radio2,
         MainDialog.Radio2_But,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Radio_Grp,
         MainDialog.Input_Radio2,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Input_Radio3,
         MainDialog.Radio3_But,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Radio_Grp,
         MainDialog.Input_Radio3,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainDialog.Dialog_Box,
         MainDialog.Radio_Grp,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      ---------------------------------------------------------------
      -- Buttons OK and Cancel Box
      ---------------------------------------------------------------
      Gtk_New (MainDialog.Button_Box, Orientation_Horizontal, 0);
      Gtk_New (MainDialog.OK, "OK");
      Pack_Start
        (MainDialog.Button_Box,
         MainDialog.OK,
         Expand  => True,
         Fill    => True,
         Padding => 2);
      Gtk_New (MainDialog.Cancel, "Cancel");
      Pack_Start
        (MainDialog.Button_Box,
         MainDialog.Cancel,
         Expand  => True,
         Fill    => True,
         Padding => 2);
      Pack_Start
        (MainDialog.Dialog_Box,
         MainDialog.Button_Box,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      ---------------------------------------------------------------
      -- Size
      ---------------------------------------------------------------
      Set_Size_Request(MainDialog, Dialogsize_H, Dialogsize_V);
      -------------------------------------------------------------
      -- Connect signals
      -------------------------------------------------------------
      Dialog_CBR.Connect(MainDialog, "delete_event",
                         Dialog_CBR.To_Marshaller(On_Dialog_Delete_Event'Access));
      Button.Connect(MainDialog.OK, "clicked",
                     Button.To_Marshaller(On_OK'Access));
      Button.Connect(MainDialog.Cancel, "clicked",
                     Button.To_Marshaller(On_Dialog_End'Access));
   end Init;

--------------------------------------------------------------------------------
-- Change "Glade_Name_Init" in the name of the package                        --
--------------------------------------------------------------------------------
end Glade_Name_Init;
