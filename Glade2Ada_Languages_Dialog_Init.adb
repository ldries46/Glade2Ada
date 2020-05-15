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
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handlers; use Gtk.Handlers;
with Glade2Ada_General; use Glade2Ada_General;
with Glade2Ada_Languages; use Glade2Ada_Languages;
with Glade2Ada_Languages_Dialog_CB; use Glade2Ada_Languages_Dialog_CB;

package body Glade2Ada_Languages_Dialog_Init is

   package Dialog_CBR is new
     Gtk.Handlers.Return_Callback (Gtk_Dialog_Record, Boolean);

   package Button is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

   procedure Gtk_New(LanDialog : out Lan_Dialog_Access) is
   begin
      LanDialog := new Lan_Dialog_Record;
      Glade2Ada_Languages_Dialog_Init.Init (LanDialog);
   end Gtk_New;

   procedure Init(LanDialog : access Lan_Dialog_Record'Class) is
      pragma Suppress (All_Checks);
      Pixmaps_Dir : constant String := "pixmaps/";
      str         : Unbounded_String;
   begin
      --------------------------------------------------------------------------
      --                                                                      --
      -- Creation of the Dialog Window                                        --
      --                                                                      --
      --------------------------------------------------------------------------
      Gtk.Dialog.Initialize (LanDialog);
      LanDialog.Set_Title (To_String(Lan_Dialog_Title(Lan)));
      Set_Position (LanDialog, Win_Pos_Center);
      Set_Modal (LanDialog, true);
      Set_Resizable (LanDialog, false);
      Set_Default_Size (LanDialog, Dialogsize_H, Dialogsize_V);
      if Set_Icon_From_File(LanDialog, To_String(Icon_Name)) then
         On_Lan_End(null);
      end if;
      --------------------------------------------------------------------------
      --                                                                      --
      -- Creation of the Basic Box                                            --
      --                                                                      --
      --------------------------------------------------------------------------
      LanDialog.Dialog_Box := Get_Content_Area(LanDialog);
      Gtk_New (LanDialog.Input_Box, Orientation_Vertical, 0);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Creation of the Box for the Combobox with label                      --
      --                                                                      --
      --------------------------------------------------------------------------
      Gtk_New (LanDialog.Combo_Box, Orientation_Horizontal, 0);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Creation of the Label and Packing it in the Input Box                --
      --                                                                      --
      --------------------------------------------------------------------------
      Gtk_New_with_Mnemonic (LanDialog.Combo_Label, To_String(Lan_Language(Lan)));
      Pack_Start
        (LanDialog.Combo_Box,
         LanDialog.Combo_Label,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Creation of the Combobox and packing it                              --
      --                                                                      --
      --------------------------------------------------------------------------
      Gtk_New (LanDialog.Combo_Inp);
      Pack_Start
        (LanDialog.Combo_Box,
         LanDialog.Combo_Inp,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Packing both in the Input box                                        --
      --                                                                      --
      --------------------------------------------------------------------------
      Pack_Start
        (LanDialog.Input_Box,
         LanDialog.Combo_Box,
         Expand  => True,
         Fill    => True,
         Padding => 5);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Packing the Mainbox in the inputbox                                  --
      --                                                                      --
      --------------------------------------------------------------------------
      Pack_Start
        (LanDialog.Dialog_Box,
         LanDialog.Input_Box,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Creation of the Button Box                                           --
      --                                                                      --
      --------------------------------------------------------------------------
      Gtk_New (LanDialog.Button_Box, Orientation_Horizontal, 0);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Creation of the Cancel Button                                        --
      --                                                                      --
      --------------------------------------------------------------------------
      Gtk_New (LanDialog.Cancel, To_String(Lan_Cancel(Lan)));
      --------------------------------------------------------------------------
      --                                                                      --
      -- Packing the Cancel Button in the Button Box                          --
      --                                                                      --
      --------------------------------------------------------------------------
      Pack_Start
        (LanDialog.Button_Box,
         LanDialog.Cancel,
         Expand  => True,
         Fill    => True,
         Padding => 2);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Creation of the OK Button                                            --
      --                                                                      --
      --------------------------------------------------------------------------
      Gtk_New (LanDialog.OK, To_String(Lan_OK(Lan)));
      --------------------------------------------------------------------------
      --                                                                      --
      -- Packing the OK Button in the Button Box                              --
      --                                                                      --
      --------------------------------------------------------------------------
      Pack_Start
        (LanDialog.Button_Box,
         LanDialog.OK,
         Expand  => True,
         Fill    => True,
         Padding => 2);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Packing the Button Box in the Main Box                               --
      --                                                                      --
      --------------------------------------------------------------------------
      Pack_Start
        (LanDialog.Dialog_Box,
         LanDialog.Button_Box,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Set a Size Request                                        --
      --                                                                      --
      --------------------------------------------------------------------------
      Set_Size_Request(LanDialog, Dialogsize_H, Dialogsize_V);
      --------------------------------------------------------------------------
      --                                                                      --
      -- Set the Signal events                                        --
      --                                                                      --
      --------------------------------------------------------------------------
      Dialog_CBR.Connect(LanDialog, "delete_event",
                         Dialog_CBR.To_Marshaller(On_Dialog_Delete_Event'Access));
      Button.Connect(LanDialog.OK, "clicked",
                     Button.To_Marshaller(On_Lan_Set'Access));
      Button.Connect(LanDialog.Cancel, "clicked",
                     Button.To_Marshaller(On_Lan_End'Access));
      --------------------------------------------------------------------------
      --                                                                      --
      -- Set All necessary Languages in the Combobox                          --
      --                                                                      --
      --------------------------------------------------------------------------
      for n in 0 .. nr_languages - 1 loop
         str := Language_Names(CH_Languages(n));
         Append_Text(LanDialog.Combo_Inp, To_String(str));
      end loop;
   end Init;

end Glade2Ada_Languages_Dialog_Init;
