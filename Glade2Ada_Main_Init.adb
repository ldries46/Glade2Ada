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
with Pango.Font;                       use Pango.Font;
with Gdk.Types;                        use Gdk.Types;
with Gtk.Widget;                       use Gtk.Widget;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Handlers;                     use Gtk.Handlers;
With Gtk.Marshallers;                  use Gtk.Marshallers;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
-- With statements for Dialogs
with Glade_Name_Init;
with Glade2Ada_FC_Init;               use Glade2Ada_FC_Init;
with Glade2Ada_Main_CB;               use Glade2Ada_Main_CB;
with Glade2Ada_General;               use Glade2Ada_General;
with Glade2Ada_FC_Init;               use Glade2Ada_FC_Init;
with Glade2Ada_Languages;             use Glade2Ada_Languages;
with Glade2Ada_Languages_Dialog_Init; use Glade2Ada_Languages_Dialog_Init;

package body Glade2Ada_Main_Init is

   package Windows_CBR is new
     Gtk.Handlers.Return_Callback (GTk_Window_Record, Boolean);

   procedure Gtk_New(MainWindow : out Main_Window_Access) is
   begin
      MainWindow := new Main_Window_Record;
      Glade2Ada_Main_Init.Init (MainWindow);
   end Gtk_New;

   procedure Init(MainWindow : access Main_Window_Record'Class) is
      pragma Suppress (All_Checks);
      Pixmaps_Dir : constant String := "pixmaps/";
      FontDesc    : Pango_Font_Description;
   begin
      Gtk.Window.Initialize (MainWindow, Window_Toplevel);
      MainWindow.Set_Title (To_String(Lan_Window_Title(Lan)));
      Set_Position (MainWindow, Win_Pos_Center);
      Set_Modal (MainWindow, False);
      Set_Resizable (MainWindow, True);
      Set_Default_Size (MainWindow, Mainsize_H, Mainsize_V);
      if Set_Icon_From_File(MainWindow, To_String(Icon_Name)) then
         On_Window_Destroy(null);
      end if;
      Gtk_New (MainWindow.Main_Box1, Orientation_Vertical, 0);
      Gtk_New (MainWindow.Text_Box, Orientation_Horizontal, 0);
      Gtk_New (MainWindow.Text_Box_G, Orientation_Vertical, 0);
      Gtk_New (MainWindow.Text_Box_S, Orientation_Vertical, 0);
      Gtk_New (MainWindow.Text_Box_B, Orientation_Vertical, 0);
      -- Build the Menu bar
      Gtk_New (MainWindow.Main_Bar);
      -- Place the File Menu
      Gtk_New_With_Mnemonic (MainWindow.File, To_String(Lan_File(Lan)));
      Gtk_New (MainWindow.File_Menu);
      Gtk_New (MainWindow.New_Project, To_String(Lan_New(Lan)));
      Append (MainWindow.File_Menu, MainWindow.New_Project);
      Gtk_New (MainWindow.Open, To_String(Lan_Open(Lan)));
      Append (MainWindow.File_Menu, MainWindow.Open);
      Gtk_New (MainWindow.Save, To_String(Lan_Save(Lan)));
      Append (MainWindow.File_Menu, MainWindow.Save);
--      Gtk_New (MainWindow.Save_As, To_String(Lan_Save_As(Lan)));
--      Append (MainWindow.File_Menu, MainWindow.Save_As);
      Gtk_New (MainWindow.Separator1);
      Append (MainWindow.File_Menu, MainWindow.Separator1);
      Gtk_New (MainWindow.Print, To_String(Lan_Print(Lan)));
      Append (MainWindow.File_Menu, MainWindow.Print);
      Gtk_New_With_Mnemonic (MainWindow.Print_Options, To_String(Lan_Print_Option(Lan)));
      Append (MainWindow.File_Menu, MainWindow.Print_Options);
      Gtk_New (MainWindow.Separator2);
      Append (MainWindow.File_Menu, MainWindow.Separator2);
      Gtk_New (MainWindow.Properties, To_String(Lan_Properties(Lan)));
      Append (MainWindow.File_Menu, MainWindow.Properties);
      Gtk_New (MainWindow.Languages, To_String(Lan_Language(Lan)));
      Append (MainWindow.File_Menu, MainWindow.Languages);
      Gtk_New (MainWindow.Separator1);
      Append (MainWindow.File_Menu, MainWindow.Separator1);
      Gtk_New (MainWindow.Quit, To_String(Lan_Quit(Lan)));
      Append (MainWindow.File_Menu, MainWindow.Quit);
      Set_Submenu (MainWindow.File, MainWindow.File_Menu);
      Append (MainWindow.Main_Bar, MainWindow.File);
      -- Place the Edit Menu
      Gtk_New_With_Mnemonic (MainWindow.Edit, To_String(Lan_Edit(Lan)));
      Gtk_New (MainWindow.Edit_Menu);
      Gtk_New (MainWindow.Cut, To_String(Lan_Cut(Lan)));
      Append (MainWindow.Edit_Menu, MainWindow.Cut);
      Gtk_New (MainWindow.Copy, To_String(Lan_Copy(Lan)));
      Append (MainWindow.Edit_Menu, MainWindow.Copy);
      Gtk_New (MainWindow.Paste, To_String(Lan_Paste(Lan)));
      Append (MainWindow.Edit_Menu, MainWindow.Paste);
      Gtk_New (MainWindow.Delete, To_String(Lan_Delete(Lan)));
      Append (MainWindow.Edit_Menu, MainWindow.Delete);
      Set_Submenu (MainWindow.Edit, MainWindow.Edit_Menu);
      Append (MainWindow.Main_Bar, MainWindow.Edit);
      --------------------------------------------------------------------------
      -- This menu item should be completly changes because program specific  --
      -- items will be directed from here. This means that more or less       --
      -- main menitems ans sub menu items are possible                        --
      --------------------------------------------------------------------------
      -- Place the Test_Dialog Menu
      Gtk_New_With_Mnemonic (MainWindow.Glade2Ada, "_Glade2Ada");
      Gtk_New (MainWindow.Glade2Ada_Menu);
      Gtk_New (MainWindow.Test, "Test");
      Append (MainWindow.Glade2Ada_Menu, MainWindow.Test);
      Set_Submenu (MainWindow.Glade2Ada, MainWindow.Glade2Ada_Menu);
      Append (MainWindow.Main_Bar, MainWindow.Glade2Ada);
      -- Place the View Menu
      Gtk_New_With_Mnemonic (MainWindow.View, To_String(Lan_View(Lan)));
      Append (MainWindow.Main_Bar, MainWindow.View);
      -- Place the Help Menu
      Gtk_New_With_Mnemonic (MainWindow.Help, To_String(Lan_Help(Lan)));
      Gtk_New (MainWindow.Help_Menu);
      Gtk_New_With_Mnemonic (MainWindow.About, To_String(Lan_About(Lan)));
      Append (MainWindow.Help_Menu, MainWindow.About);
      Set_Submenu (MainWindow.Help, MainWindow.Help_Menu);
      Append (MainWindow.Main_Bar, MainWindow.Help);
      Pack_Start
        (MainWindow.Main_Box1,
         MainWindow.Main_Bar,
         Expand  => False,
         Fill    => False,
         Padding => 0);
      Gtk_New (MainWindow.Scrolledwindow_G);
      Set_Policy (MainWindow.Scrolledwindow_G, Policy_Always, Policy_Always);
      Set_Shadow_Type (MainWindow.Scrolledwindow_G, Shadow_None);
      Set_Placement (MainWindow.Scrolledwindow_G, Corner_Top_Left);
      Gtk_New (MainWindow.Scrolledwindow_S);
      Set_Policy (MainWindow.Scrolledwindow_S, Policy_Always, Policy_Always);
      Set_Shadow_Type (MainWindow.Scrolledwindow_S, Shadow_None);
      Set_Placement (MainWindow.Scrolledwindow_S, Corner_Top_Left);
      Gtk_New (MainWindow.Scrolledwindow_B);
      Set_Policy (MainWindow.Scrolledwindow_B, Policy_Always, Policy_Always);
      Set_Shadow_Type (MainWindow.Scrolledwindow_B, Shadow_None);
      Set_Placement (MainWindow.Scrolledwindow_B, Corner_Top_Left);
      Gtk_New (MainWindow.Textview_G);
      Set_Editable (MainWindow.Textview_G, False);
      Set_Justification (MainWindow.Textview_G, Justify_Left);
      Set_Wrap_Mode (MainWindow.Textview_G, Wrap_None);
      Set_Cursor_Visible (MainWindow.Textview_G, True);
      Set_Pixels_Above_Lines (MainWindow.Textview_G, 0);
      Set_Pixels_Below_Lines (MainWindow.Textview_G, 0);
      Set_Pixels_Inside_Wrap (MainWindow.Textview_G, 0);
      Set_Left_Margin (MainWindow.Textview_G, 0);
      Set_Right_Margin (MainWindow.Textview_G, 0);
      Set_Indent (MainWindow.Textview_G, 0);
      Gtk_New (MainWindow.Textview_S);
      Set_Editable (MainWindow.Textview_S, True);
      Set_Justification (MainWindow.Textview_S, Justify_Left);
      Set_Wrap_Mode (MainWindow.Textview_S, Wrap_None);
      Set_Cursor_Visible (MainWindow.Textview_S, True);
      Set_Pixels_Above_Lines (MainWindow.Textview_S, 0);
      Set_Pixels_Below_Lines (MainWindow.Textview_S, 0);
      Set_Pixels_Inside_Wrap (MainWindow.Textview_S, 0);
      Set_Left_Margin (MainWindow.Textview_S, 0);
      Set_Right_Margin (MainWindow.Textview_S, 0);
      Set_Indent (MainWindow.Textview_S, 0);
      Gtk_New (MainWindow.Textview_B);
      Set_Editable (MainWindow.Textview_B, True);
      Set_Justification (MainWindow.Textview_B, Justify_Left);
      Set_Wrap_Mode (MainWindow.Textview_B, Wrap_None);
      Set_Cursor_Visible (MainWindow.Textview_B, True);
      Set_Pixels_Above_Lines (MainWindow.Textview_B, 0);
      Set_Pixels_Below_Lines (MainWindow.Textview_B, 0);
      Set_Pixels_Inside_Wrap (MainWindow.Textview_B, 0);
      Set_Left_Margin (MainWindow.Textview_B, 0);
      Set_Right_Margin (MainWindow.Textview_B, 0);
      Set_Indent (MainWindow.Textview_B, 0);
      declare
         Iter : Gtk_Text_Iter;
      begin
         Get_Iter_At_Line (Get_Buffer (MainWindow.Textview_G), Iter, 0);
         Insert (Get_Buffer (MainWindow.Textview_G), Iter, "");
         Get_Iter_At_Line (Get_Buffer (MainWindow.Textview_S), Iter, 0);
         Insert (Get_Buffer (MainWindow.Textview_S), Iter, "");
         Get_Iter_At_Line (Get_Buffer (MainWindow.Textview_B), Iter, 0);
         Insert (Get_Buffer (MainWindow.Textview_B), Iter, "");
      end;
      Add (MainWindow.Scrolledwindow_G, MainWindow.Textview_G);
      Add (MainWindow.Scrolledwindow_S, MainWindow.Textview_S);
      Add (MainWindow.Scrolledwindow_B, MainWindow.Textview_B);
      Gtk_New (MainWindow.Statusbar_G);
      Gtk_New (MainWindow.Statusbar_S);
      Gtk_New (MainWindow.Statusbar_B);
      Pack_Start
        (MainWindow.Text_Box_G,
         MainWindow.Scrolledwindow_G,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainWindow.Text_Box_G,
         MainWindow.Statusbar_G,
         Expand  => False,
         Fill    => False,
         Padding => 0);
      Pack_Start
        (MainWindow.Text_Box_S,
         MainWindow.Scrolledwindow_S,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainWindow.Text_Box_S,
         MainWindow.Statusbar_S,
         Expand  => False,
         Fill    => False,
         Padding => 0);
      Pack_Start
        (MainWindow.Text_Box_B,
         MainWindow.Scrolledwindow_B,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainWindow.Text_Box_B,
         MainWindow.Statusbar_B,
         Expand  => False,
         Fill    => False,
         Padding => 0);
       Pack_Start
        (MainWindow.Text_Box,
         MainWindow.Text_Box_G,
         Expand  => True,
         Fill    => True,
         Padding => 0);
       Pack_Start
        (MainWindow.Text_Box,
         MainWindow.Text_Box_S,
         Expand  => True,
         Fill    => True,
         Padding => 0);
       Pack_Start
        (MainWindow.Text_Box,
         MainWindow.Text_Box_B,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Pack_Start
        (MainWindow.Main_Box1,
         MainWindow.Text_Box,
         Expand  => True,
         Fill    => True,
         Padding => 0);
      Add (MainWindow, MainWindow.Main_Box1);
      Gtk_New(Glade_Buffer);
      Gtk_New(Spec_Buffer);
      Gtk_New(Body_Buffer);
      MainWindow.Textview_G.Set_Buffer(Glade_Buffer);
      MainWindow.Textview_S.Set_Buffer(Spec_Buffer);
      MainWindow.Textview_B.Set_Buffer(Body_Buffer);
      Set_Size_Request (MainWindow, Mainsize_H, Mainsize_V );
      Grab_Default (MainWindow);
      --  Connect signals
      Windows_CBR.Connect
        (MainWindow, "delete_event",
         Windows_CBR.To_Marshaller(On_Window_Delete_Event'Access));
      On_Activate(MainWindow.New_Project, On_New'Access);
      Activate(MainWindow.Open);
      On_Activate(MainWindow.Open, On_Open'Access);
      Activate(MainWindow.Save);
      On_Activate(MainWindow.Save, On_Save'Access);
--      Activate(MainWindow.Save_As);
--      On_Activate(MainWindow.Save_As, On_Save_As'Access);
      Activate(MainWindow.Print);
      On_Activate(MainWindow.Print, On_Print'Access);
      Activate(MainWindow.Print_Options);
      On_Activate(MainWindow.Print_Options, On_Print_Options'Access);
      Activate(MainWindow.Properties);
      On_Activate(MainWindow.Properties, On_Properties'Access);
      Activate(MainWindow.Languages);
      On_Activate(MainWindow.Languages, On_Languages'Access);
      Activate(MainWindow.Quit);
      On_Activate(MainWindow.Quit, On_Window_Destroy'Access);
      Activate(MainWindow.Cut);
      On_Activate(MainWindow.Cut, On_Cut'Access);
      Activate(MainWindow.Copy);
      On_Activate(MainWindow.Copy, On_Copy'Access);
      Activate(MainWindow.Paste);
      On_Activate(MainWindow.Paste, On_Paste'Access);
      Activate(MainWindow.Delete);
      On_Activate(MainWindow.Delete, On_Delete'Access);
      -- Activate user program menuitems
      Activate(MainWindow.Test);
      On_Activate(MainWindow.Test, On_Test'Access);
      -- End Activate user program menuitems
      Activate(MainWindow.About);
      On_Activate(MainWindow.About, On_About'Access);
      Set_Statusbar_Text(MainWindow.Statusbar_G, "File_Type", Pack_Name & Glade_File(Lan));
      Set_Statusbar_Text(MainWindow.Statusbar_S, "File_Type", Pack_Name & Specification_File(Lan));
      Set_Statusbar_Text(MainWindow.Statusbar_B, "File_Type", Pack_Name & Body_File(Lan));
      FontDesc := From_String("Monospace 9");
      MainWindow.TextView_G.Modify_Font(Fontdesc);
      MainWindow.TextView_S.Modify_Font(Fontdesc);
      MainWindow.TextView_B.Modify_Font(Fontdesc);
      -- Initiation of Dialogs
      --------------------------------------------------------------------------
      -- Change "Glade_Name" in the name of the dialog                   --
      -- If necesary enter more dialogs or remove this statement              --
      --------------------------------------------------------------------------
      Gtk_New (Glade_Name_Init.Main_Dialog);
      Init_Open;
--      Init_Save;
      Init_Select_Folder;
      Gtk_New (Glade2Ada_Languages_Dialog_Init.Lan_Dialog);
      --------------------------------------------------------------------------
      -- Other initiation staments
      --------------------------------------------------------------------------
   end Init;

   procedure Set_Statusbar_Text( Bar : Gtk_Status_Bar; Context_Des : UTF8_String; Text : Unbounded_String) is
      C_ID : Context_Id;
      M_ID : Message_ID;
   begin
      C_ID := Get_Context_Id(Bar, Context_Des );
      M_ID := Push(Bar, C_ID, To_String(Text));
   end Set_Statusbar_Text;

end Glade2Ada_Main_Init;
