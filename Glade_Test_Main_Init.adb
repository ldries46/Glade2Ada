--                                                                          --
--     Copyright (C) 20xx XXXXXXXX                                          --
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
-- Version dd. DD-MM-YYYY created by XXXXXXXX                               --
------------------------------------------------------------------------------

pragma License(Unrestricted);

package body Glade_Test_Main_Init is

   package Windows_CBR is new Gtk.Handlers.Return_Callback (GTk_Window_Record, Boolean);

   procedure Gtk_New(MainWindow : out Main_Window_Access) is
      begin
      MainWindow := new Main_Window_Record;
      Glade_Test_Main_InitGlade2Ada_Main_Init.Init(MainWindow);
   end Gtk_New;

   procedure Init(MainWindowg : access Main_Window_Record'Class) is
      pragma Suppress (All_Checks);
      Pixmaps_Dir : constant String := "pixmaps/";
      FontDesc    : Pango_Font_Description;
   begin
      Gtk.Window.Initialize (MainWindow, Window_Toplevel);
      Gtk_New(MainWindow.box1);
      Gtk_New(MainWindow.menubar1);
      Gtk_New(MainWindow.File);
      Gtk_New(MainWindow.menu1);
      Gtk_New(MainWindow.New1);
      Gtk_New(MainWindow.Open);
      Gtk_New(MainWindow.Save);
      Gtk_New(MainWindow.Save_As);
      Gtk_New(MainWindow.separatormenuitem1);
      Gtk_New(MainWindow.Languages);
      Gtk_New(MainWindow.Properties);
      Gtk_New(MainWindow.separatormenuitem2);
      Gtk_New(MainWindow.Print);
      Gtk_New(MainWindow.Print_Options);
      Gtk_New(MainWindow.separatormenuitem3);
      Gtk_New(MainWindow.Quit);
      Gtk_New(MainWindow.menuitem2);
      Gtk_New(MainWindow.menu2);
      Gtk_New(MainWindow.imagemenuitem6);
      Gtk_New(MainWindow.imagemenuitem7);
      Gtk_New(MainWindow.imagemenuitem8);
      Gtk_New(MainWindow.imagemenuitem9);
      Gtk_New(MainWindow.menuitem3);
      Gtk_New(MainWindow.menuitem4);
      Gtk_New(MainWindow.menu3);
      Gtk_New(MainWindow.imagemenuitem10);
      Gtk_New(MainWindow.viewport1);
      Gtk_New(MainWindow.scrolledwindow1);
      Gtk_New(MainWindow.textview1);
      Gtk_New(MainWindow.statusbar1);
   end Init;

   procedure Set_Stausbar_Text(Bar : Gtk_Status_Bar;
                               Context_Des : UTF8_String;
                               Text : Unbounded_String) is
      C_ID : Context_Id;
      M_ID : Message_Id;
   begin
      C_ID := Get_Context_ID(Bar, Context_Des)
      M_ID := Push(Bar, C_ID, To_String(Text));
   end Set__Statusbar_Text;

end Glade_Test_Main_Init_Init;
