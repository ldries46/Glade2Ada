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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Values;             use Glib.Values;
with Gtk;                     use Gtk;
with Gtk.Handlers;            use Gtk.Handlers;
with Gdk.Window;              use Gdk.Window;
with Gtk.Window;              use Gtk.Window;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Menu_bar;            use Gtk.Menu_bar;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Image_Menu_Item;     use Gtk.Image_Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Viewport;            use Gtk.Viewport;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Text_View;           use Gtk.Text_View;
with Gtk.Status_Bar;          use Gtk.Status_Bar;

package Glade_Test_Main_Init is

   type Main_Window_Record is new Gtk_Window_Record with record
      box1                         : Gtk_Box;
      menubar1                     : Gtk_Menu_bar;
      File                         : Gtk_Menu_Item;
      menu1                        : Gtk_Menu;
      New1                         : Gtk_Menu_Item;
      Open                         : Gtk_Menu_Item;
      Save                         : Gtk_Menu_Item;
      Save_As                      : Gtk_Image_Menu_Item;
      separatormenuitem1           : Gtk_Separator_Menu_Item;
      Languages                    : Gtk_Menu_Item;
      Properties                   : Gtk_Menu_Item;
      separatormenuitem2           : Gtk_Separator_Menu_Item;
      Print                        : Gtk_Menu_Item;
      Print_Options                : Gtk_Menu_Item;
      separatormenuitem3           : Gtk_Separator_Menu_Item;
      Quit                         : Gtk_Menu_Item;
      menuitem2                    : Gtk_Menu_Item;
      menu2                        : Gtk_Menu;
      imagemenuitem6               : Gtk_Image_Menu_Item;
      imagemenuitem7               : Gtk_Image_Menu_Item;
      imagemenuitem8               : Gtk_Image_Menu_Item;
      imagemenuitem9               : Gtk_Image_Menu_Item;
      menuitem3                    : Gtk_Menu_Item;
      menuitem4                    : Gtk_Menu_Item;
      menu3                        : Gtk_Menu;
      imagemenuitem10              : Gtk_Image_Menu_Item;
      viewport1                    : Gtk_Viewport;
      scrolledwindow1              : Gtk_Scrolled_Window;
      textview1                    : Gtk_Text_View;
      statusbar1                   : Gtk_Status_Bar;
   end record;

   type Main_Window_Access is access all Main_Window_Record'Class;
   
   MainSize_H  : Gint := 760;
   MainSize_V  : Gint := 670;
   Main_Window : Main_Window_Access;

   procedure Gtk_New(MainWindow : out Window_Access);
   procedure Init(MainWindow : access Window_Record'Class);
   procedure Set_Stausbar_Text(Bar : Gtk_Status_Bar;
                               Context_Des : UTF8_String;
                               Text : Unbounded_String);

end Glade_Test_Main_Init_Init;
