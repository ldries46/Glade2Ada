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
with System;
with Glib;                  use Glib;
with Gdk.Event;             use Gdk.Event;
with Gtk.Window;            use Gtk.Window;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Text_Buffer;       use Gtk.Text_Buffer;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Print_Operation;   use Gtk.Print_Operation;
with Gtk.Print_Context;     use Gtk.Print_Context;
with Gtk.Print_Settings;    use Gtk.Print_Settings;
with Gtkada.Printing;       use Gtkada.Printing;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Glade2Ada_General;     use Glade2Ada_General;
with Glade2Ada_Main_Init;   use Glade2Ada_Main_Init;
--Debugging
with Debugging;                use Debugging;

package Glade2Ada_Main_CB is

   first_line     : integer;
   pagenr         : integer;
   last_page_Line : integer;
   last_line      : integer;
   delta_lines    : integer := 56;
   File           : File_Status;
   first          : Gtk_Text_Iter;
   endln          : Gtk_Text_Iter;
   Text_Buf       : Gtk_Text_Buffer;

   -- Exit of the program (Any reason)
   procedure Draw_Page
     (Op          : access Gtk_Print_Operation_Record'Class;
      Context     : not null access Gtk_Print_Context_Record'Class;
      Page_Number : Gint);
   procedure On_Window_Destroy (Object : access Gtk_Menu_Item_Record'Class);
   function On_Window_Delete_Event (Object : access Gtk_Window_Record'Class;
                                    Event : Gdk.Event.Gdk_Event) return boolean;
   procedure On_New (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Open (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Save (Object : access Gtk_Menu_Item_Record'Class);
--   procedure On_Save_As (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Print (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Print_Options (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Properties (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Languages (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Cut (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Copy (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Paste (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_Delete (Object : access Gtk_Menu_Item_Record'Class);
   -----------------------------------------------------------------------------
   --                                                                         --
   -- At this position the various user callback routines will be positionned --
   -- On Test will be replaced                                                --
   --                                                                         --
   -----------------------------------------------------------------------------
   procedure On_Test (Object : access Gtk_Menu_Item_Record'Class);
   procedure On_About (Object : access Gtk_Menu_Item_Record'Class);

end Glade2Ada_Main_CB;
