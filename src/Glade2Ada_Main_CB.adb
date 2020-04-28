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
with Cairo;                           use Cairo;
with Glib;                            use Glib;
with Gtk.Main;
with Gtk.Tree_Model;
with Gtk.GEntry;                      use Gtk.GEntry;
with Gtk.Enums;                       use Gtk.Enums;
with Gtk.Combo_Box_Text;              use Gtk.Combo_Box_Text;
with Gtk.Text_Buffer;                 use Gtk.Text_Buffer;
with Gtk.Text_Iter;                   use Gtk.Text_Iter;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Program_Init;                    use Program_Init;
with Glade2Ada_FC_Init;               use Glade2Ada_FC_Init;
with Glade2Ada_Languages;             use Glade2Ada_Languages;
with Glade2Ada_Languages_Dialog_Init; use Glade2Ada_Languages_Dialog_Init;
with Glade2Ada_Main_Init;             use Glade2Ada_Main_Init;
with Glade2Ada_General;               use Glade2Ada_General;
with Glade_Name_Init;                 use Glade_Name_Init;

package body Glade2Ada_Main_CB is

   procedure Draw_Page
     (Op          : access Gtk_Print_Operation_Record'Class;
      Context     : not null access Gtk_Print_Context_Record'Class;
      Page_Number : Gint) is
      Cr    : Cairo_Context;
      nr    : integer;
      width : integer;
      rb    : boolean;
      ok    : boolean;
      str   : Unbounded_String;
   begin
      nr := first_line;
      Cr := Get_Cairo_Context (Context);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Select_font_face(Cr, "Monospace", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
      Set_Font_Size(Cr, GDouble(9));
      if nr = 1 then
         pagenr := 1;
      else
         pagenr := pagenr + 1;
      end if;
      if pagenr rem 2 = 1 then
         width := 70;
      else
         width := 40;
      end if;
      if last_page_line > last_line then
         last_page_line := last_line;
      end if;
      ok := false;
      while not ok loop
         Get_Iter_At_Line(Text_Buf, first, Gint(nr));
         Get_Iter_At_Line_Offset(Text_Buf, endln, Gint(nr), 500);
         move_to(cr, Gdouble(width), Gdouble((nr - first_line + 1) * 13 + 65));
         str := To_Unbounded_String(Get_Text(Text_Buf,first,endln));
         show_text(cr, To_String(str));
         nr := nr + 1;
         ok := nr >= last_page_line;
         if nr <= last_line then
            Forward_Line(first, rb);
         end if;
      end loop;
      first_line := nr;
      last_page_line := last_page_line + delta_lines;
   end Draw_Page;

   -----------------------------------------------------------------------------
   -- Internal procedure End_Program used in the Quit and Exit commands
   -----------------------------------------------------------------------------
   procedure End_Program is
   begin
      Close_Ini;
      Gtk.Main.Main_Quit;
   end End_Program;

   -----------------------------------------------------------------------------
   -- Internal procedure Stop ile chooser Buttons
   -----------------------------------------------------------------------------
   procedure Stop_File_Chooser is
   begin
      Close_Ini;
      Gtk.Main.Main_Quit;
   end Stop_File_Chooser;

   -----------------------------------------------------------------------------
   -- Procedure On Window_Destroy used in the Destroy command
   -----------------------------------------------------------------------------
   procedure On_Window_Destroy (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      -- At this point the ini files must be closed. All ini files are updated
      -- and closed with one call to Close_IIni
      -- If no Init Files are used then this call can be deleted
      End_Program;
   end On_Window_Destroy;

   -----------------------------------------------------------------------------
   -- Procedure On Window_Delete_Event used in the Delete_Event
   -----------------------------------------------------------------------------
   function On_Window_Delete_Event (Object : access Gtk_Window_Record'Class;
                                    Event : Gdk.Event.Gdk_Event) return boolean is
      pragma Unreferenced (Object);
   begin
      End_Program;
      return true;
   end On_Window_Delete_Event;

   -----------------------------------------------------------------------------
   -- Procedure On_New used in the File New command
   -----------------------------------------------------------------------------
   procedure On_New (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
      S_I : Gtk_Text_Iter;
      E_I : Gtk_Text_Iter;
   begin
      follow := false;
      Glade_Buffer.Get_Start_Iter(S_I);
      Glade_Buffer.Get_End_Iter(E_I);
      Glade_Buffer.Delete(S_I, E_I);
      Spec_Buffer.Get_Start_Iter(S_I);
      Spec_Buffer.Get_End_Iter(E_I);
      Spec_Buffer.Delete(S_I, E_I);
      Body_Buffer.Get_Start_Iter(S_I);
      Body_Buffer.Get_End_Iter(E_I);
      Body_Buffer.Delete(S_I, E_I);
      File_Name := To_Unbounded_String(Default_Name);
   end On_New;

   -----------------------------------------------------------------------------
   -- Procedure On_Open used in the File Open command
   -----------------------------------------------------------------------------
   procedure On_Open (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      File := Open;
      File_Open_Dialog.Show_All;
   end On_Open;

   -----------------------------------------------------------------------------
   -- Procedure Write_File which writes the file in bothSave and Save As cases
   -----------------------------------------------------------------------------
   procedure Write_File is
   begin
--      if Response = Gtk_Response_OK then
--         File_Name := To_Unbounded_String(Get_Current_Folder(File_Dialog));
         -- Open the file, write it and close it
         null;
--      end if;
   end Write_File;

    -----------------------------------------------------------------------------
   -- Procedure On_Save used in the File Save command
   -----------------------------------------------------------------------------
   procedure On_Save (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      if follow then
         On_Save_As(null);
      else
         File := Save;
         Write_File;
      end if;
   end On_Save;

   -----------------------------------------------------------------------------
   -- Procedure On_Save_As used in the File Save As command
   -----------------------------------------------------------------------------
   procedure On_Save_As (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      File := Save;
      File_Save_Dialog.Show_All;
      Write_File;
   end On_Save_As;

   -----------------------------------------------------------------------------
   -- Procedure On_Print used in the File Print command
   -----------------------------------------------------------------------------
   procedure On_Print (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
      nr_Pages  : Gint;
      len       : Gint;
      nr        : Gint;
      Number    : long_float;
      str       : Unbounded_String;
      Print_Op  : Gtkada_Print_Operation;
      Result    : Gtk_Print_Operation_Result;
      Print_Set : Gtk_Print_Settings;
      start_it  : Gtk_Text_Iter;
      start     : Gtk_Text_Iter;
      end_it    : Gtk_Text_Iter;
      stop      : Gtk_Text_Iter;
      Draw      : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void
                  := Draw_Page'Access;
   begin
      Ar_Text_Buf(1) := Glade_Buffer;
      Ar_Text_Buf(2) := Spec_Buffer;
      Ar_Text_Buf(3) := Body_Buffer;
      for n in 1 .. 3 loop
         first_line := 1;
         nr_Pages := 0;
         Text_Buf := Ar_Text_Buf(n);
         Last_page_line := delta_lines + 1;
         Get_Start_Iter(Text_Buf, start_it);
         Get_End_Iter(Text_Buf, end_it);
         len := Text_Buf.Get_Line_Count;
         nr := 1;
         while nr < len - 1 loop
            Get_Iter_At_Line(Text_Buf, start, nr);
            Get_Iter_At_Line_Offset(Text_Buf, stop, nr, 500);
            str := To_Unbounded_String(Text_Buf.Get_Text(start, stop));
            nr := nr + 1;
         end loop;
         last_line := integer(nr) - 1;
         Print_Op := new Gtkada_Print_Operation_Record;
         Gtkada.Printing.Initialize (Print_Op);
         Gtk_New(Print_Set);
         Set_Current_Page (Print_Op, 1);
         Number := long_float(last_line) / long_float(delta_lines);
         Number := long_float'Rounding(Number + 0.4999999);
         nr_Pages := Gint(Number);
         Set_N_Pages (Print_Op, nr_Pages);
         Set_Use_Full_Page(Print_Op, true);
         Set_Unit (Print_Op, points);
         Print_Op.Set_Print_Settings(Print_Set);
--------------------------------------------------------------------
--   procedure On_Draw_Page
--      (Self  : not null access Gtk_Print_Operation_Record;
--       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void;
--       After : Boolean := False);
--------------------------------------------------------------------
         On_Draw_Page(Print_Op, Draw);
         Result := Connect_and_Run(Print_Op ,Action_Print_Dialog, Main_Window);
      end loop;
  end On_Print;

   -----------------------------------------------------------------------------
   -- Procedure On_Print_Options used in the File Print Options command
   -----------------------------------------------------------------------------
   procedure On_Print_Options (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      null;
   end On_Print_Options;

   -----------------------------------------------------------------------------
   -- Procedure On_Properties used in the File Properties command
   -----------------------------------------------------------------------------
   procedure On_Properties (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      null;
   end On_Properties;

   -----------------------------------------------------------------------------
   -- Procedure On_Languages used in the File Languages command
   -----------------------------------------------------------------------------
   procedure On_Languages (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      Set_Active(Lan_Dialog.Combo_Inp, Lan_ID);
      Lan_Dialog.Show_All;
   end On_Languages;

   -----------------------------------------------------------------------------
   -- Procedure On_Cut used in the Edit Cut command
   -----------------------------------------------------------------------------
   procedure On_Cut (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      null;
   end On_Cut;

  -----------------------------------------------------------------------------
   -- Procedure On_Copy used in the Edit Copy command
   -----------------------------------------------------------------------------
   procedure On_Copy (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      null;
   end On_Copy;

   -----------------------------------------------------------------------------
   -- Procedure On_Paste used in the Edit Paste command
   -----------------------------------------------------------------------------
   procedure On_Paste (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      null;
   end On_Paste;

   -----------------------------------------------------------------------------
   -- Procedure On_Delete used in the Edit Delete command
   -----------------------------------------------------------------------------
   procedure On_Delete (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      null;
   end On_Delete;

   -----------------------------------------------------------------------------
   --                                                                         --
   -- At this position the various user callback routines will be posiyionned --
   --                                                                         --
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -- Procedure On_Test used in the Glade2Ada Test command
   -- This routine will be replaced by various others
   -----------------------------------------------------------------------------
   procedure On_Test (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
      n    : integer := 0;
      str  : Unbounded_String;
   begin
      Glade_Name_Init.Dialog_Open := true;
--      Set_Text(Glade_Name_Init.Main_Dialog.Str_Entry,
--               To_String(Glade_Name_Init.last_txt));
      Glade_Name_Init.Main_Dialog.Check_But.Set_active(Glade_Name_Init.mode);
      Glade_Name_Init.Main_Dialog.Radio1_But.Set_Active(Glade_Name_Init.mode_Radio1);
      Glade_Name_Init.Main_Dialog.Radio2_But.Set_Active(Glade_Name_Init.mode_Radio2);
      Glade_Name_Init.Main_Dialog.Radio3_But.Set_Active(Glade_Name_Init.mode_Radio3);
      --------------------------------------------------------------------------
      -- Example of filling a combobox                                        --
      --------------------------------------------------------------------------
      Remove_All(Glade_Name_Init.Main_Dialog.Str_Combo);
      while n < 10 loop
        str := To_Unbounded_String(integer'Image(n));
         Append_Text(Glade_Name_Init.Main_Dialog.Str_Combo,
                     To_String(str));
         n := n + 1;
      end loop;
      Set_Active(Glade_Name_Init.Main_Dialog.Str_Combo,
                 Glade_Name_Init.n_ID);
      Glade_Name_Init.Main_Dialog.Show_All;
   end On_Test;

    -----------------------------------------------------------------------------
   -- Procedure On_About used in the Help About command
   -----------------------------------------------------------------------------
   procedure On_About (Object : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Object);
   begin
      null;
   end On_About;

end Glade2Ada_Main_CB;
