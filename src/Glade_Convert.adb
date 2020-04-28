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

with System;
with Glib;                     use Glib;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Glade2Ada_General;        use Glade2Ada_General;
with Glade2Ada_Main_Init;      use Glade2Ada_Main_Init;

--Debugging
with Debugging;                use Debugging;

Package body Glade_Convert is

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   function SP return String is
      str : Unbounded_String := Empty_String;
   begin
      for n in 1 .. Level_I loop
         str := str & "  ";
      end loop;
      return To_String(str);
   end SP;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   procedure In_At_Cursor(Buffer : Gtk_Text_Buffer; str : String) is
      str1 : Unbounded_String := To_Unbounded_String(str);
   begin
      if Buffer = Spec_Buffer then
         Init_S.Set_Buffer(str1);
      else
         Init_B.Set_Buffer(str1);
      end if;
   end In_At_Cursor;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   function buf_String(str : Unbounded_String; level : integer := 0; CR : boolean := false) return string is
      start     : Unbounded_String := Empty_String;
      str1      : Unbounded_String;
   begin
      for n in 1 .. level loop
         start := start & "   ";
      end loop;
      str1 := start & str;
      if CR then
         str1 := str1 & Glade2Ada_General.CRLF;
      end if;
      return To_String(str1);
   end buf_String;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   procedure Read_File is
      str    : Unbounded_String;
      C_File : File_Type;
   begin
      glade_lines := 0;
      Open(C_File, In_File, To_String(Inp_File));
      while not End_of_File(C_File) loop
         str := To_Unbounded_String(Get_Line(C_File));
         Buffer_G.Set_Buffer(str);
         glade_lines := glade_lines + 1;
      end loop;
      Close(C_File);
      for n in 1 .. Buffer_G.Length loop
         Insert_At_Cursor(Glade_Buffer, buf_String(Buffer_G.Get_Buffer(n), 0, true));
      end loop;
      convert;
   end Read_File;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   procedure With_Line(Buffer : Gtk_Text_Buffer; Win : Items) is
      len  : integer;
      str  : Unbounded_String;
      str1 : Unbounded_String;
   begin
      str := Item_Names(Win, 2) & ";";
      Replace_Slice(str, 4, 4, ".");
      len := use_dist - 1 - Length(str);
      str1 := To_Unbounded_String("with ") & str;
      for n in 1 .. len loop
         str1 := str1 & " ";
      end loop;
      str1 := str1 & "use " & str;
      if str1 /= Empty_String then
         if Buffer = Spec_Buffer then
            Init_S.Insert_Buffer(str1, Spec_Mark);
         end if;
      end if;
   end With_Line;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   procedure With_Buffer(Buffer : Gtk_Text_Buffer; Win : Items) is
      n       : integer;
      len     : integer;
      present : boolean := false;
      it      : Items;
   begin
      n := 1;
      if Buffer = Spec_Buffer then
         len := With_Spec.Length;
         while n <= len and not present loop
            it := With_Spec.Get_Buffer(n);
            present := it = Win;
            n := n + 1;
         end loop;
         if not present then
            With_Spec.Set_Buffer(Win);
         end if;
      else
         len := With_Body.Length;
         while n <= len and not present loop
            it := With_Body.Get_Buffer(n);
            present := it = Win;
            n := n + 1;
         end loop;
         if not present then
            With_Body.Set_Buffer(Win);
         end if;
      end if;
   end With_Buffer;

   -----------------------------------------------------------------------------
   -- procedure insert_license(Buffer : Gtk_Text_Buffer)                      --
   -- insert_licence writes the GNU General Puiblic License statement at the  --
   -- start of the .ads and .adb files and some other generally neccessary    --
   -- statements
   -----------------------------------------------------------------------------
   procedure insert_license(Buffer : Gtk_Text_Buffer) is
      str       : Unbounded_String;
      End_Iter  : Gtk_Text_Iter;
   begin
      if not follow then
         In_At_Cursor(Buffer, To_String(Empty_String));
         In_At_Cursor(Buffer, To_String(Empty_String));
      else
         follow := true;
      end if;
      str := To_Unbounded_String("------------------------------------------------------------------------------");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("--                                                                          --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("--     Copyright (C) 20xx XXXXXXXX                                          --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("--                                                                          --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- This program is free software;  you can redistribute it and/or modify it --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- under terms of the  GNU General Public License  as published by the Free --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- Software  Foundation;  either version 3,  or (at your  option) any later --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- version. This library is distributed in the hope that it will be useful, --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("--                                                                          --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- As a special exception under Section 7 of GPL version 3, you are granted --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- additional permissions described in the GCC Runtime Library Exception,   --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- version 3.1, as published by the Free Software Foundation.               --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("--                                                                          --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- You should have received a copy of the GNU General Public License and    --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- a copy of the GCC Runtime Library Exception along with this program;     --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- <http://www.gnu.org/licenses/>.                                          --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("--                                                                          --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("------------------------------------------------------------------------------");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("-- Version dd. DD-MM-YYYY created by XXXXXXXX                               --");
      In_At_Cursor(Buffer, buf_String(str));
      str := To_Unbounded_String("------------------------------------------------------------------------------");
      In_At_Cursor(Buffer, buf_String(str));
      In_At_Cursor(Buffer, buf_String(Empty_String));
      str := To_Unbounded_String("pragma License(Unrestricted)");
      In_At_Cursor(Buffer, buf_String(str));
      Get_End_Iter(Buffer, End_Iter);
      In_At_Cursor(Buffer, buf_String(Empty_String));
      if Buffer = Spec_Buffer then
         str := To_Unbounded_String("with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;");
         In_At_Cursor(Buffer, buf_String(str));
         str := To_Unbounded_String("with Glib;                    use Glib;");
         In_At_Cursor(Buffer, buf_String(str));
         str := To_Unbounded_String("with Glib.Object;             use Glib.Object;");
         In_At_Cursor(Buffer, buf_String(str));
         str := To_Unbounded_String("with Glib.Values;             use Glib.Values;");
         In_At_Cursor(Buffer, buf_String(str));
         str := To_Unbounded_String("with Gtk;                     use Gtk;");
         In_At_Cursor(Buffer, buf_String(str));
         Get_End_Iter(Buffer, With_Spec_Iter);
         Spec_Mark := Init_S.Length;
      else
         Get_End_Iter(Buffer, With_Body_Iter);
      end if;
   end Insert_License;

   -----------------------------------------------------------------------------
   -- procedure Make_Program_Spec(str : unbounded_String)                     --
   -- Make_Program_Spec Creates The centerpart op the .ads file               --
   -----------------------------------------------------------------------------
   procedure Make_Program_Spec(str : unbounded_String) is
      pos1   : integer;
      pos2   : integer;
      pos3   : integer;
      last   : boolean := false;
      str1   : Unbounded_String;
      space  : Unbounded_String;
      n      : Items;
      p      : Properties;
      s      : Signals;
   begin
      if CIndex(str, "<object") > 0 then
         pos1 := CIndex(str, """") + 1;
         pos2 := CIndex(str, """", pos1) - 1;
         str1 := Slice(str, pos1, pos2);
         n := first_item;
         statusbar := false;
         while not last loop
            if str1 = Item_Names(n, 1) then
               if n <= I_AppChooserDialog then
                  first_object := true;
               end if;
               case n is
                  when I_Window | I_ApplicationWindow | I_AboutDialog |
                       I_AppChooserDialog | I_Assistant | I_RecentChooserDialog |
                       I_MessageDialog | I_FontChooserDialog | I_ColorChooserDialog |
                       I_FileChooserDialog | I_OffscreenWindow | I_Dialog =>
                     Do_Start(Spec_Buffer, n);
                  when others =>
                     if n = I_Statusbar then
                        Statusbar := true;
                     end if;
                     space := Empty_String;
                     pos1 := CIndex(str, "id=");
                     pos2 := CIndex(str, """", pos1 + 1) + 1;
                     pos3 := CIndex(str, """", pos2) - 1;
                     for m in pos3 - pos2 .. use_dist loop
                        space := space & " ";
                     end loop;
                     str1 := Slice(str, pos2, pos3);
                     str1 := str1 & space & " : " ;
                     str1 := str1 & Item_Names(n, 2) & ";";
                     if Level_S > 0 then
                        In_At_Cursor(Spec_Buffer, buf_String(str1, level_S));
                     else
                        Pipe_Spec.Push(str1);
                     end if;
                     str1 := Item_Names(n, 2);
                     With_Buffer(Spec_Buffer, n);
               end case;
               last := true;
            end if;
            if n = last_item and not last then
               last := true;
            end if;
            if not last then
               n := Items'Succ(n);
            end if;
         end loop;
      elsif CIndex(str, "<property name") > 0 then
         pos1 := CIndex(str, """") + 1;
         pos2 := CIndex(str, """", pos1) - 1;
         str1 := Slice(str, pos1, pos2);
         p := first_property;
         while not last loop
            if p = last_property and not last then
               last := true;
            end if;
            if not last then
               p := Properties'Succ(p);
            end if;
         end loop;
      elsif CIndex(str, "<signal name=") > 0 then
         pos1 := CIndex(str, """") + 1;
         pos2 := CIndex(str, """", pos1) - 1;
         str1 := Slice(str, pos1, pos2);
         s := first_signal;
         while not last loop
            if s = last_signal and not last then
               last := true;
            end if;
            if not last then
               s := Signals'Succ(s);
            end if;
         end loop;
      end if;
   end Make_Program_Spec;

   -----------------------------------------------------------------------------
   -- procedure Make_Program_Body(str : unbounded_String)                     --
   -- Make_Program_Body Creates The centerpart op the .ads file               --
   -----------------------------------------------------------------------------
   procedure Make_Program_Body(str : unbounded_String) is
      pos1      : integer;
      pos2      : integer;
      pos3      : integer;
      last      : boolean := false;
      str1      : Unbounded_String;
      str_start : Unbounded_String;
      n         : Items;
      p         : Properties;
      s         : Signals;
      Present   : Object_Depth;
   begin
      if CIndex(str, "<object") > 0 then
         Object_Level := Object_Level + 1;
         pos1 := CIndex(str, """") + 1;
         pos2 := CIndex(str, """", pos1) - 1;
         str1 := Slice(str, pos1, pos2);
         n := I_Window;
         while not last loop
            if str1 = Item_Names(n, 1) then
               if n <= I_AppChooserDialog then
                  first_object := true;
               end if;
               case n is
                  when I_Window =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Window.Initialize (MainWindow, Window_Toplevel);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_ApplicationWindow =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Application_Window.Initialize (MainWindow, Window_Toplevel);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_AboutDialog =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.About_Dialog.Initialize (MainWindow);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                      Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                 when I_AppChooserDialog =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.App_Chooser_Dialog.Initialize (MainWindow);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_Assistant =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Assistant.Initialize (MainWindow);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_RecentChooserDialog =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Recent_Chooser_Dialog.Initialize (MainWindow);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_MessageDialog =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Message_Dialog.Initialize (MainWindow);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_FontChooserDialog =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Font_Chooser_Dialog.Initialize (MainWindow);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_ColorChooserDialog =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Color_Chooser_Dialog.Initialize (MainWindow);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_FileChooserDialog =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.FileChooserDialog.Initialize (MainWindow);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_OffscreenWindow =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Offscreen_Window.Initialize (MainWindow, Window_Toplevel);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when I_Dialog =>
                     Do_Start(Body_Buffer, n);
                     str_start := To_Unbounded_String("Gtk.Dialog.Initialize (MainDialog);");
                     Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str_start, Level_B)), Start_Buffer);
                     Body_Mark := Init_B.Length;
                     Level_Body:= Level_B;
                  when others =>
                     pos1 := CIndex(str, "id=");
                     pos2 := CIndex(str, """", pos1 + 1) + 1;
                     pos3 := CIndex(str, """", pos2) - 1;
                     str1 := Slice(str, pos2, pos3);
                     str1 := str1 & Item_Names(n, 2) & ";";
                     str1 := Slice(str, pos2, pos3);
                     str_start := Empty_String & "Gtk_New(MainWindow." & str1;
                     Present.Present := n;
                     Present.Depth := Object_Level;
                     Present.Level := Level_B;
                     Present.str := str_start;
                     case n is
                        when I_Box | I_Notebook | I_Frame | I_AspectFrame |
                             I_Paned | I_Layout | I_ButtonBox | I_Expander |
                             I_Viewport | I_ScrolledWindow | I_Alignment |
                             I_Radiobutton | I_Spinbutton | I_Label |
                             I_Accellabel | I_Filechooserbutton | I_Linkbutton |
                             I_Scalebutton | I_ApplicationChooserbutton | I_Scale |
                             I_Scrollbar | I_Separator | I_Arrow | I_DrawingArea |
                             I_ApplicationChooserWidget | I_ListStore |
                             I_Treestore | I_Treemodelfilter | I_Entrybuffer |
                             I_Texttag | I_Sizegroup | I_Adjustment =>
                           Present.Line := Init_B.Length + 1;
                        when others =>
                           Present.Line := 0;
                           str_start := str_start & ");";
                     end case;
                     if Present.Line > 0 then
                        New_Item.Push(Present);
                     end if;
                     if Level_B > 0 then
                        In_At_Cursor(Body_Buffer, buf_String(str_start, level_B));
                     else
                        Pipe_Body.Push(str1);
                     end if;
                     if n = I_Statusbar then
                        Statusbar := true;
                     end if;
               end case;
               last := true;
            end if;
            if n = last_item and not last then
               last := true;
            end if;
            if not last then
               n := Items'Succ(n);
            end if;
         end loop;
      elsif CIndex(str, "<property name") > 0 then
         pos1 := CIndex(str, """") + 1;
         pos2 := CIndex(str, """", pos1) - 1;
         str1 := Slice(str, pos1, pos2);
         p := first_property;
         while not last loop
            if P = last_property and not last then
               last := true;
            end if;
            if not last then
               p := Properties'Succ(p);
            end if;
            case p is
               when P_Visilble =>
                  null;
               when others =>
                  null;
            end case;
         end loop;
      elsif CIndex(str, "<signal name=") > 0 then
         pos1 := CIndex(str, """") + 1;
         pos2 := CIndex(str, """", pos1) - 1;
         str1 := Slice(str, pos1, pos2);
         s := first_signal;
         while not last loop
            if s = last_signal and not last then
               last := true;
            end if;
            if not last then
               s := Signals'Succ(s);
            end if;
         end loop;
      elsif CIndex(str, "</object") > 0 then
         Present := New_Item.Get;
         if Present.Depth = Object_Level then
            if Present.Line > 0 then
               str_start := Present.str;
               case Present.Present is
                  when I_Adjustment =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Alignment =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_ApplicationChooserbutton =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_ApplicationChooserWidget =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_AspectFrame =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Arrow =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Box =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_ButtonBox =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_DrawingArea =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Entrybuffer =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Expander =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Filechooserbutton =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Frame =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Label =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Linkbutton =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Notebook =>
                     New_Item.Remove;
                     str_start := Present.str;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Paned =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Radiobutton =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Scalebutton =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Scrollbar =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_ScrolledWindow =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Separator =>
                     New_Item.Remove;
                     str_start := Present.str;
                     str_start := str_start & ");";
                     Print_Line(str_start);
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Sizegroup =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Print_Line(str_start);
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Spinbutton =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Texttag =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Treemodelfilter =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Treestore =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when I_Viewport =>
                     New_Item.Remove;
                     str_start := str_start & ");";
                     Init_B.Set_Buffer(To_Unbounded_String(buf_String(str_start, Present.Level)), Present.Line);
                  when others =>
                     null;
               end case;
            end if;
         end if;
         Object_Level := Object_Level - 1;
      end if;
   end Make_Program_Body;

   -----------------------------------------------------------------------------
   -- procedure Start_Spec(Name   : Unbounded_String;                         --
   --                      str    : String;                                   --
   --                      Buffer : Gtk_Text_Buffer                           --
   -- Start Spec Creates the static begin part of the body for the .ads file  --
   -- from the package call                                                   --
   -----------------------------------------------------------------------------
   procedure Start_Spec(Name   : Unbounded_String;
                        str    : String;
                        Buffer : Gtk_Text_Buffer) is
      str1   : Unbounded_String;
   begin
      insert_license(Buffer);
      --------------------------------------------------------------------------
      -- Package Header
      --------------------------------------------------------------------------
      In_At_Cursor(Buffer, buf_String(Empty_String));
      str1 := To_Unbounded_String("package ") & Name & " is";
      In_At_Cursor(Buffer, buf_String(str1));
      In_At_Cursor(Buffer, buf_String(Empty_String));
      Level_S := Level_S + 1;
      str1 := To_Unbounded_String("type Main_") & str & "_Record is new Gtk_" & str & "_Record with record";
      In_At_Cursor(Buffer, buf_String(str1, Level_S));
      Level_S := Level_S + 1;
   end Start_Spec;

   -----------------------------------------------------------------------------
   -- procedure Start_Body(Name   : Unbounded_String;                         --
   --                      str    : String;                                   --
   --                      Buffer : Gtk_Text_Buffer                           --
   -- Start Body Creates the static begin part of the body for the .adb file  --
   -- from the package call                                                   --
   -----------------------------------------------------------------------------
   procedure Start_Body(Name   : Unbounded_String;
                        str    : String;
                        Buffer : Gtk_Text_Buffer) is
      str1   : Unbounded_String;
   begin
      insert_license(Buffer);
      --------------------------------------------------------------------------
      -- Package Header
      --------------------------------------------------------------------------
      str1 := To_Unbounded_String("package body ") & Name & " is";
      In_At_Cursor(Buffer, buf_String(str1));
      In_At_Cursor(Buffer, buf_String(Empty_String));
      level_B := level_B + 1;
      str1 := To_Unbounded_String("package ") & str & "s_CBR is new Gtk.Handlers.Return_Callback (GTk_" & str & "_Record, Boolean);";
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      In_At_Cursor(Buffer, buf_String(Empty_String));
      --------------------------------------------------------------------------
      -- procedure Gtk_New
      --------------------------------------------------------------------------
      str1 := To_Unbounded_String("procedure Gtk_New(Main") & str & " : out Main_" & str & "_Access) is";
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      str1 := To_Unbounded_String("begin");
      level_B := level_B + 1;
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      str1 := To_Unbounded_String("Main") & str & " := new Main_" & str & "_Record;";
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      str1 := To_Unbounded_String("Glade2Ada_Main_Init.Init (Main") & str & ");";
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      level_B := level_B - 1;
      str1 := To_Unbounded_String("end Gtk_New;");
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      In_At_Cursor(Buffer, buf_String(Empty_String));
      --------------------------------------------------------------------------
      -- Procedure Init
      --------------------------------------------------------------------------
      str1 :=  To_Unbounded_String("procedure Init(Main") & str & "g : access Main_" & str & "_Record'Class) is";
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      Level_B := Level_B + 1;
      str1 :=  To_Unbounded_String("pragma Suppress (All_Checks);");
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      str1 :=  To_Unbounded_String("Pixmaps_Dir : constant String := ""pixmaps/"";");
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      str1 :=  To_Unbounded_String("FontDesc    : Pango_Font_Description;");
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      Level_B := Level_B - 1;
      str1 :=  To_Unbounded_String("begin");
      In_At_Cursor(Buffer, buf_String(str1, level_B));
      Level_B := Level_B + 1;
      Start_Buffer := Init_B.Length;
   end Start_Body;

   -----------------------------------------------------------------------------
   -- procedure Insert_End_Spec(Name : Unbounded_String)                      --
   -- Insert_End_Spec creates the last lines of the .ads file                 --
   -----------------------------------------------------------------------------
   procedure Insert_End_Spec(Name : Unbounded_String) is
      str    : Unbounded_String;
      Buffer : constant Gtk_Text_Buffer := Spec_Buffer;
   begin
      while Pipe_Spec.Get_Length > 0 loop
         str := Pipe_Spec.Pop;
         In_At_Cursor(Buffer, buf_String(str, Level_S));
      end loop;
      Level_S := Level_S - 1;
      str := To_Unbounded_String("end record;");
      In_At_Cursor(Buffer, buf_String(str, level_S));
      In_At_Cursor(Buffer, buf_String(Empty_String));
      str := To_Unbounded_String("type Main_Window_Access is access all Main_Window_Record'Class;");
      In_At_Cursor(Buffer, buf_String(str, level_S));
      In_At_Cursor(Buffer, buf_String(Empty_String, level_S));
      str := To_Unbounded_String("MainSize_H  : Gint :=") & integer'image(Size_H) & ";";
      In_At_Cursor(Buffer, buf_String(str, level_S));
      str := To_Unbounded_String("MainSize_V  : Gint :=") & integer'image(Size_V) & ";";
      In_At_Cursor(Buffer, buf_String(str, level_S));
      str := To_Unbounded_String("Main_Window : Main_Window_Access;");
      In_At_Cursor(Buffer, buf_String(str, level_S));
      In_At_Cursor(Buffer, buf_String(Empty_String));
      str := To_Unbounded_String("procedure Gtk_New(MainDialog : out Win_Dialog_Access);");
      In_At_Cursor(Buffer, buf_String(str, level_S));
      str := To_Unbounded_String("procedure Init(MainDialog : access Win_Dialog_Record'Class);");
      In_At_Cursor(Buffer, buf_String(str, level_S));
      if Statusbar then
         str := To_Unbounded_String("procedure Set_Stausbar_Text(Bar : Gtk_Status_Bar;");
         In_At_Cursor(Buffer, buf_String(str, level_S));
         str := To_Unbounded_String("                            Context_Des : UTF8_String;");
         In_At_Cursor(Buffer, buf_String(str, level_S));
         str := To_Unbounded_String("                            Text : Unbounded_String);");
         In_At_Cursor(Buffer, buf_String(str, level_S));
      end if;
      Level_S := level_S - 1;
      In_At_Cursor(Buffer, buf_String(Empty_String));
      str := To_Unbounded_String("end ") & Name & "_Init;";
      In_At_Cursor(Buffer, buf_String(str));
   end Insert_End_Spec;

   -----------------------------------------------------------------------------
   -- procedure Insert_End_Body(Name : Unbounded_String)                      --
   -- Insert_End_Body creates the last lines of the .adb file                 --
   -----------------------------------------------------------------------------
   procedure Insert_End_Body(Name : Unbounded_String) is
      str    : Unbounded_String;
      Buffer : constant Gtk_Text_Buffer := Body_Buffer;
   begin
      Level_B := Level_B - 1;
      str := To_Unbounded_String("end Init;");
      In_At_Cursor(Buffer, buf_String(str, level_B));
      if Statusbar then
         In_At_Cursor(Buffer, buf_String(Empty_String));
         str := To_Unbounded_String("procedure Set_Stausbar_Text(Bar : Gtk_Status_Bar;");
         In_At_Cursor(Buffer, buf_String(str, level_B));
         str := To_Unbounded_String("                            Context_Des : UTF8_String;");
         In_At_Cursor(Buffer, buf_String(str, level_B));
         str := To_Unbounded_String("                            Text : Unbounded_String) is");
         In_At_Cursor(Buffer, buf_String(str, level_B));
         Level_B := Level_B + 1;
         str := To_Unbounded_String("C_ID : Context_Id;");
         In_At_Cursor(Buffer, buf_String(str, level_B));
         str := To_Unbounded_String("M_ID : Message_Id;");
         In_At_Cursor(Buffer, buf_String(str, level_B));
         Level_B := Level_B - 1;
         str := To_Unbounded_String("begin");
         In_At_Cursor(Buffer, buf_String(str, level_B));
         Level_B := Level_B + 1;
         str := To_Unbounded_String("C_ID := Get_Context_ID(Bar, Context_Des)");
         In_At_Cursor(Buffer, buf_String(str, level_B));
         str := To_Unbounded_String("M_ID := Push(Bar, C_ID, To_String(Text));");
         In_At_Cursor(Buffer, buf_String(str, level_B));
         Level_B := Level_B - 1;
         str := To_Unbounded_String("end Set__Statusbar_Text;");
         In_At_Cursor(Buffer, buf_String(str, level_B));
      end if;
      In_At_Cursor(Buffer, buf_String(Empty_String));
      Level_B := level_B - 1;
      str := To_Unbounded_String("end ") & Name & "_Init;";
      In_At_Cursor(Buffer, buf_String(str));
       while Pipe_Body.Get_Length > 0 loop
         str := Pipe_Body.Pop;
         str := "Gtk_New(MainWindow." & str & ");";
         Init_B.Insert_Buffer(To_Unbounded_String(buf_String(str, Level_Body)), Body_Mark);
      end loop;
   end Insert_End_Body;

   -----------------------------------------------------------------------------
   -- procedure Do_Start(Buffer : Gtk_Text_Buffer; n : Items)                 --
   -- Do_Start creates in Buffer The Correct Package start and the entry in   --
   -- the with part of the program                                            --
   -----------------------------------------------------------------------------
   procedure Do_Start(Buffer : Gtk_Text_Buffer; n : Items) is
      str       : Unbounded_String;
      End_Iter  : Gtk_Text_Iter;
   begin
      str := Slice(Item_Names(n, 1), 4, Length(Item_Names(n, 1)));
      if Buffer = Spec_Buffer then
         Start_Spec(Pack_Name_Init, To_String(str), Spec_Buffer);
      else
         Start_Body(Pack_Name_Init, To_String(str), Body_Buffer);
      end if;
      if not S_Item_With(n) then
         With_Buffer(Spec_Buffer, n);
         S_Item_With(n) := true;
         Get_End_Iter(Buffer, End_Iter);
         Place_Cursor(Buffer, End_Iter);
      end if;
   end Do_Start;

   -----------------------------------------------------------------------------
   -- procedure convert                                                        --
   -- This is the main procedure that converts a .glade file to the necessary --
   -- .ads and .adb files                                                     --
   -----------------------------------------------------------------------------
   procedure convert is
      n          : integer;
      len        : integer;
      pos        : integer;
      start      : boolean := true;
      str        : Unbounded_String;
      str1       : Unbounded_String;
      start_iter : Gtk_Text_Iter;
      end_iter   : Gtk_Text_Iter;
   begin
      Level_B := 0;
      Level_S := 0;
      Level_I := 0;
      n := 1;
      while start loop
         first_object := false;
         while n <= glade_lines loop
            str := Buffer_G.Get_Buffer(n);
            pos := CIndex(str, "<");
            len := Length(str);
            if pos > 0 then
               str := Slice(str, pos, len);
            end if;
            len := Length(str);
            pos := CIndex(str, ">");
            if pos > 1 then
               str1 := Slice(str, 1, pos - 1);
               len := Length(str1);
            else
               str1 := empty_string;
               len := 0;
            end if;
            Make_Program_Spec(str1);
            Make_Program_Body(str1);
            if CIndex(str1, "<interface") > 0 or CIndex(str1, "<object") > 0 or
                CIndex(str1, "<child") > 0 or CIndex(str1, "<packing") > 0 or
                CIndex(str1, "<object") > 0 then
               Level_I := Level_I + 1;
            end if;
            if CIndex(str1, "</interface") > 0 or CIndex(str1, "</object") > 0 or
                CIndex(str1, "</child") > 0 or CIndex(str1, "</packing") > 0 or
                CIndex(str1, "</object") > 0 then
               Level_I := Level_I - 1;
            end if;
            if Level_I = 1 and CIndex(str1, "</object") > 0 and
               first_object then
               Insert_End_Spec(Pack_Name_Init);
               Insert_End_Body(Pack_Name_Init);
            end if;
            if CIndex(str1, "</interface") > 0 then
               start := false;
            end if;
            if CIndex(str1, "<?") < 1 or CIndex(str1, "<!") < 1 or
               CIndex(str1, "<requires") < 1 then
               if len > 1 then
                  str1 := Slice(str1, 2, len);
               else
                  str1 := Empty_String;
               end if;
            else
               null;
            end if;
            n := n + 1;
         end loop;
      end loop;
      for n in 1 .. With_Spec.Length loop
         With_Line(Spec_Buffer, With_Spec.Get_Buffer(n));
      end loop;
      for n in 1 .. Init_S.Length loop
         Insert_At_Cursor(Spec_Buffer, buf_String(Init_S.Get_Buffer(n), 0, true));
      end loop;
      for n in 1 .. With_Body.Length loop
         With_Line(Body_Buffer, With_Body.Get_Buffer(n));
      end loop;
      for n in 1 .. Init_B.Length loop
         Insert_At_Cursor(Body_Buffer, buf_String(Init_B.Get_Buffer(n), 0, true));
      end loop;
   end convert;

end Glade_Convert;
