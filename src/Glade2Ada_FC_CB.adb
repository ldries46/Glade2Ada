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
with Gtk.File_Chooser;        use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Program_Init;            use Program_Init;
with Strings;                 use Strings;
with Glade2Ada_Main_CB;       use Glade2Ada_Main_CB;
with Glade2Ada_FC_Init;       use Glade2Ada_FC_Init;
with Glade2Ada_General;       use Glade2Ada_General;
with Glade_Convert;           use Glade_Convert;

Package body Glade2Ada_FC_CB is

   procedure On_FC_Open_OK (Object : access Gtk_Button_Record'Class) is
      pos1 : integer;
   begin
      Inp_Folder := To_Unbounded_String(Get_Current_Folder(File_Open_Dialog));
      Inp_File := To_Unbounded_String(Get_Filename(+File_Open_Dialog));
      pos1 := CIndex(Inp_File, ".");
      if pos1 = 0 then
         pos1 := Length(Inp_File);
      else
         pos1 := pos1 - 1;
      end if;
      Pack_Name := To_Unbounded_String(Slice(Inp_File, 1, pos1));
      pos1 := CIndex(Pack_Name, Dir_Sep, 1);
      while pos1 > 0 loop
         Pack_Name := To_Unbounded_String(Slice(Pack_Name, pos1 + 1, Length(Pack_Name)));
         pos1 := CIndex(Pack_Name, Dir_Sep, 1);
      end loop;
      Pack_Name_Init := Pack_Name & "_Main_Init";
      Pack_Name_CB := Pack_Name & "_Main_CB";
      Set_Value (n_ini, "Directory", "Input", To_String(Inp_Folder));
      File_Open_Dialog.Hide;
      Read_File;
   end On_FC_Open_OK;

   procedure On_FC_Save_OK (Object : access Gtk_Button_Record'Class) is
   begin
      Out_Folder := To_Unbounded_String(Get_Current_Folder(File_Save_Dialog));
      Set_Value (n_ini, "Directory", "Output", To_String(Out_Folder));
      File_Save_Dialog.Hide;
   end On_FC_Save_OK;

   procedure On_FC_Dir_OK (Object : access Gtk_Button_Record'Class) is
   begin
      Cur_Folder := To_Unbounded_String(Get_Current_Folder(File_Dir_Dialog));
      Set_Value (n_ini, "Directory", "Current", To_String(Cur_Folder));
      File_Dir_Dialog.Hide;
   end On_FC_Dir_OK;

   procedure On_FC_Cancel (Object : access Gtk_Button_Record'Class) is
   begin
      case File is
         when Open => File_Open_Dialog.Hide;
         when Save => File_Save_Dialog.Hide;
         when Dir => File_Dir_Dialog.Hide;
      end case;
   end On_FC_Cancel;

end Glade2Ada_FC_CB;
