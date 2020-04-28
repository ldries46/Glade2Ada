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
with Gtk.Handlers; use Gtk.Handlers;
with Glade2Ada_FC_CB; use Glade2Ada_FC_CB;
with Program_Init; use Program_Init;
with Glade_Name_Init;
with Glade2Ada_General; use Glade2Ada_General;
with Glade2Ada_Languages; use Glade2Ada_Languages;

package body Glade2Ada_FC_Init is

   package Button is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

   procedure Init_Open is
      b : boolean;
   begin
      Inp_Folder := To_Unbounded_String(Get_Value (n_ini, "Directory", "Input",
                                        To_String(Inp_folder)));
      File_Open_Dialog := Gtk_File_Chooser_Dialog_New
        (Open_Title, Glade_Name_Init.Main_Dialog, Action_Open);
      b := Set_Current_Folder(File_Open_Dialog, To_String(Inp_folder));
      Set_Current_Name(File_Open_Dialog,
                              To_String(File_Name) & "." & To_String(Extension));
      FC_Open_Cancel := Gtk_Button(Add_Button(Gtk_Dialog(File_Open_Dialog),
                                   To_String(Lan_Cancel(Lan)), Gtk_Response_Cancel));
      FC_Open_OK := Gtk_Button(Add_Button(Gtk_Dialog(File_Open_Dialog),
                               To_String(Lan_Open(Lan)), Gtk_Response_OK));
      Button.Connect(FC_Open_OK, "clicked",
                     Button.To_Marshaller(On_FC_Open_OK'Access));
      Button.Connect(FC_Open_Cancel, "clicked",
                     Button.To_Marshaller(On_FC_Cancel'Access));
      b := Set_Current_Folder(File_Open_Dialog, To_String(Inp_folder));
   end Init_Open;

   procedure Init_Save is
      b : boolean;
   begin
      Out_folder := To_Unbounded_String(Get_Value (n_ini, "Directory", "Output",
                                        To_String(Out_folder)));
      File_Save_Dialog := Gtk_File_Chooser_Dialog_New
        (Save_As_Title, Glade_Name_Init.Main_Dialog, Action_Save);
      b := Set_Current_Folder(File_Save_Dialog, To_String(Out_folder));
      Set_Current_Name(File_Save_Dialog,
                              To_String(File_Name) & "." & To_String(Extension));
      FC_Save_Cancel := Gtk_Button(Add_Button(Gtk_Dialog(File_Save_Dialog),
                                   To_String(Lan_Cancel(Lan)), Gtk_Response_Cancel));
      FC_Save_OK := Gtk_Button(Add_Button(Gtk_Dialog(File_Save_Dialog),
                               To_String(Lan_Save(Lan)), Gtk_Response_OK));
      Button.Connect(FC_Save_OK, "clicked",
                     Button.To_Marshaller(On_FC_Save_OK'Access));
      Button.Connect(FC_Save_Cancel, "clicked",
                     Button.To_Marshaller(On_FC_Cancel'Access));
      b := Set_Current_Folder(File_Save_Dialog, To_String(Out_folder));
   end Init_Save;

   procedure Init_Select_Folder is
      b : boolean;
   begin
      Cur_folder := To_Unbounded_String(Get_Value (n_ini, "Directory", "Current",
                                        To_String(Cur_folder)));
      File_Dir_Dialog := Gtk_File_Chooser_Dialog_New
        (Dir_Title, Glade_Name_Init.Main_Dialog, Action_Select_Folder);
      b := Set_Current_Folder(File_Dir_Dialog, To_String(Cur_folder));
      FC_Dir_Cancel := Gtk_Button(Add_Button(Gtk_Dialog(File_Dir_Dialog),
                                  To_String(Lan_Cancel(Lan)), Gtk_Response_Cancel));
      FC_Dir_OK := Gtk_Button(Add_Button(Gtk_Dialog(File_Dir_Dialog),
                              To_String(Lan_Set(Lan)), Gtk_Response_OK));
      Button.Connect(FC_Dir_OK, "clicked",
                     Button.To_Marshaller(On_FC_Dir_OK'Access));
      Button.Connect(FC_Dir_Cancel, "clicked",
                     Button.To_Marshaller(On_FC_Cancel'Access));
      b := Set_Current_Folder(File_Dir_Dialog, To_String(Cur_folder));
   end Init_Select_Folder;

end Glade2Ada_FC_Init;
