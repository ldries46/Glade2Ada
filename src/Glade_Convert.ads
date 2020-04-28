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

with Gtk.Text_buffer;       use Gtk.Text_Buffer;
with Gtk.Text_Mark;         use Gtk.Text_Mark;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Strings;               use Strings;
with Buffer;
with Pipe;
with Stack;

package Glade_Convert is

   type Items is (I_Window,
                  I_OffscreenWindow,
                  I_ApplicationWindow,
                  I_Dialog,
                  I_AboutDialog,
                  I_FileChooserDialog,
                  I_ColorChooserDialog,
                  I_FontChooserDialog,
                  I_MessageDialog,
                  I_RecentChooserDialog,
                  I_Assistant,
                  I_AppChooserDialog,
                  I_Accelgroup,
                  I_Accellabel,
                  I_Adjustment,
                  I_Alignment,
                  I_ApplicationChooserbutton,
                  I_ApplicationChooserWidget,
                  I_Arrow,
                  I_AspectFrame,
                  I_Box,
                  I_Button,
                  I_ButtonBox,
                  I_Calendar,
                  I_Checkbutton,
                  I_Colorbutton,
                  I_ColorchooserWidget,
                  I_Combobox,
                  I_Comboboxtext,
                  I_DrawingArea,
                  I_Entry,
                  I_Entrybuffer,
                  I_Entrycompletion,
                  I_Eventbox,
                  I_Expander,
                  I_Filechooserbutton,
                  I_FilechooserWidget,
                  I_Filefilter,
                  I_Fixed,
                  I_FontButton,
                  I_FontChooserWidget,
                  I_Frame,
                  I_Grid,
                  I_Iconfactory,
                  I_Iconview,
                  I_Image,
                  I_ImageMenuItem,
                  I_Infobar,
                  I_Label,
                  I_Layout,
                  I_LevelBar,
                  I_Linkbutton,
                  I_Listbox,
                  I_ListStore,
                  I_Menu,
                  I_Menubar,
                  I_Menubutton,
                  I_MenuItem,
                  I_Notebook,
                  I_Overlay,
                  I_Paned,
                  I_Placessidebar,
                  I_Popupmenu,
                  I_Progressbar,
                  I_Radiobutton,
                  I_RecentChooser,
                  I_Recentfilter,
                  I_Recentmanager,
                  I_Revealer,
                  I_Scale,
                  I_Scalebutton,
                  I_Scrollbar,
                  I_ScrolledWindow,
                  I_SearchEntry,
                  I_Separator,
                  I_SeparatorMenuItem,
                  I_Sizegroup,
                  I_Spinbutton,
                  I_Spinner,
                  I_Statusbar,
                  I_Statusicon,
                  I_Switch,
                  I_Textbuffer,
                  I_TextEntry,
                  I_Texttag,
                  I_Texttagtable,
                  I_TextView,
                  I_Togglebutton,
                  I_Treestore,
                  I_Treemodelfilter,
                  I_Treemodelsort,
                  I_Treeview,
                  I_Toolbar,
                  I_ToolPalette,
                  I_Viewport,
                  I_Volumebutton,
                  I_Windowgroup,
                  I_NULL);
   type Properties is (P_Can_Focus,
                       P_Expand,
                       P_Fill,
                       P_Height,
                       P_Image,
                       P_Label,
                       P_Left_Attach,
                       P_Margin_Bottom,
                       P_Margin_End,
                       P_Margin_Left,
                       P_Margin_Right,
                       P_Margin_Start,
                       P_Margin_Top,
                       P_Orientation,
                       P_Position,
                       P_Shadow_Type,
                       P_Spacing,
                       P_Stock,
                       P_Receives_Default,
                       P_Right_Attach,
                       P_Use_UnderLine,
                       P_Use_action_appearance,
                       P_Use_Stock,
                       P_Visilble,
                       P_Width);
   Type Signals is (S_Activate,
                    S_Activate_item,
                    S_Deselect,
                    S_Pressed,
                    S_Select);

   Type Object_Depth is record
      Present : Items;
      Depth   : integer;
      Line    : integer;
      Level   : integer;
      str     : Unbounded_String;
   end record;

   package With_Spec is new Buffer(Items);
   package With_Body is new Buffer(Items);
   package New_Item is new Stack(Object_Depth, (I_NULL, 0, 0, 0, Empty_String));
   package Pipe_Spec is new Pipe(Unbounded_String, Empty_String);
   package Pipe_Body is new Pipe(Unbounded_String, Empty_String);

   level_S        : integer;
   level_B        : integer;
   level_I        : integer;
   Size_H         : integer := 760;
   Size_V         : integer := 670;
   glade_lines    : integer;
   Spec_Mark      : integer;
   Body_Mark      : integer;
   Level_Body     : integer;
   Start_Buffer   : integer;
   Object_Level   : integer := 0;
   use_dist       : constant integer := 26;
   first_object   : boolean;
   statusbar      : boolean;
   Present_Object : Object_Depth;
   first_item     : constant Items := I_Window;
   last_Item      : constant Items := I_Windowgroup;
   first_property : constant Properties := P_Can_Focus;
   last_property  : constant Properties := P_Width;
   first_signal   : constant Signals := S_Activate;
   last_signal    : constant Signals := S_Select;
   Iter_End       : Gtk_Text_Iter;
   With_Spec_Iter : Gtk_Text_Iter;
   With_Body_Iter : Gtk_Text_Iter;

   S_Item_With    : array(first_item .. last_Item) of boolean :=
     (false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false);
   B_Item_With    : array(first_item .. last_Item) of boolean :=
     (false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false);

   Item_Names     : constant array(first_item .. last_Item, 1 .. 2 ) of Unbounded_String :=
     ((To_Unbounded_String("GtkWindow"),To_Unbounded_String("Gtk_Window")),
      (To_Unbounded_String("GtkOffscreenWindow"),To_Unbounded_String("Gtk_Offscreen_Window")),
      (To_Unbounded_String("GtkApplicationWindow"),To_Unbounded_String("Gtk_Application_Window")),
      (To_Unbounded_String("GtkDialog"),To_Unbounded_String("Gtk_Dialog")),
      (To_Unbounded_String("GtkAboutDialog"),To_Unbounded_String("Gtk_About_Dialog")),
      (To_Unbounded_String("GtkFileChooserDialog"),To_Unbounded_String("Gtk_FileChooserDialog")),
      (To_Unbounded_String("GtkColorChooserDialog"),To_Unbounded_String("Gtk_Color_Chooser_Dialog")),
      (To_Unbounded_String("GtkFontChooserDialog"),To_Unbounded_String("Gtk_Font_Chooser_Dialog")),
      (To_Unbounded_String("GtkMessageDialog"),To_Unbounded_String("Gtk_Message_Dialog")),
      (To_Unbounded_String("GtkRecentChooserDialog"),To_Unbounded_String("Gtk_Recent_Chooser_Dialog")),
      (To_Unbounded_String("GtkAssistant"),To_Unbounded_String("Gtk_Assistant")),
      (To_Unbounded_String("GtkAppChooserDialog"),To_Unbounded_String("Gtk_App_Chooser_Dialog")),
      -------------
      (To_Unbounded_String("GtkAccelGroup"),To_Unbounded_String("Gtk_Accel_Group")),
      (To_Unbounded_String("GtkAccelLabel"),To_Unbounded_String("Gtk_Accel_Label")),
      (To_Unbounded_String("GtkAdjustment"),To_Unbounded_String("Gtk_Adjustment")),
      (To_Unbounded_String("GtkAlignment"),To_Unbounded_String("Gtk_Alignment")),
      (To_Unbounded_String("GtkApplicationChooserButton"),To_Unbounded_String("Gtk_Application_Chooser_Button")),
      (To_Unbounded_String("GtkApplicationChooserWidget"),To_Unbounded_String("Gtk_Application_Chooser_Widget")),
      (To_Unbounded_String("GtkArrow"),To_Unbounded_String("Gtk_Arrow")),
      (To_Unbounded_String("GtkAspectFrame"),To_Unbounded_String("Gtk_Aspect_Frame")),
      (To_Unbounded_String("GtkBox"),To_Unbounded_String("Gtk_Box")),
      (To_Unbounded_String("GtkButton"),To_Unbounded_String("Gtk_Button")),
      (To_Unbounded_String("GtkButtonBox"),To_Unbounded_String("Gtk_Button_Box")),
      (To_Unbounded_String("GtkCalendar"),To_Unbounded_String("Gtk_Calendar")),
      (To_Unbounded_String("GtkCheckButton"),To_Unbounded_String("Gtk_Check_Button")),
      (To_Unbounded_String("GtkColorButton"),To_Unbounded_String("Gtk_Color_Button")),
      (To_Unbounded_String("GtkColorChooserWidget"),To_Unbounded_String("Gtk_Color_Chooser_Widget")),
      (To_Unbounded_String("GtkComBox"),To_Unbounded_String("Gtk_Combo_Box")),
      (To_Unbounded_String("GtkComboBoxText"),To_Unbounded_String("Gtk_Combo_Box_Text")),
      (To_Unbounded_String("GtkDrawing_Area"),To_Unbounded_String("Gtk_Drawing_Area")),
      (To_Unbounded_String("GtkEntry"),To_Unbounded_String("Gtk_Entry")),
      (To_Unbounded_String("GtkEntryBuffer"),To_Unbounded_String("Gtk_Entry_Buffer")),
      (To_Unbounded_String("GtkEntryCompletion"),To_Unbounded_String("Gtk_Entry_Completion")),
      (To_Unbounded_String("GtkEventBox"),To_Unbounded_String("Gtk_Event_Box")),
      (To_Unbounded_String("GtkExpander"),To_Unbounded_String("Gtk_Expander")),
      (To_Unbounded_String("GtkFileChooserButton"),To_Unbounded_String("Gtk_File_Chooser_Button")),
      (To_Unbounded_String("GtkFileChooserWidget"),To_Unbounded_String("Gtk_File_Chooser_Widget")),
      (To_Unbounded_String("GtkFileFilter"),To_Unbounded_String("Gtk_File_Filter")),
      (To_Unbounded_String("GtkFixed"),To_Unbounded_String("Gtk_Fixed")),
      (To_Unbounded_String("GtkFontButton"),To_Unbounded_String("Gtk_Font_Button")),
      (To_Unbounded_String("GtkFontChooserWidget"),To_Unbounded_String("Gtk_GtkFontChooserWidget")),
      (To_Unbounded_String("GtkFrame"),To_Unbounded_String("Gtk_Frame")),
      (To_Unbounded_String("GtkGrid"),To_Unbounded_String("Gtk_Grid")),
      (To_Unbounded_String("GtkIconFactory"),To_Unbounded_String("Gtk_Icon_Factory")),
      (To_Unbounded_String("GtkIconView"),To_Unbounded_String("Gtk_Icon_View")),
      (To_Unbounded_String("GtkImage"),To_Unbounded_String("Gtk_Image")),
      (To_Unbounded_String("GtkImageMenuItem"),To_Unbounded_String("Gtk_Image_Menu_Item")),
      (To_Unbounded_String("GtkInfoBar"),To_Unbounded_String("Gtk_Info_Bar")),
      (To_Unbounded_String("GtkLabel"),To_Unbounded_String("Gtk_Label")),
      (To_Unbounded_String("GtkLayout"),To_Unbounded_String("Gtk_Layout")),
      (To_Unbounded_String("GtkLevelBar"),To_Unbounded_String("Gtk_Level_Bar")),
      (To_Unbounded_String("GtkLinkButton"),To_Unbounded_String("Gtk_Link_Button")),
      (To_Unbounded_String("GtkListbox"),To_Unbounded_String("Gtk_List_Box")),
      (To_Unbounded_String("GtkListStore"),To_Unbounded_String("Gtk_List_Store")),
      (To_Unbounded_String("GtkMenu"),To_Unbounded_String("Gtk_Menu")),
      (To_Unbounded_String("GtkMenuBar"),To_Unbounded_String("Gtk_Menu_bar")),
      (To_Unbounded_String("GtkMenuButton"),To_Unbounded_String("Gtk_Menu_Button")),
      (To_Unbounded_String("GtkMenuItem"),To_Unbounded_String("Gtk_Menu_Item")),
      (To_Unbounded_String("GtkNotebook"),To_Unbounded_String("Gtk_Notebook")),
      (To_Unbounded_String("GtkOverlay"),To_Unbounded_String("Gtk_Overlay")),
      (To_Unbounded_String("GtkPaned"),To_Unbounded_String("Gtk_Paned")),
      (To_Unbounded_String("GtkPlacesSidebar"),To_Unbounded_String("Gtk_Places_Sidebar")),
      (To_Unbounded_String("GtkPopupMenu"),To_Unbounded_String("Gtk_Popup_Menu")),
      (To_Unbounded_String("GtkProgressBar"),To_Unbounded_String("Gtk_Progress_Bar")),
      (To_Unbounded_String("GtkRadioButton"),To_Unbounded_String("Gtk_Radio_Button")),
      (To_Unbounded_String("GtkRecentChooser"),To_Unbounded_String("Gtk_Recent_Chooser")),
      (To_Unbounded_String("GtkRecentFilter"),To_Unbounded_String("Gtk_Recent_Filter")),
      (To_Unbounded_String("GtkRecentManager"),To_Unbounded_String("Gtk_Recent_Manager")),
      (To_Unbounded_String("GtkRevealer"),To_Unbounded_String("Gtk_Revealer")),
      (To_Unbounded_String("GtkScale"),To_Unbounded_String("Gtk_Scale")),
      (To_Unbounded_String("GtkScaleButton"),To_Unbounded_String("Gtk_Scale_Button")),
      (To_Unbounded_String("GtkScrollbar"),To_Unbounded_String("Gtk_Scrollbar")),
      (To_Unbounded_String("GtkScrolledWindow"),To_Unbounded_String("Gtk_Scrolled_Window")),
      (To_Unbounded_String("GtkSearchEntry"),To_Unbounded_String("Gtk_Search_Entry")),
      (To_Unbounded_String("GtkSeparator"),To_Unbounded_String("Gtk_Separator")),
      (To_Unbounded_String("GtkSeparatorMenuItem"),To_Unbounded_String("Gtk_Separator_Menu_Item")),
      (To_Unbounded_String("GtkSizeGroup"),To_Unbounded_String("Gtk_Size_Group")),
      (To_Unbounded_String("GtkSpinButton"),To_Unbounded_String("Gtk_Spin_Button")),
      (To_Unbounded_String("GtkSpinner"),To_Unbounded_String("Gtk_Spinner")),
      (To_Unbounded_String("GtkStatusbar"),To_Unbounded_String("Gtk_Statusbar")),
      (To_Unbounded_String("GtkStatusIcon"),To_Unbounded_String("Gtk_Status_Icon")),
      (To_Unbounded_String("GtkSwitch"),To_Unbounded_String("Gtk_Switch")),
      (To_Unbounded_String("GtkTextBuffer"),To_Unbounded_String("Gtk_Text_Buffer")),
      (To_Unbounded_String("GtkTextEntry"),To_Unbounded_String("Gtk_Text_Entry")),
      (To_Unbounded_String("GtkTextTag"),To_Unbounded_String("Gtk_Text_Tag")),
      (To_Unbounded_String("GtkTextTagTable"),To_Unbounded_String("Gtk_Text_Table")),
      (To_Unbounded_String("GtkTextView"),To_Unbounded_String("Gtk_Text_View")),
      (To_Unbounded_String("GtkToggleButton"),To_Unbounded_String("Gtk_Toggle_Button")),
      (To_Unbounded_String("GtkTreeModelFilter"),To_Unbounded_String("Gtk_Tree_Model_Filter")),
      (To_Unbounded_String("GtkTreeModelSort"),To_Unbounded_String("Gtk_Tree_Model_Sort")),
      (To_Unbounded_String("GtkTreeStore"),To_Unbounded_String("Gtk_Tree_Store")),
      (To_Unbounded_String("GtkTreeView"),To_Unbounded_String("Gtk_Tree_View")),
      (To_Unbounded_String("GtkToolbar"),To_Unbounded_String("Gtk_Toolbar")),
      (To_Unbounded_String("GtkToolPalette"),To_Unbounded_String("Gtk_Tool_Palette")),
      (To_Unbounded_String("GtkViewport"),To_Unbounded_String("Gtk_Viewport")),
      (To_Unbounded_String("GtkVolmeButton"),To_Unbounded_String("Gtk_Volume_Button")),
      (To_Unbounded_String("GtkWindowGroup"),To_Unbounded_String("Gtk_Window_Group")));

   procedure In_At_Cursor(Buffer : Gtk_Text_Buffer; str : String);
   function buf_String(str : Unbounded_String; level : integer := 0; CR : boolean := false) return string;
   procedure Read_File;
   procedure With_Line(Buffer : Gtk_Text_Buffer; Win : Items);
   procedure With_Buffer(Buffer : Gtk_Text_Buffer; Win : Items);
   procedure insert_license(Buffer : Gtk_Text_Buffer);
   procedure Make_Program_Spec(str : unbounded_String);
   procedure Make_Program_Body(str : unbounded_String);
   procedure Start_Spec(Name   : Unbounded_String;
                        str    : String;
                        Buffer : Gtk_Text_Buffer);
   procedure Start_Body(Name   : Unbounded_String;
                        str    : String;
                        Buffer : Gtk_Text_Buffer);
   procedure Insert_End_Spec(Name : Unbounded_String);
   procedure Insert_End_Body(Name : Unbounded_String);
   procedure Do_Start(Buffer : Gtk_Text_Buffer; n : Items);
   procedure convert;

end Glade_Convert;
