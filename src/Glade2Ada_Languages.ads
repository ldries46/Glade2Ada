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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Glade2Ada_Languages is

   type Language is (GB_USA, NL_B, F_B, D, ES);

      Lan_ID         : Gint := 0;

   -----------------------------------------------------------------------------
   --                                                                         --
   -- If not all languages are wanted nr_languages must be altered and in     --
   -- CH_Lanuages CH_Languages the not wanted languages must be deleted       --
   --                                                                         --
   -----------------------------------------------------------------------------
   nr_languages      : constant integer := 5;

   type ar_lan is array (0 .. nr_languages - 1) of Language;

   Lan               : Language := GB_USA;
   CH_Languages      : constant ar_lan := (GB_USA, NL_B, F_B, D, ES);

   -- Languages Names
   Language_Names    : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("English"), To_Unbounded_String("Nederlands"),
      To_Unbounded_String("Francais"), To_Unbounded_String("Deutsch"),
      To_Unbounded_String("Espagnol"));
   -- Titles
   Lan_Window_Title  : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Glade2Ada Program"),
      To_Unbounded_String("Glade2Ada Programma"),
      To_Unbounded_String(""), To_Unbounded_String(""),
      To_Unbounded_String(""));
   Lan_Dialog_Title  : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Glade2Ada Dialog"),
      To_Unbounded_String("Glade2Ada Dialoog"),
      To_Unbounded_String(""), To_Unbounded_String(""),
      To_Unbounded_String(""));

   -- Menu
   Lan_File           : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("_File"), To_Unbounded_String("_Bestanden"),
      To_Unbounded_String("Fichier"), To_Unbounded_String("Datei"),
      To_Unbounded_String("Archivo"));
   Lan_Edit           : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("_Edit"), To_Unbounded_String("Be_werken"),
      To_Unbounded_String("Modifier"), To_Unbounded_String("Bearbeiten"),
      To_Unbounded_String("Editar"));
   Lan_View           : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("_View"), To_Unbounded_String("Beeld"),
      To_Unbounded_String("Vue"), To_Unbounded_String("Bild"),
      To_Unbounded_String("Ver"));
   Lan_Help           : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("_Help"), To_Unbounded_String("Hulp"),
      To_Unbounded_String("Aide"), To_Unbounded_String("Hilfe"),
      To_Unbounded_String("Ayuda"));
   Lan_New            : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("New"), To_Unbounded_String("Nieuw"),
      To_Unbounded_String(""), To_Unbounded_String("Neu"),
      To_Unbounded_String(""));
   Lan_Open           : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Open"), To_Unbounded_String("Open"),
      To_Unbounded_String("Ouvrir"), To_Unbounded_String("Ofnen"),
      To_Unbounded_String("Abierto"));
   Lan_Save           : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Save"), To_Unbounded_String("Opslaan"),
      To_Unbounded_String("Sauvegarder"), To_Unbounded_String("Speichern"),
      To_Unbounded_String("Guardar"));
   Lan_Save_As        : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Save As"), To_Unbounded_String("Opslaan Als"),
      To_Unbounded_String("Enregistrer sous"), To_Unbounded_String("Speichern unter"),
      To_Unbounded_String("Guardar como"));
   Lan_Print          : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Print"), To_Unbounded_String("Print"),
      To_Unbounded_String("Imprimer"), To_Unbounded_String("Drucken"),
      To_Unbounded_String("Imprimir"));
   Lan_Print_Option   : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Print Options"), To_Unbounded_String("Print Opties"),
      To_Unbounded_String("Options d'impression"), To_Unbounded_String("Druckoptionen"),
      To_Unbounded_String("Opciones de impresión"));
   Lan_Properties     : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Properties"), To_Unbounded_String("Eigenschappen"),
      To_Unbounded_String("Propriétés"), To_Unbounded_String("Eigenschaften"),
      To_Unbounded_String("Propiedades"));
   Lan_Language       : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Languages"), To_Unbounded_String("Talen (Languages)"),
      To_Unbounded_String("Langues (Languages)"), To_Unbounded_String("Sprachen (Languages)"),
      To_Unbounded_String("Idiomas (Languages)"));
   Lan_Quit           : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Quit"), To_Unbounded_String("Afsluiten"),
      To_Unbounded_String("Quitter"), To_Unbounded_String("Verlassen"),
      To_Unbounded_String("Dejar"));
   Lan_Cut            : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Cut"), To_Unbounded_String("Knippen"),
      To_Unbounded_String("Couper"), To_Unbounded_String("Kopieren"),
      To_Unbounded_String("Cortar"));
   Lan_Copy           : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Copy"), To_Unbounded_String("Copieren"),
      To_Unbounded_String("Copie"), To_Unbounded_String("Schnitt"),
      To_Unbounded_String("Dupdo"));
   Lan_Paste          : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Paste"), To_Unbounded_String("Plakken"),
      To_Unbounded_String("Coller"), To_Unbounded_String("Einfügen"),
      To_Unbounded_String("Pegar"));
   Lan_Delete         : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Delete"), To_Unbounded_String("Verwijderen"),
      To_Unbounded_String("Effacer"), To_Unbounded_String("Löschen"),
      To_Unbounded_String("Effacer"));
   Lan_About          : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("About"), To_Unbounded_String("Over"),
      To_Unbounded_String("Sur"), To_Unbounded_String("Über"),
      To_Unbounded_String("Acerca de"));

   --Buttons
   Lan_OK             : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("OK"), To_Unbounded_String("OK"),
      To_Unbounded_String("OK"), To_Unbounded_String("OK"),
      To_Unbounded_String("OK"));
   Lan_Cancel         : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Cancel"), To_Unbounded_String("Annuleren"),
      To_Unbounded_String("Annuler"), To_Unbounded_String("Abbrechen"),
      To_Unbounded_String("Cancelar"));
   Lan_Open_But       : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Open"), To_Unbounded_String("Open"),
      To_Unbounded_String("Ouvrir"), To_Unbounded_String("Öffnen"),
      To_Unbounded_String("Abierto"));
   Lan_Save_but       : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Save"), To_Unbounded_String("Opslaan"),
      To_Unbounded_String("Sauvegarder"), To_Unbounded_String("Speichern"),
      To_Unbounded_String("Guardar"));
   Lan_Set            : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String("Set"), To_Unbounded_String("Zet"),
      To_Unbounded_String("Tourner"), To_Unbounded_String("Wende an"),
      To_Unbounded_String("Vuelta"));

   -- Various Labels

   Glade_File            : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String(".glade File"), To_Unbounded_String(".glade Bestand"),
      To_Unbounded_String(".glade Fichier"), To_Unbounded_String(".glade Datei"),
      To_Unbounded_String(".glade Archivo"));
   Specification_File            : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String(".ads File"), To_Unbounded_String(".ads Bestand"),
      To_Unbounded_String(".ads Fichier"), To_Unbounded_String(".ads Datei"),
      To_Unbounded_String(".ads Archivo"));
   Body_File            : constant array(GB_USA .. ES) of Unbounded_String :=
     (To_Unbounded_String(".adb File"), To_Unbounded_String(".adb Bestand"),
      To_Unbounded_String(".adb Fichier"), To_Unbounded_String(".adb Datei"),
      To_Unbounded_String(".adb Archivo"));

   procedure Reload_Language(l : Language);

end Glade2Ada_Languages;
