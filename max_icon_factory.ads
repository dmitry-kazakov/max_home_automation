--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Max_Icon_Factory                            Luebeck            --
--  Interface                                      Summer, 2015       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with GLib;                 use GLib;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Widget;           use Gtk.Widget;
with MAX_User;             use MAX_User;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;

with Gtk.Generic_Style_Button;
with Strings_Edit.UTF8;
with Strings_Edit.Integers.Superscript;

package Max_Icon_Factory is
--
-- The icons provided by the factory
--
   Add_Device_Icon          : constant String := "add-device-id";
   Automatic_Icon           : constant String := "automatic-id";
   Battery_High_Icon        : constant String := "battery-high-id";
   Battery_Low_Icon         : constant String := "battery-low-id";
   Boost_Icon               : constant String := "boost-id";
   Calendar_Icon            : constant String := "calendar-id";
   Cancel_Icon              : constant String := "cancel-id";
   Copy_From_Icon           : constant String := "copy-from-id";
   Cube_Icon                : constant String := "cube-id";
   Cube_Crossed_Icon        : constant String := "cube-crossed-id";
   Disconnect_Icon          : constant String := "disconnect-id";
   Eco_Button_Icon          : constant String := "eco-button-id";
   EMail_Icon               : constant String := "email-id";
   Error_Link_Icon          : constant String := "error-link-id";
   Error_No_Link_Icon       : constant String := "error-no-link-id";
   Link_Icon                : constant String := "link-id";
   Manual_Icon              : constant String := "manual-id";
   MAX_Icon                 : constant String := "max-id";
   Move_Icon                : constant String := "move-id";
   Network_Scan_Icon        : constant String := "network-scan-id";
   No_Link_Icon             : constant String := "no_link-id";
   NTP_Icon                 : constant String := "ntp-id";
   Operating_Mode_Icon      : constant String := "operating-mode-id";
   Radiator_Thermostat_Icon : constant String := "radiator-id";
   Reconnect_Icon           : constant String := "reconnect-id";
   Rename_Icon              : constant String := "rename-id";
   Room_Icon                : constant String := "room-id";
   Set_Thermostat_Icon      : constant String := "set-thremostat-id";
   Vacation_Icon            : constant String := "vacation-id";
   Wake_Up_Icon             : constant String := "wake-up-id";
   Wall_Thermostat_Icon     : constant String := "wall-thremostat-id";
   Window_Open_Icon         : constant String := "window_open-id";
   Window_Closed_Icon       : constant String := "window-closed-id";
--
-- Icon_Size_Panel -- Size of huge 224x224 icons
--
   function Icon_Size_Panel return Gtk_Icon_Size;
--
-- Escape_Name -- Double each underscore in the string
--
--    Name - To add escapes
--
-- Returns :
--
--    Name that can be used as a label
--
   function Escape_Name (Name : UTF8_String) return UTF8_String;
--
-- Init -- Factory initialization
--
-- The initialization has to be called before the first use of any  icon
-- from the factory. It can be called several times. Note that it can be
-- done  during  package elaboration, because GTK+ might be not ready at
-- that point.
--
   procedure Init;

   MAX_Domain          : constant String := "EHA";
   Number_Of_Shortcuts : constant := 10;

   package Add_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Add_Buttons",
             Label      => "",
             Icon       => "gtk-add",
             Relief     => Relief_None,
             Tip        => "Manually specify the address of a MAX! cube"
          );
   package Add_Device_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Add_Device_Buttons",
             Label      => "",
             Icon       => Add_Device_Icon,
             Relief     => Relief_None,
             Tip        => "Add a new device"
          );
   package About_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "About_Buttons",
             Label      => "",
             Icon       => "gtk-about",
             Relief     => Relief_None,
             Tip        => "About MAX! Home Automation"
          );
   package Calendar_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Calendar_Buttons",
             Label      => "",
             Icon       => Calendar_Icon,
             Relief     => Relief_None,
             Tip        => "Select vacation end date"
          );
   package Cancel_Restore_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Cancel_Restore_Button",
             Label      => "",
             Icon       => "gtk-cancel",
             Relief     => Relief_None,
             Tip        => "Cancel restoring configuration"
          );
   package Cancel_Topology_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Cancel_Topology_Buttons",
             Label      => "",
             Icon       => "gtk-cancel",
             Relief     => Relief_None,
             Tip        => "Cancel restoring topology"
          );
   package Clear_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Clear_Trace_Button",
             Label      => "",
             Icon       => "gtk-clear",
             Relief     => Relief_None,
             Tip        => "Erase all trace box contents"
          );
   package Configure_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Configure_Button",
             Label      => "",
             Icon       => "gtk-execute",
             Relief     => Relief_None,
             Tip        => "Configure device"
          );
   package Continue_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Continue_Trace_Button",
             Label      => "",
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Continue tracing"
          );
   package Copy_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Copy_Button",
             Label      => "",
             Icon       => "gtk-copy",
             Relief     => Relief_None,
             Tip        => "Copy"
          );
   package Copy_From_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Copy_From_Button",
             Label      => "",
             Icon       => Copy_From_Icon,
             Relief     => Relief_None,
             Tip        => "Copy temperature schedule from another " &
                           "thermostat"
          );
   package Cut_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Cut_Button",
             Label      => "",
             Icon       => "gtk-cut",
             Relief     => Relief_None,
             Tip        => "Cut"
          );
   package Delete_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Delete_Button",
             Label      => "",
             Icon       => "gtk-delete",
             Relief     => Relief_None,
             Tip        => "Delete selected"
          );
   package Disconnect_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Disconnect_Button",
             Label      => "",
             Icon       => Reconnect_Icon,
             Relief     => Relief_None,
             Tip        => "Connected to the cube, click to disconnect"
          );
   package Down_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Down_Button",
             Label      => "",
             Icon       => "gtk-go-down",
             Relief     => Relief_None,
             Tip        => "Move down"
          );
   package Email_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "EMail_Button",
             Label      => "",
             Icon       => EMail_Icon,
             Relief     => Relief_None,
             Tip        => "Send a test E-Mail"
          );
   package Erase_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Erase_Button",
             Label      => "",
             Icon       => "gtk-clear",
             Relief     => Relief_None,
             Tip        => "Delete all"
          );
   package Export_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Export_Button",
             Label      => "",
             Icon       => "gtk-save",
             Relief     => Relief_None,
             Tip        => "Export settings and visual appearance "   &
                           "into a file. The settings are kept by "   &
                           "the GTK recent manager. They can be "     &
                           "restored from the file when lost or "     &
                           "corrupted. In order to that the file "    &
                           "must be passed to the MAX! home "         &
                           "automation using the command line "       &
                           "argument --restore=<file>"
          );
   package Faulty_Devices_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Faulty_Devices_Button",
             Label      => "",
             Icon       => "gtk-dialog-warning",
             Relief     => Relief_None,
             Tip        => "Delete faulty devices. There are some "   &
                           "malfunctioning devices detected. "        &
                           "These do not appear in the list of "      &
                           "devices and rooms below. "                &
                           "Press this button to begin deleting "     &
                           "these devices. "                          &
                           "After deletion you will be able to "      &
                           "pair the deleted devices again. "         &
                           "Optionally, prior to pairing, a device "  &
                           "can be reset to its factory settings. "   &
                           "Usually it is done by removing the "      &
                           "batteries and reinserting back them "     &
                           "after a one-minute wait, while pressing " &
                           "a certain button on the device pannel"
          );
   package Hold_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Hold_Trace_Button",
             Label      => "",
             Icon       => "gtk-media-pause",
             Relief     => Relief_None,
             Tip        => "Hold tracing"
          );
   package Move_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Move_Button",
             Label      => "",
             Icon       => Move_Icon,
             Relief     => Relief_None,
             Tip        => "Move selected to another room"
          );
   package MAX_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "MAX_Button",
             Label      => "",
             Icon       => MAX_Icon,
             Relief     => Relief_None,
             Tip        => "Export the configuration of devices to "  &
                           "ELV Elektronik AG MAX! software. "        &
                           "If the configuration is changed outside " &
                           "the MAX! software it will stop "          &
                           "working. If you plan to use it, press "   &
                           "to update MAX! files located in "         &
                           Quote (User_Path)                          &
                           ". The original files will be backed up."
          );
   package Network_Scan_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Network_Scan_Button",
             Label      => "",
             Icon       => Network_Scan_Icon,
             Relief     => Relief_None,
             Tip        => "Rescan the LAN"
          );
   package NTP_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "NTP_Button",
             Label      => "",
             Icon       => NTP_Icon,
             Relief     => Relief_None,
             Tip        => "Change MAX! cube's NTP " &
                           "(Network Time Protocol) settings"
          );
   package Page_File_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Page_File_Button",
             Label      => "",
             Icon       => "gtk-open",
             Relief     => Relief_None,
             Tip        => "Browse for a custom HTTP page"
          );
   package Reboot_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Reboot_Buttons",
             Label      => "",
             Icon       => "gtk-refresh",
             Relief     => Relief_None,
             Tip        => "Reboot the cube"
          );
   package Reconnect_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Reconnect_Button",
             Label      => "",
             Icon       => Disconnect_Icon,
             Relief     => Relief_None,
             Tip        => "Not connected to the cube, " &
                           "click to reconnect"
          );
   package Rename_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Rename_Button",
             Label      => "",
             Icon       => Rename_Icon,
             Relief     => Relief_None,
             Tip        => "Rename selected"
          );
   package Reset_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Reset_Trace_Button",
             Label      => "",
             Icon       => "gtk-delete",
             Relief     => Relief_None,
             Tip        => "Reset the cube "                    &
                           "deleting all devices and rooms. "   &
                           "Note that removed devices must be " &
                           "paired again"
          );
   package Restore_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Restore_Button",
             Label      => "",
             Icon       => "gtk-open",
             Relief     => Relief_None,
             Tip        => "Load configuration from a file"
          );
   package Retry_Restore_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Retry_Restore_Button",
             Label      => "",
             Icon       => "gtk-refresh",
             Relief     => Relief_None,
             Tip        => "Retry to restore failed configurations"
          );
   package Revert_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Revert_Button",
             Label      => "",
             Icon       => "gtk-undo",
             Relief     => Relief_None,
             Tip        => "Restore previously saved E-Mail settings"
          );
   package Open_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Open_Button",
             Label      => "",
             Icon       => "gtk-open",
             Relief     => Relief_None,
             Tip        => "Load from a file"
          );
   package Paste_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Paste_Button",
             Label      => "",
             Icon       => "gtk-paste",
             Relief     => Relief_None,
             Tip        => "Paste"
          );
   package Play_Buttons_1 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_1",
             Label      => Strings_Edit.Integers.Superscript.Image (1),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #1"
          );
   package Play_Buttons_2 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_2",
             Label      => Strings_Edit.Integers.Superscript.Image (2),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #2"
          );
   package Play_Buttons_3 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_3",
             Label      => Strings_Edit.Integers.Superscript.Image (3),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #3"
          );
   package Play_Buttons_4 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_4",
             Label      => Strings_Edit.Integers.Superscript.Image (4),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #4"
          );
   package Play_Buttons_5 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_5",
             Label      => Strings_Edit.Integers.Superscript.Image (5),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #5"
          );
   package Play_Buttons_6 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_6",
             Label      => Strings_Edit.Integers.Superscript.Image (6),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #6"
          );
   package Play_Buttons_7 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_7",
             Label      => Strings_Edit.Integers.Superscript.Image (7),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #7"
          );
   package Play_Buttons_8 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_8",
             Label      => Strings_Edit.Integers.Superscript.Image (8),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #8"
          );
   package Play_Buttons_9 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_9",
             Label      => Strings_Edit.Integers.Superscript.Image (9),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #9"
          );
   package Play_Buttons_10 is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Play_Button_10",
             Label      => Strings_Edit.Integers.Superscript.Image (10),
             Icon       => "gtk-media-play",
             Relief     => Relief_None,
             Tip        => "Set thermostats into the preconfigured " &
                           "modes #10"
          );
   pragma Assert (Number_Of_Shortcuts = 10);

   package Properties_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Properties_Buttons",
             Label      => "",
             Icon       => "gtk-properties",
             Relief     => Relief_None,
             Tip        => "Save style properties as a CSS file"
          );
   package Redo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Redo_Button",
             Label      => "",
             Icon       => "gtk-redo",
             Relief     => Relief_None,
             Tip        => "Redo"
          );
   package Save_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Save_Button",
             Label      => "",
             Icon       => "gtk-save",
             Relief     => Relief_None,
             Tip        => "Save configuration (into the current file)"
          );
   package Save_As_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Save_As_Button",
             Label      => "",
             Icon       => "gtk-save-as",
             Relief     => Relief_None,
             Tip        => "Save configuration into a file"
          );
   package Select_All_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Select_All_Button",
             Label      => "",
             Icon       => "gtk-select-all",
             Relief     => Relief_None,
             Tip        => "Select all"
          );
   package Select_Thermostats_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Select_Termostats_Button",
             Label      => "",
             Icon       => Radiator_Thermostat_Icon,
             Relief     => Relief_None,
             Tip        => "Select all thermostats"
          );
   package Set_Thermostat_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Set_Thermostat_Button",
             Label      => "",
             Icon       => Set_Thermostat_Icon,
             Relief     => Relief_None,
             Tip        => "Send settings to the selected thermostat(s)"
          );
   package Script_File_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Script_File_Button",
             Label      => "",
             Icon       => "gtk-open",
             Relief     => Relief_None,
             Tip        => "Browse for a script file"
          );
   package Script_Library_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Script_Library_Button",
             Label      => "",
             Icon       => "gtk-open",
             Relief     => Relief_None,
             Tip        => "Browse for a script library file"
          );
   package SQLite_File_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "SQLite_Button",
             Label      => "",
             Icon       => "gtk-open",
             Relief     => Relief_None,
             Tip        => "Browse for an SQLite database file"
          );
   package Start_Restore_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Start_Restore_Button",
             Label      => "",
             Icon       => "gtk-execute",
             Relief     => Relief_None,
             Tip        => "Start or continue restoring thermostat " &
                           "configurations"
          );
   package Start_Topology_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Start_Topology_Buttons",
             Label      => "",
             Icon       => "gtk-execute",
             Relief     => Relief_None,
             Tip        => "Start or continue restoring topology "  &
                           "of the rooms and devices. "             &
                           "You can select the row to indicated "   &
                           "where to start restoring"
          );
   package Store_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Store_Button",
             Label      => "",
             Icon       => "gtk-save",
             Relief     => Relief_None,
             Tip        => "Save E-Mail settings"
          );
   package Trace_File_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Trace_File_Button",
             Label      => "",
             Icon       => "gtk-save",
             Relief     => Relief_None,
             Tip        => "Browse for a trace file to write to"
          );
   package Undo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Undo_Button",
             Label      => "",
             Icon       => "gtk-undo",
             Relief     => Relief_None,
             Tip        => "Undo"
          );
   package Up_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Up_Button",
             Label      => "",
             Icon       => "gtk-go-up",
             Relief     => Relief_None,
             Tip        => "Move up"
          );
   package Wake_Up_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "Wake_Up_Buttons",
             Label      => "",
             Icon       => Wake_Up_Icon,
             Relief     => Relief_None,
             Tip        => "Wake up device(s)"
          );

end Max_Icon_Factory;
