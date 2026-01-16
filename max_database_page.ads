--                                                                    --
--  package MAX_Database_Page       Copyright (c)  Dmitry A. Kazakov  --
--     Database page                               Luebeck            --
--  Interface                                      Winter, 2017       --
--                                                                    --
--                                Last revision :  09:24 30 Nov 2019  --
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

with Gtk.Box;                use Gtk.Box;
with Gtk.Check_Button;       use Gtk.Check_Button;
with Gtk.Combo_Box_Text;     use Gtk.Combo_Box_Text;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Grid;               use Gtk.Grid;
with Gtk.Widget;             use Gtk.Widget;
with Interfaces;             use Interfaces;
with MAX_Icon_Factory;       use MAX_Icon_Factory;
with ODBC.API;               use ODBC.API;
with ODBC.Bound_Parameters;  use ODBC.Bound_Parameters;

with Generic_Map;
with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
with Gtk.Handlers;
with Object.Handle;
with SQLite;

package MAX_Database_Page is
   use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

   Table_Name   : constant String   := "datalog";
   Max_Attempts : constant Positive := 3;
--
-- Database columns
--
--  1. Time_stamp
--  2. RF_address
--  3. Device_type
--  4. Set_temperature
--  5. New_temperature
--  6. Is_temperature
--  7. Offset
--  8. Valve
--  9. Open
-- 10. Error
-- 11. Duty
-- 12. Slots
-- 13. Battery low
-- 14. Panel locked
-- 15. Mode
--
   Time_Column            : constant String := "time_stamp";
   Address_Column         : constant String := "address";
   Device_Type_Column     : constant String := "device_type";
   Set_Temperature_Column : constant String := "set_temperature";
   New_Temperature_Column : constant String := "new_temperature";
   Is_Temperature_Column  : constant String := "is_temperature";
   Offset_Column          : constant String := "offset";
   Valve_Column           : constant String := "valve_position";
   Open_Column            : constant String := "contact_open";
   Error_Column           : constant String := "error";
   Duty_Column            : constant String := "duty";
   Slots_Column           : constant String := "slots";
   Battery_Low_Column     : constant String := "battery_low";
   Panel_Locked_Column    : constant String := "panel_locked";
   Mode_Column            : constant String := "current_mode";

   type MAX_Database_Record is new Gtk_Widget_Record with private;
   procedure Update_Data
             (  Logger : not null access MAX_Database_Record;
                Data   : Device_Data;
                Offset : Centigrade
             );
   procedure Update_Status
             (  Logger  : not null access MAX_Database_Record;
                Address : RF_Address;
                Error   : Boolean;
                Duty    : Ratio;
                Slots   : Natural
             );
   type MAX_Database is access all MAX_Database_Record'Class;
   function Is_Enabled (Widget : not null access MAX_Database_Record)
      return Boolean;
   function Gtk_Database_New return MAX_Database;
private
   type DB_Record is record
      Set_temperature : Centigrade := Centigrade'First;
      New_temperature : Centigrade := Centigrade'First;
      Is_temperature  : Centigrade := Centigrade'First;
      Offset          : Centigrade := Centigrade'First;
      Slots           : Integer    := -1;
      Device_Type     : Unsigned_8 := 255;
      Valve           : Unsigned_8 := 255;
      Open            : Unsigned_8 := 255;
      Error           : Unsigned_8 := 255;
      Duty            : Unsigned_8 := 255;
      Battery_Low     : Unsigned_8 := 255;
      Panel_Locked    : Unsigned_8 := 255;
      Mode            : Unsigned_8 := 255;
   end record;
   type DB_Record_Ptr is access DB_Record;
   package Address_To_Record_Maps is
      new Generic_Map (RF_Address, DB_Record_Ptr);

   type Abstract_Data_Base is abstract
      new Object.Entity with null record;
   type Abstract_Data_Base_Ptr is access Abstract_Data_Base'Class;

   procedure Create
             (  Base   : in out Abstract_Data_Base;
                Widget : not null access MAX_Database_Record'Class;
                Silent : Boolean;
                Failed : out Boolean
             )  is abstract;
   procedure Insert_Minimal_Device
             (  Base    : in out Abstract_Data_Base;
                Address : RF_Address;
                Data    : DB_Record
             )  is abstract;
   procedure Insert_Radiator_Thermostat_Full
             (  Base    : in out Abstract_Data_Base;
                Address : RF_Address;
                Data    : DB_Record
             )  is abstract;
   procedure Insert_Radiator_Thermostat_Short
             (  Base    : in out Abstract_Data_Base;
                Address : RF_Address;
                Data    : DB_Record
             )  is abstract;
   procedure Insert_Shutter_Contact
             (  Base    : in out Abstract_Data_Base;
                Address : RF_Address;
                Data    : DB_Record
             )  is abstract;
   procedure Insert_Status
             (  Base    : in out Abstract_Data_Base;
                Address : RF_Address;
                Data    : DB_Record
             )  is abstract;
   procedure Insert_Wall_Thermostat_Full
             (  Base    : in out Abstract_Data_Base;
                Address : RF_Address;
                Data    : DB_Record
             )  is abstract;
   procedure Insert_Wall_Thermostat_Short
             (  Base    : in out Abstract_Data_Base;
                Address : RF_Address;
                Data    : DB_Record
             )  is abstract;

   package Data_Base_Handles is
      new Object.Handle (Abstract_Data_Base, Abstract_Data_Base_Ptr);

   type SQLite_Data is new Abstract_Data_Base with record
      DB                               : SQLite.Data_Base;
      Insert_Status                    : SQLite.Statement;
      Insert_Minimal_Device            : SQLite.Statement;
      Insert_Shutter_Contact           : SQLite.Statement;
      Insert_Wall_Thermostat_Short     : SQLite.Statement;
      Insert_Wall_Thermostat_Full      : SQLite.Statement;
      Insert_Radiator_Thermostat_Short : SQLite.Statement;
      Insert_Radiator_Thermostat_Full  : SQLite.Statement;
   end record;
   overriding
      procedure Create
                (  Base   : in out SQLite_Data;
                   Widget : not null access MAX_Database_Record'Class;
                   Silent : Boolean;
                   Failed : out Boolean
                );
   overriding
      procedure Finalize (Base : in out SQLite_Data);
   overriding
      procedure Insert_Minimal_Device
                (  Base    : in out SQLite_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Shutter_Contact
                (  Base    : in out SQLite_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Status
                (  Base    : in out SQLite_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Radiator_Thermostat_Full
                (  Base    : in out SQLite_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Radiator_Thermostat_Short
                (  Base    : in out SQLite_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Wall_Thermostat_Full
                (  Base    : in out SQLite_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Wall_Thermostat_Short
                (  Base    : in out SQLite_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );

   type ODBC_Connection_Ptr is access ODBC_Connection'Class;
   type ODBC_Command_Ptr    is access ODBC_Command'Class;

   type ODBC_Data is new Abstract_Data_Base with record
      Environment                      : aliased ODBC_Environment;
      DB                               : ODBC_Connection_Ptr;
      Insert_Status                    : ODBC_Command_Ptr;
      Insert_Minimal_Device            : ODBC_Command_Ptr;
      Insert_Shutter_Contact           : ODBC_Command_Ptr;
      Insert_Wall_Thermostat_Short     : ODBC_Command_Ptr;
      Insert_Wall_Thermostat_Full      : ODBC_Command_Ptr;
      Insert_Radiator_Thermostat_Short : ODBC_Command_Ptr;
      Insert_Radiator_Thermostat_Full  : ODBC_Command_Ptr;

      Address         : aliased SQLUINTEGER_Parameter;
      Device_Type     : aliased SQLUTINYINT_Parameter;
      Set_Temperature : aliased SQLDOUBLE_Parameter;
      New_Temperature : aliased SQLDOUBLE_Parameter;
      Is_Temperature  : aliased SQLDOUBLE_Parameter;
      Offset          : aliased SQLDOUBLE_Parameter;
      Valve           : aliased SQLUTINYINT_Parameter;
      Open            : aliased SQLUTINYINT_Parameter;
      Error           : aliased SQLUTINYINT_Parameter;
      Duty            : aliased SQLUTINYINT_Parameter;
      Slots           : aliased SQLUINTEGER_Parameter;
      Battery_Low     : aliased SQLUTINYINT_Parameter;
      Panel_Locked    : aliased SQLUTINYINT_Parameter;
      Mode            : aliased SQLUTINYINT_Parameter;
   end record;

   overriding
      procedure Create
                (  Base   : in out ODBC_Data;
                   Widget : not null access MAX_Database_Record'Class;
                   Silent : Boolean;
                   Failed : out Boolean
                );
   overriding
      procedure Finalize (Base : in out ODBC_Data);
   overriding
      procedure Insert_Minimal_Device
                (  Base    : in out ODBC_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Shutter_Contact
                (  Base    : in out ODBC_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Status
                (  Base    : in out ODBC_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Radiator_Thermostat_Full
                (  Base    : in out ODBC_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Radiator_Thermostat_Short
                (  Base    : in out ODBC_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Wall_Thermostat_Full
                (  Base    : in out ODBC_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );
   overriding
      procedure Insert_Wall_Thermostat_Short
                (  Base    : in out ODBC_Data;
                   Address : RF_Address;
                   Data    : DB_Record
                );

   type MAX_Database_Record is new Gtk_VBox_Record with record
      Grid      : Gtk_Grid;
      Enable    : Gtk_Check_Button;
      Method    : Gtk_Combo_Box_Text;
      DSN       : Gtk_GEntry;
      File_Name : Gtk_GEntry;
      User      : Gtk_GEntry;
      Password  : Gtk_GEntry;
      Visible   : Gtk_Check_Button;
      Browse    : SQLite_File_Buttons.Gtk_Style_Button;
         -- Last written data
      Last      : Address_To_Record_Maps.Map;
      Base      : Data_Base_Handles.Handle;
   end record;
   function Get_Data
            (  Widget  : not null access MAX_Database_Record;
               Address : RF_Address
            )  return not null DB_Record_Ptr;

   procedure On_Browse
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             );
   procedure On_Destroy
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             );
   procedure Set_Enabled (Widget : not null access MAX_Database_Record);
   procedure On_Method_Changed
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             );
   procedure On_Revert
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             );
   procedure On_Toggled
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             );
   procedure On_Visible
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             );
   procedure Store (Widget : not null access MAX_Database_Record);

   package Database_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             MAX_Database
          );
   use Database_Handlers;

end MAX_Database_Page;
