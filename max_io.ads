--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO                                      Luebeck            --
--  Interface                                      Summer, 2015       --
--                                                                    --
--                                Last revision :  13:51 05 Jul 2023  --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Streams;               use Ada.Streams;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with MAX_Trace;                 use MAX_Trace;
with GLib;                      use GLib;
with GLib.Main;                 use GLib.Main;
with GLib.Messages;             use GLib.Messages;
with GLib.Object;               use GLib.Object;
with GNAT.Sockets.MQTT.Server;  use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;       use GNAT.Sockets.Server;
with Gtk.Widget;                use Gtk.Widget;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Gdk.Event;
with Generic_Indefinite_Map;
with Generic_Doubly_Linked;
with Generic_Map;
with Generic_Set;
with GNAT.Sockets.Server.Secure.X509;
with Gtk.Main.Router.GNAT_Stack;
with Interfaces;
with Object.Handle.Generic_Set;
with Strings_Edit.UTF8;
with System;

package MAX_IO is

   type Server_Ptr is
      access GNAT.Sockets.Server.Connections_Server'Class;

   Degree    : constant UTF8_String :=
                  Character'Val (16#C2#) & Character'Val (16#B0#);
   Set_T     : constant UTF8_String :=
                  Strings_Edit.UTF8.Image (16#2197#);
   Unchanged : constant UTF8_String :=
                  Strings_Edit.UTF8.Image (16#2013#);

   type Connections_Factory_Ptr is access Connections_Factory'Class;
   type Server_Data
        (  Security : GLib.GInt;
           Method   : GLib.GInt
        )  is new Object.Entity with
   record
      Factory : Connections_Factory_Ptr;
      Server  : Connections_Server_Ptr;
   end record;
   type Server_Data_Ptr is access Server_Data'Class;
   overriding procedure Finalize (Data : in out Server_Data);

   package Server_Data_Handles is
      new Object.Handle (Server_Data, Server_Data_Ptr);

   type Topology_Object (Size : Natural) is
      new Object.Entity with
   record
      Length   : Natural := 0;
      Metadata : String (1..Size);
   end record;
   type Topology_Ptr is access Topology_Object'Class;

   package Topology_Handles is
      new Object.Handle (Topology_Object, Topology_Ptr);

   type MQTT_Server_Ptr is access all MQTT_Server'Class;
   type Poll_Count is new Interfaces.Unsigned_32;

   Store_ID               : G_Source_ID := No_Source_ID;
   HTTP_Server            : Server_Ptr;
   MQTT_Listener          : Server_Ptr;
   MQTT_State             : MQTT_Server_Ptr;
   MQTT_Connection_Policy : Message_Type := Updated;
   MQTT_Cube_Policy       : Message_Type := Updated;
   MQTT_Device_Policy     : Message_Type := Updated;
   SMTP_Server            : Server_Data_Handles.Handle;
   Hosting_Server         : Server_Data_Handles.Handle;
   Graph_Width            : Duration   := 60.0;
   Temperature_Timeout    : Duration   := 60.0 * 5.0;
   Scan_Timeout           : Duration   := 60.0 * 2.0;
   Scan_Period            : Duration   := 60.0 * 5.0;
   Manual_Pairing_Time    : Duration   := 60.0 * 1.0;
   Restore_Pairing_Time   : Duration   := 4.0;
   Faulty_Timeout         : Duration   := 60.0 * 1.0;
   Graph_Add_Offset       : Boolean    := True;
   Graph_Fixed_Scale      : Boolean    := False;
   DB_Trace               : Boolean    := False;
   ELV_Trace              : Boolean    := True;
   Decoded_Trace          : Boolean    := False;
   Scan_On                : Boolean    := False;
   Discovery_On           : Boolean    := True;
   Temperature_Low        : Centigrade := 0.0;
   Temperature_High       : Centigrade := 0.0;
   Poll_No                : Poll_Count := 0;
   Next_Poll              : Time       := Clock;
   View                   : Trace_Box;
   Host                   : Unbounded_String;
   Cube_Address           : Unbounded_String;

   pragma Atomic (Graph_Add_Offset);
   pragma Atomic (DB_Trace);
   pragma Atomic (Decoded_Trace);
   pragma Atomic (ELV_Trace);
   pragma Atomic (MQTT_Connection_Policy);
   pragma Atomic (MQTT_Cube_Policy);
   pragma Atomic (MQTT_Device_Policy);
   pragma Atomic (Poll_No);
   pragma Atomic (Scan_On);

   function Restore (Key : String; Default : Integer) return GInt;
   function Restore (Key : String; Default : Integer) return Integer;

   function Image (Schedule : Week_Schedule) return String;
   function Image (Schedule : Day_Schedule ) return String;
   function Image (Value : RF_Address_Array) return String;

   function Note (Duty : Ratio) return String;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Week_Schedule
             );
   function To_String (Data : Stream_Element_Array) return String;

   package Rooms_Maps is
      new Generic_Indefinite_Map (Room_ID, String);
   package Devices_Maps is
      new Generic_Indefinite_Map (RF_Address, Device_Parameters);
--
-- Device_Parameters_Data -- Device parameters and room name
--
   type Device_Parameters_Data
        (  Kind_Of     : Device_Type;
           Name_Length : Natural;
           Room_Length : Natural
        )  is new Object.Entity with
   record
      Parameters : Device_Parameters (Kind_Of, Name_Length);
      Room       : String (1..Room_Length);
   end record;
   type Device_Parameters_Data_Ptr is
      access Device_Parameters_Data'Class;
   package Device_Parameters_Data_Handles is
      new Object.Handle
          (  Device_Parameters_Data,
             Device_Parameters_Data_Ptr
          );
   package Device_Parameters_Data_Sets is
      new Device_Parameters_Data_Handles.Generic_Set;
--
-- Add -- Device to the list
--
--    List       - The devices list
--    Parameters - The device parameters
--    Room       - The room name
--
   procedure Add
             (  List       : in out Device_Parameters_Data_Sets.Set;
                Parameters : Device_Parameters;
                Room       : String
             );
   type Room_Devices_List (Room_Length : Natural) is
      new Object.Entity with
   record
      List : Devices_Maps.Map;
      Room : String (1..Room_Length);
   end record;
   type Room_Devices_List_Ptr is access Room_Devices_List'Class;
   package Room_Devices_List_Handles is
      new Object.Handle (Room_Devices_List, Room_Devices_List_Ptr);
   package Room_To_Device_List is
      new Generic_Map (Room_ID, Room_Devices_List_Handles.Handle);
--
-- ELV factory
--
   type ELV_Factory is new Connections_Factory with private;
   overriding
      procedure Trace
                (  Factory : in out ELV_Factory;
                   Message : String
                );
   overriding
      procedure Trace_Error
                (  Factory    : in out ELV_Factory;
                   Context    : String;
                   Occurrence : Exception_Occurrence
                );
   overriding
      procedure Trace_Received
                (  Factory : in out ELV_Factory;
                   Client  : Connection'Class;
                   Data    : Stream_Element_Array;
                   From    : Stream_Element_Offset;
                   To      : Stream_Element_Offset;
                   Encoded : Boolean := False
                );
   overriding
      procedure Trace_Sending
                (  Factory : in out ELV_Factory;
                   Client  : Connection'Class;
                   Enabled : Boolean;
                   Reason  : String
                );
   overriding
      procedure Trace_Sent
                (  Factory : in out ELV_Factory;
                   Client  : Connection'Class;
                   Data    : Stream_Element_Array;
                   From    : Stream_Element_Offset;
                   To      : Stream_Element_Offset;
                   Encoded : Boolean := False
                );
   overriding
      procedure Trace_Service_Loop
                (  Factory : in out ELV_Factory;
                   Stage   : Service_Loop_Stage;
                   Server  : in out Connections_Server'Class
                );

   type ELV_Secure_Factory is
      new GNAT.Sockets.Server.Secure.X509.
          X509_Authentication_Factory with private;
   overriding
      procedure Trace
                (  Factory : in out ELV_Secure_Factory;
                   Message : String
                );
   overriding
      procedure Trace_Error
                (  Factory    : in out ELV_Secure_Factory;
                   Context    : String;
                   Occurrence : Exception_Occurrence
                );
   overriding
      procedure Trace_Received
                (  Factory : in out ELV_Secure_Factory;
                   Client  : Connection'Class;
                   Data    : Stream_Element_Array;
                   From    : Stream_Element_Offset;
                   To      : Stream_Element_Offset;
                   Encoded : Boolean := False
                );
   overriding
      procedure Trace_Sending
                (  Factory : in out ELV_Secure_Factory;
                   Client  : Connection'Class;
                   Enabled : Boolean;
                   Reason  : String
                );
   overriding
      procedure Trace_Sent
                (  Factory : in out ELV_Secure_Factory;
                   Client  : Connection'Class;
                   Data    : Stream_Element_Array;
                   From    : Stream_Element_Offset;
                   To      : Stream_Element_Offset;
                   Encoded : Boolean := False
                );

   type Cube_Client
        (  Server : not null access Connections_Server'Class
        )  is new ELV_MAX_Cube_Client with private;
   type Cube_Client_Ptr is access all Cube_Client'Class;
   overriding
      procedure Configuration_Updated
                (  Client : in out Cube_Client;
                   Update : GNAT.Sockets.Connection_State_Machine.
                            ELV_MAX_Cube_Client.Update_Data
                );
   overriding
      procedure Disconnected (Client : in out Cube_Client);
   overriding
      procedure Handshake_Received (Client : in out Cube_Client);
   overriding
      procedure Received
                (  Client  : in out Cube_Client;
                   Data    : Stream_Element_Array;
                   Pointer : in out Stream_Element_Offset
                );

   function Has_Wall_Thermostat
            (  Client : Cube_Client;
               Room   : Room_ID
            )  return Boolean;
   function Get_ID (Client : Cube_Client) return Cube_Descriptor;
   function Get_ID (Client : Cube_Client) return String;

   package RF_Address_Sets is new Generic_Set (RF_Address, 0);
   function To_Array
            (  Set : RF_Address_Sets.Set
            )  return RF_Address_Array;
   package RF_Address_Maps is new Generic_Map (RF_Address, Device_Type);
   function Image (Map : RF_Address_Maps.Map) return String;
   function To_Array
            (  Map : RF_Address_Maps.Map
            )  return RF_Address_Array;

   function Delete_Event_Handler
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event
            )  return Boolean;
   procedure Destroy_Handler (Widget : access Gtk_Widget_Record'Class);
   procedure Exit_Handler;

------------------------------------------------------------------------
   type MAX_MQTT_Client is
      new GNAT.Sockets.MQTT.Server.MQTT_Connection with null record;
   overriding
      procedure Trace
                (  Client  : in out MAX_MQTT_Client;
                   Session : String;
                   Message : String;
                   Kind_Of : GNAT.Sockets.MQTT.Trace_Message_Type
                );
------------------------------------------------------------------------
-- Device_List_Monior_Interface -- Monitoring devices added
--
   type Device_List_Monior_Interface is interface;
   type Device_List_Monior_Ptr is
      access all Device_List_Monior_Interface'Class;
   procedure Device_Added
             (  Handler : in out Device_List_Monior_Interface;
                Cube    : RF_Address;
                Device  : Device_Parameters
             )  is abstract;
------------------------------------------------------------------------
-- Settings_Handler_Interface -- Handler settings
--
   type Settings_Sequence_No is new Natural;
   type Settings_Handler_Interface is interface;
   type Settings_Handler_Ptr is
      access all Settings_Handler_Interface'Class;
   type Settings_Handler_Ref is
      access constant Settings_Handler_Interface'Class;
   procedure Canceled
             (  Handler : in out Settings_Handler_Interface
             )  is abstract;
   procedure Finished
             (  Handler : in out Settings_Handler_Interface
             )  is abstract;
   function Get_Object
            (  Handler : not null access Settings_Handler_Interface
            )  return GObject is abstract;
   function Image
            (  Handler : not null access
                         Settings_Handler_Interface'Class
            )  return String;
   procedure On_A_Response
             (  Handler  : in out Settings_Handler_Interface;
                Address  : RF_Address;
                Devices  : Devices_Maps.Map;
                List     : Rooms_Maps.Map;
                Expected : in out Natural
             )  is abstract;
   procedure On_S_Response
             (  Handler  : in out Settings_Handler_Interface;
                Cube     : access Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is abstract;
   procedure Registered
             (  Handler : in out Settings_Handler_Interface;
                No      : Settings_Sequence_No
             )  is null;
   function Replace_Handler
            (  No      : Settings_Sequence_No;
               Handler : Settings_Handler_Ptr
            )  return Boolean;
------------------------------------------------------------------------
-- Pairing_Interface -- Pairing interface
--
   type Pairing_Interface is limited interface;
   procedure Pairing_Ended
             (  Pairing : in out Pairing_Interface
             )  is abstract;
   procedure Paired_Device
             (  Pairing   : in out Pairing_Interface;
                Cube      : RF_Address;
                Device    : Device_Type;
                Address   : RF_Address;
                Serial_No : String
             )  is abstract;
   procedure Ping_Progress (Pairing : in out Pairing_Interface) is null;
   type Pairing_Interface_Ptr is access all Pairing_Interface'Class;
------------------------------------------------------------------------
   type Pairing_Issuer is interface and Settings_Handler_Interface;
   procedure Completed
             (  Issuer     : in out Pairing_Issuer;
                Successful : Boolean
             )  is abstract;
   function Get_Name
            (  Issuer : Pairing_Issuer;
               Device : RF_Address
            )  return String is abstract;
   function Get_Room
            (  Issuer : Pairing_Issuer;
               Device : RF_Address
            )  return String is abstract;
   type Pairing_Issuer_Ptr is access all Pairing_Issuer'Class;
------------------------------------------------------------------------
   type Detached_Interface is limited interface;
   procedure Detached_Device
             (  Handler   : in out Detached_Interface;
                Cube      : RF_Address;
                Kind_Of   : Device_Type;
                Device    : RF_Address;
                Serial_No : String
             )  is abstract;
   type Detached_Interface_Ptr is access all Detached_Interface'Class;
------------------------------------------------------------------------
-- GUI_Interface -- GUI interface
--
   type Version_Status is (Outdated, Uptodate, Unknown);
   type GUI_Interface is limited interface and Pairing_Interface
                                           and Detached_Interface;
   procedure Faulty_Device
             (  Rooms       : in out GUI_Interface;
                Cube        : RF_Address;
                Device      : RF_Address;
                Length      : Natural;
                Error       : Boolean;
                Initialized : Boolean;
                Orphaned    : Boolean
             )  is abstract;
   procedure Set_Auto_Scale (Rooms : in out GUI_Interface) is abstract;
   procedure Set_Fixed_Scale
             (  Rooms : in out GUI_Interface;
                Low   : Centigrade;
                High  : Centigrade
             )  is abstract;
   procedure Set_NTP_List
             (  Rooms : in out GUI_Interface;
                Cube  : RF_Address;
                List  : Servers_List.Set
             )  is abstract;
   procedure Update_Connected
             (  Rooms : in out GUI_Interface;
                ID    : Cube_Descriptor
             )  is abstract;
   procedure Update_Cube
             (  Rooms     : in out GUI_Interface;
                Address   : RF_Address;
                Serial_No : String;
                Source    : GNAT.Sockets.Sock_Addr_Type
             )  is abstract;
   procedure Update_Cube_Statistics
             (  Rooms   : in out GUI_Interface;
                Address : RF_Address;
                Average : Natural
             )  is abstract;
   procedure Update_Devices
             (  Rooms     : in out GUI_Interface;
                Handler   : Settings_Handler_Ptr;
                Address   : RF_Address;
                Serial_No : String;
                Source    : GNAT.Sockets.Sock_Addr_Type;
                Devices   : Devices_Maps.Map;
                List      : Rooms_Maps.Map;
                Expected  : in out Natural
             )  is abstract;
   procedure Update_Data
             (  Rooms   : in out GUI_Interface;
                Cube    : RF_Address;
                Data    : Device_Data;
                Offset  : Centigrade
             )  is abstract;
   procedure Update_Disconnected
             (  Rooms : in out GUI_Interface;
                ID    : Cube_Descriptor
             )  is abstract;
   procedure Update_Parameters
             (  Rooms      : in out GUI_Interface;
                Cube       : RF_Address;
                Parameters : Device_Parameters
             )  is abstract;
   procedure Update_Status
             (  Rooms    : in out GUI_Interface;
                Handler  : Settings_Handler_Ptr;
                Cube     : in out Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is abstract;
   procedure Update_Version
             (  Rooms   : in out GUI_Interface;
                Major   : Natural;
                Minor   : Natural;
                Version : Version_Status
             )  is abstract;
------------------------------------------------------------------------
   type Cube_Client_Handle is
      new Ada.Finalization.Controlled with
   record
      Ptr : Cube_Client_Ptr;
   end record;
   overriding procedure Adjust   (Handle : in out Cube_Client_Handle);
   overriding procedure Finalize (Handle : in out Cube_Client_Handle);
   procedure Invalidate (Handle : in out Cube_Client_Handle);
   function Is_Valid (Handle : Cube_Client_Handle) return Boolean;
   procedure Set
             (  Handle : in out Cube_Client_Handle;
                Cube   : Cube_Client_Ptr
             );
------------------------------------------------------------------------
   type Shortcut_Target is record
      Cube       : RF_Address;
      Thermostat : RF_Address;
   end record;
   function "<" (Left, Right : Shortcut_Target) return Boolean;

   type Shortcut_Mode is
        (  Unchanged_Shortcut,
           Automatic_Shortcut,
           Automatic_With_Temperature_Shortcut,
           Manual_Shortcut,
           Vacation_Shortcut,
           Boost_Shortcut,
           Eco_Shortcut,
           Comfort_Shortcut,
           Airing_Shortcut
        );
   function From_String (Text : String) return Shortcut_Mode;
   function To_String (Mode : Shortcut_Mode) return String;

   type Shortcut_Action
        (  Mode : Shortcut_Mode := Unchanged_Shortcut
        )  is
   record
      case Mode is
         when Unchanged_Shortcut | Automatic_Shortcut |
              Boost_Shortcut     | Eco_Shortcut       |
              Comfort_Shortcut   | Airing_Shortcut    =>
            null;
         when Automatic_With_Temperature_Shortcut =>
            Automatic_Temperature : Centigrade;
         when Manual_Shortcut =>
            Manual_Temperature    : Centigrade;
         when Vacation_Shortcut =>
            Vacation_Temperature  : Centigrade;
            Vacation_Time         : Duration;
      end case;
   end record;

   package Shortcut_Maps is
      new Generic_Map (Shortcut_Target, Shortcut_Action);
   subtype Shortcut is Shortcut_Maps.Map;
------------------------------------------------------------------------
--
-- Add_Manually -- Add a cube
--
--    Address - The cube address
--
   procedure Add_Manually (Address : UTF8_String);
--
-- Get_Cube -- Get cube by adddress
--
--    Box / Address - To search for
--
-- Returns :
--
--    A handle to the cube client
--
   function Get_Cube
            (  Box : RF_Address
            )  return Cube_Client_Handle;
   function Get_Cube
            (  Address : GNAT.Sockets.Inet_Addr_Type
            )  return Cube_Client_Handle;
--
-- Get_Cubes_List -- Get cubes
--
-- Returns :
--
--    A set of cubes
--
   function Get_Cubes_List return RF_Address_Sets.Set;
--
-- Get_Device_Data -- Get device status by its address
--
--    Box            - The cube address
--    Device / Index - The address or index
--    No_Device      - Exception/error.  When device  is found it is set
--                     to False.  When  no device  found,  and True,  no
--       exception is propagated.  When no device found,  and False then
--       End_Error is propagated.
--
-- Returns :
--
--    The device data (unknown of no device)
--
-- Exceptions :
--
--    End_Error - No such device
--
   function Get_Device_Data
            (  Box, Device : RF_Address;
               No_Device   : not null access Boolean
            )  return Device_Data;
   function Get_Device_Data
            (  Box       : RF_Address;
               Index     : not null access Positive;
               No_Device : not null access Boolean
            )  return Device_Data;
--
-- Get_Device_Name -- Get device name by its address
--
--    Box    - The cube address
--    Device - The address
--
-- Returns :
--
--    The device name or empty string
--
   function Get_Device_Name (Box, Device : RF_Address) return String;
--
-- Get_Devices_List -- Get list of devices
--
--    Box    - The cube address, 0 if any cube
--    Filter - Types of the devices to return
--
-- Returns :
--
--    The devices list
--
   type Device_Type_Set is array (Device_Type) of Boolean;
   function Get_Devices_List
            (  Box     : RF_Address      := 0;
               Filter  : Device_Type_Set := (others => True)
            )  return RF_Address_Sets.Set;
--
-- Get_Parameters -- Get device parameters by cube and device address
--
--    Box            - The cube address
--    Device / Index - The address or index
--
-- Returns :
--
--    A handle to the device parameters
--
   function Get_Parameters (Box, Device : RF_Address)
      return Device_Parameters_Data_Handles.Handle;
--
-- Get_Radiator_Thermostats -- Get the list of radiator thermostats
--
--    Box  - The cube address
--    Room - The room ID
--
-- Returns :
--
--    The set of thermostat addresses
--
   function Get_Radiator_Thermostats
            (  Box  : RF_Address;
               Room : Room_ID
            )  return RF_Address_Sets.Set;
--
-- Get_Rooms_List -- Get list of rooms
--
--    Box    - The cube address, 0 if any cube
--    Filter - Types of the devices to return
--
-- Returns :
--
--    The rooms list
--
   function Get_Rooms_List
            (  Box     : RF_Address      := 0;
               Filter  : Device_Type_Set := (others => True)
            )  return Room_To_Device_List.Map;
--
-- Get_Thermostats -- Get the list of all thermostats
--
--    Box    - The cube address
--    Room   - The room ID or No_Room
--    Filter - Types of the devices to return
--
-- Returns :
--
--    The list of devices in the room
--
   function Get_Thermostats
            (  Box    : RF_Address;
               Room   : Room_ID;
               Filter : Device_Type_Set :=
                        (  Radiator_Thermostat..Wall_Thermostat => True,
                           others => False
            )           )  return RF_Address_Maps.Map;
--
-- Get_Thermostats -- Get the list of all thermostats
--
--    Box - The cube address
--
-- Returns :
--
--    The set of thermostat addresses
--
   function Get_Thermostats return RF_Address_Sets.Set;
--
-- Get_Topology -- Get the topology
--
--    Box - The cube address
--
-- Returns :
--
--    The topology
--
   function Get_Topology (Box : RF_Address)
      return Topology_Handles.Handle;
--
-- Get_Valves -- Get the valves statistics
--
--    Box     - The cube address
--    Average - Average valve position
--    Min     - Minimum valve position
--    Max     - Maximum valve position
--
   procedure Get_Valves
             (  Box     : RF_Address;
                Average : out Natural;
                Min     : out Natural;
                Max     : out Natural
             );
--
-- Reconnect -- The cube by address
--
--    Cube    - The cube
--    Connect - True if connect, false if disconnect
--    Silent  - Silent mode
--
   procedure Reconnect
             (  Cube    : Cube_Client;
                Connect : Boolean;
                Silent  : Boolean
             );
--
-- Trace -- Do trace
--
--    Message - Trace message
--    Mode    - Message mode
--
   procedure Trace (Message : String; Mode : Trace_Type);
--
-- Scan -- The LAN for cubes
--
--    Address - The host address
--
   procedure Scan (Address : UTF8_String);
--
-- Set_Poll_Time -- Change the poll time
--
--    Poll - The period
--
   procedure Set_Poll_Time (Poll : Duration);
--
-- Set_Auto_Scale -- Set graphs into auto-scaling mode
--
   procedure Set_Auto_Scale;
--
-- Set_Fixed_Scale -- Set graphs into fixed scale mode
--
   procedure Set_Fixed_Scale (Low, High : Centigrade);
--
-- Set_GUI -- Initialize GUI interface
--
   procedure Set_GUI (List : not null access GUI_Interface'Class);
--
-- Set_Trace_Box -- Initialize tracing box
--
   procedure Set_Trace_Box (Box : Trace_Box);

   type Store_Mode is
        (  Store_Start,
           Store_Schedule,
           Store_Parameters,
           Store_Valve,
           Store_Stop
        );
   subtype Store_Action is Store_Mode range Store_Schedule..Store_Valve;
   function Image (Action : Store_Mode) return String;
   function Never return Boolean;
   function Next
             (  Action     : Store_Mode;
                Schedule   : Boolean;
                Parameters : Boolean;
                Valve      : Boolean;
                Cycle      : not null access function return Boolean :=
                             Never'Access
             )  return Store_Mode;

   type IO_Blocker is
      new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Initialize (Block : in out IO_Blocker);
   overriding procedure Finalize (Block : in out IO_Blocker);
   function Is_Free (Block : IO_Blocker) return Boolean;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  GNAT.Sockets.Server.Connections_Server'Class,
             Server_Ptr
          );

   function Compare_Schedule
            (  Client  : Cube_Client;
               Address : RF_Address;
               Day     : Week_Day;
               Data    : Week_Schedule
            )  return Boolean;
   function Compare_Schedule
            (  Client  : Cube_Client;
               Address : RF_Address;
               Day     : Week_Day;
               Data    : Device_Parameters
            )  return Boolean;
   function Compare_Parameters
            (  Client  : Cube_Client;
               Address : RF_Address;
               Data    : Device_Parameters
            )  return Boolean;
   function Compare_Valve
            (  Client  : Cube_Client;
               Address : RF_Address;
               Data    : Device_Parameters
            )  return Boolean;
--
-- Trace_Draw -- Trace draw event (debugging)
--
   procedure Trace_Draw
             (  Widget : not null access Gtk_Widget_Record'Class
             );
--
-- Wake_Up -- Worker wake up
--
   procedure Wake_Up;

   function Value (Text : String) return RF_Address;

   type Trace_Request is record
      Mode    : Trace_Type;
      Message : Unbounded_String;
   end record;
   procedure Service (Request : in out Trace_Request);
--
-- Version_Update -- Version update notification
--
   procedure Version_Update   (Major, Minor : Positive);
   procedure Version_Uptodate (Major, Minor : Positive);
   procedure Version_Unknown;

private
   type ELV_Factory is new Connections_Factory with null record;
   type ELV_Secure_Factory is
      new GNAT.Sockets.Server.Secure.X509.
          X509_Authentication_Factory with null record;

   type Cube_Client
        (  Server : not null access Connections_Server'Class
        )  is new ELV_MAX_Cube_Client
                  (  Listener       => Server,
                     Line_Length    => 1024 * 64,
                     Input_Size     => 200,
                     Output_Size    => 2000,
                     Secondary_Size => 2048
                  )  with
   record
      Created : Time := Clock;
      ID      : Cube_Descriptor;
         -- Valve position statistics
      Average : Natural := 0;
      Max     : Natural := 0;
      Min     : Natural := 0;
      Known_Address  : RF_Address := 0;
      Faulty_Devices : RF_Address_Sets.Set;
         -- Device configuration quering
      Configuration_Queried : Boolean := False;

      pragma Atomic (Average);
      pragma Atomic (Max);
      pragma Atomic (Min);
   end record;

   overriding
      procedure Data_Received
                (  Client : in out Cube_Client;
                   Data   : Device_Data
                );
   overriding
      procedure Faulty_Device_Received
                (  Client      : in out Cube_Client;
                   Device      : RF_Address;
                   Length      : Natural;
                   Error       : Boolean;
                   Initialized : Boolean;
                   Orphaned    : Boolean
                );
   overriding
      procedure Status_Received
                (  Client : in out Cube_Client;
                   Error  : Boolean;
                   Duty   : Ratio;
                   Slots  : Natural
                );
   overriding
      procedure Trace
                (  Client  : in out Cube_Client;
                   Message : String
                );

   function "=" (Left, Right : Cube_Descriptor) return Boolean;
   function "<" (Left, Right : Cube_Descriptor) return Boolean;

   package Cube_List is
      new Generic_Map (Cube_Descriptor, Cube_Client_Handle);

   function Image (Address : System.Address) return String;
------------------------------------------------------------------------
--
-- S-commands request
--
   type Settings_Request is abstract tagged limited record
      Cube        : RF_Address;
      Client      : Cube_Client_Handle;
      No          : Settings_Sequence_No;
      Started     : Time;
      Silent_Mode : Boolean := True;
      Error       : Boolean := False; -- An S-command had error
      Reported    : Boolean := False;
      Expected    : Natural := 0;
      pragma Atomic (Expected);
   end record;
   procedure Complete
             (  Request  : in out Settings_Request;
                Canceled : Boolean
             );
   procedure Continue
             (  Request : in out Settings_Request;
                Client  : in out Cube_Client'Class
             )  is abstract;
   function Image
            (  Request : Settings_Request
            )  return String is abstract;
   function Is_Handled (Request : Settings_Request) return Boolean;
   procedure Report
             (  Request : in out Settings_Request;
                Message : String;
                Title   : String := "Communication error"
             );
   procedure Start
             (  Request : in out Settings_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is abstract;
--
-- Settings_Request_List -- Of requests
--
   package Settings_Request_List is
      new Generic_Doubly_Linked (Settings_Request'Class);
   procedure Register
             (  Handler : Settings_Handler_Ptr;
                Request : Settings_Request_List.Doubly_Linked.Item
             );

   type Completed_Message is record
      Canceled : Boolean;
      No       : Settings_Sequence_No;
   end record;
   package Completed_Messages is
      new Gtk.Main.Router.Generic_Message (Completed_Message);
   procedure Service (Request : in out Completed_Message);
------------------------------------------------------------------------
   type Connected_Request is new Gtk.Main.Router.Request_Data with
   record
      ID : Cube_Descriptor;
   end record;
   overriding
      procedure Service (Request : in out Connected_Request);

   type Disconnected_Request is new Gtk.Main.Router.Request_Data with
   record
      ID : Cube_Descriptor;
   end record;
   overriding
      procedure Service (Request : in out Disconnected_Request);

   type Cube_Statistics_Message is record
      Address : RF_Address;
      Average : Natural;
   end record;
   package Cube_Statistics_Messages is
      new Gtk.Main.Router.Generic_Message (Cube_Statistics_Message);
   procedure Service (Request : in out Cube_Statistics_Message);

   type Faulty_Device_Request is
      new Gtk.Main.Router.Request_Data with
   record
      Cube        : RF_Address;
      Device      : RF_Address;
      Length      : Natural;
      Error       : Boolean;
      Initialized : Boolean;
      Orphaned    : Boolean;
   end record;
   overriding
      procedure Service (Request : in out Faulty_Device_Request);

   type Data_Received_Request (Kind_Of : Device_Type) is
      new Gtk.Main.Router.Request_Data with
   record
      Cube   : RF_Address;
      Offset : Centigrade;
      Data   : Device_Data (Kind_Of);
   end record;
   overriding
      procedure Service (Request : in out Data_Received_Request);

   type Detached_Device_Message is record
      Cube      : RF_Address;
      Kind_Of   : Device_Type;
      Device    : RF_Address;
      Serial_No : String (1..10);
   end record;
   procedure Service (Request : in out Detached_Device_Message);
   package Detached_Device_Messages is
      new GTK.Main.Router.Generic_Message (Detached_Device_Message);

   type NTP_Update_Message is record
      Cube : RF_Address;
      List : Servers_List.Set;
   end record;
   procedure Service (Request : in out NTP_Update_Message);
   package NTP_Update_Messages is
      new GTK.Main.Router.Generic_Message (NTP_Update_Message);

   type Pairing_Update_Message (Ended : Boolean := True) is record
      Cube : RF_Address;
      case Ended is
         when False =>
            Kind_Of   : Device_Type;
            Address   : RF_Address;
            Serial_No : String (1..10);
         when True =>
            null;
      end case;
   end record;
   procedure Service (Request : in out Pairing_Update_Message);
   package Pairing_Messages is
      new GTK.Main.Router.Generic_Message (Pairing_Update_Message);

   type Status_Received_Message is record
      Cube  : Cube_Client_Handle;
      Error : Boolean;
      Duty  : Ratio;
      Slots : Natural;
   end record;
   package Status_Messages is
      new GTK.Main.Router.Generic_Message (Status_Received_Message);

   package Trace_Messages is
      new Gtk.Main.Router.Generic_Message (Trace_Request);

   type Trace_IO_Request is record
      Mode    : Trace_Type;
      Stamp   : Time := Clock;
      ID      : Unbounded_String;
      Message : Unbounded_String;
   end record;
   procedure Service (Request : in out Trace_IO_Request);

   package Trace_IO_Messages is
      new Gtk.Main.Router.Generic_Message (Trace_IO_Request);

   type Update_Cube_Request (Length : Natural) is
      new Gtk.Main.Router.Request_Data with
   record
      Address   : RF_Address;
      Source    : GNAT.Sockets.Sock_Addr_Type;
      Serial_No : String (1..Length);
   end record;
   overriding
      procedure Service (Request : in out Update_Cube_Request);

   type Update_Devices_Request is record
      Address   : RF_Address;
      Source    : GNAT.Sockets.Sock_Addr_Type;
      Devices   : Devices_Maps.Map;
      Rooms     : Rooms_Maps.Map;
      Serial_No : String (1..10);
   end record;
   procedure Service (Request : in out Update_Devices_Request);

   package Update_Devices_Messages is
      new Gtk.Main.Router.Generic_Message (Update_Devices_Request);

   type Update_Parameters_Request
        (  Kind_Of     : Device_Type;
           Name_Length : Natural;
           Cube        : RF_Address
        )  is new Gtk.Main.Router.Request_Data with
   record
      Parameters : Device_Parameters (Kind_Of, Name_Length);
   end record;
   overriding
      procedure Service (Request : in out Update_Parameters_Request);

   type Update_Version_Message is record
      Major   : Natural := 0;
      Minor   : Natural := 0;
      Version : Version_Status := Unknown;
   end record;
   package Update_Version_Messages is
      new GTK.Main.Router.Generic_Message (Update_Version_Message);

   type Messages_Filter is
       new Gtk.Main.Router.GNAT_Stack.Log_Filter with null record;
   overriding function Ignore
                       (  Filter  : not null access Messages_Filter;
                          Domain  : String;
                          Level   : Log_Level_Flags;
                          Message : UTF8_String
                       )  return Boolean;

--
-- Device_Status_Data -- Device status
--
   type Device_Status_Data (Kind_Of : Device_Type) is
      new Object.Entity with
   record
      Status : Device_Data (Kind_Of);
   end record;
   type Device_Status_Data_Ptr is
      access Device_Status_Data'Class;
   package Device_Status_Data_Handles is
      new Object.Handle
          (  Device_Status_Data,
             Device_Status_Data_Ptr
          );

   subtype Managed_Mode is Operating_Mode range Automatic..Manual;
   type Action_Type is
        (  No_Action,
           Switch_Forward,
           Switch_Backward
        );
   type Room_Type is (Unknown, Unmanaged_Room, Managed_Room);
   type Device_Control is new Object.Entity with record
      Action      : Action_Type := No_Action;
      Control     : Room_Type   := Unknown;
      Box         : RF_Address;
      Mode        : Managed_Mode;
      Temperature : Centigrade;
      Timeout     : Duration;
      Last_Switch : Time;
   end record;
   procedure Set_Control
             (  Device  : in out Device_Control;
                Address : RF_Address;
                Cube    : in out Cube_Client'Class
             );
   procedure Switch
             (  Device  : in out Device_Control;
                Address : RF_Address;
                Cube    : in out Cube_Client'Class;
                Mode    : Managed_Mode
             );
   procedure Switch_Back
             (  Device  : in out Device_Control;
                Address : RF_Address;
                Data    : Device_Data;
                Now     : Time;
                Cube    : in out Cube_Client'Class
             );
   procedure Switch_Forth
             (  Device      : in out Device_Control;
                Address     : RF_Address;
                From        : Managed_Mode;
                Temperature : Centigrade;
                Now         : Time;
                Cube        : in out Cube_Client'Class
             );
   type Device_Control_Ptr is access Device_Control'Class;

   package Device_Control_Handles is
      new Object.Handle (Device_Control, Device_Control_Ptr);
   package Device_Maps is
      new Generic_Map (RF_Address, Device_Control_Handles.Handle);
------------------------------------------------------------------------
--
-- Worker -- The worker task
--
   task Worker is
      entry Add_Manually (Address : UTF8_String);
      entry Dequeue (No : Settings_Sequence_No; Cancel : Boolean);
      entry Enqueue
            (  Request : Settings_Request_List.Doubly_Linked.Item
            );
      entry Get
            (  Box    : RF_Address;
               Device : RF_Address;
               Data   : in out Device_Parameters_Data_Handles.Handle
            );
      entry Get_Cube
            (  Box  : RF_Address;
               Cube : in out Cube_Client_Handle
            );
      entry Get_Cube
            (  Address : GNAT.Sockets.Inet_Addr_Type;
               Cube    : in out Cube_Client_Handle
            );
      entry Get_Cubes_List (Cubes : in out RF_Address_Sets.Set);
      entry Get_Device_Data
            (  Box    : RF_Address;
               Device : RF_Address;
               Data   : in out Device_Status_Data_Handles.Handle
            );
      entry Get_Devices_List
            (  Box     : RF_Address;
               Devices : in out RF_Address_Sets.Set;
               Filter  : Device_Type_Set;
               No_Cube : out Boolean
            );
      entry Get_Device_Data
            (  Box   : RF_Address;
               Index : in out Positive;
               Data  : in out Device_Status_Data_Handles.Handle
            );
      entry Get_Device_Name
            (  Box    : RF_Address;
               Device : RF_Address;
               Data   : in out Unbounded_String
            );
      entry Get_Radiator_Thermostats
            (  Box  : RF_Address;
               Room : Room_ID;
               Set  : in out RF_Address_Sets.Set
            );
      entry Get_Room_Thermostats
            (  Box    : RF_Address;
               Room   : Room_ID;
               Filter : Device_Type_Set;
               Set    : in out RF_Address_Maps.Map
            );
      entry Get_Rooms_List
            (  Box     : RF_Address;
               Rooms   : in out Room_To_Device_List.Map;
               Filter  : Device_Type_Set;
               No_Cube : out Boolean
            );
      entry Get_Thermostats (Set : in out RF_Address_Sets.Set);
      entry Get_Topology
            (  Box      : RF_Address;
               Topology : in out Topology_Handles.Handle
            );
      entry Get_Valves
            (  Box     : RF_Address;
               Average : out Natural;
               Min     : out Natural;
               Max     : out Natural
            );
      entry Reconnect
            (  Address       : GNAT.Sockets.Inet_Addr_Type;
               Connect_Again : Boolean;
               Silent        : Boolean
            );
      entry Scan (Address : UTF8_String);
      entry Set_Poll (Poll : Duration);
      entry Shutdown;
      entry Wake_Up;
   end Worker;

end MAX_IO;
