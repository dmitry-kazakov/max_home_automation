--                                                                    --
--  package MAX_Settings_Page       Copyright (c)  Dmitry A. Kazakov  --
--  Settings page                                  Luebeck            --
--  Interface                                      Autumn, 2015       --
--                                                                    --
--                                Last revision :  09:07 16 Jun 2023  --
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

with GNAT.Sockets;           use GNAT.Sockets;
with GNAT.Sockets.Server;    use GNAT.Sockets.Server;
with Gtk.Box;                use Gtk.Box;
with Gtk.Check_Button;       use Gtk.Check_Button;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Grid;               use Gtk.Grid;
with Gtk.Notebook;           use Gtk.Notebook;
with Gtk.Widget;             use Gtk.Widget;
with Max_Icon_Factory;       use Max_Icon_Factory;
with MAX_IO;                 use MAX_IO;
with MAX_MQTT_Server.State;  use MAX_MQTT_Server.State;

with Ada.Streams.Stream_IO;
with Generic_Doubly_Linked;
with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
with GNAT.Sockets.Connection_State_Machine.HTTP_Server.WebSocket_Server;
with Gtk.Handlers;
with Tables.Names;

package MAX_Settings_Page is
   use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
   use GNAT.Sockets.Connection_State_Machine.HTTP_Server;
   use WebSocket_Server;

   type MAX_Settings_Record is new Gtk_Widget_Record with private;
   type MAX_Settings is access all MAX_Settings_Record'Class;

   function Gtk_Settings_New return MAX_Settings;
   procedure Update_Host;
--
-- Messages_Queues -- A list of trace requests
--
   package Messages_Queues is new Generic_Doubly_Linked (Trace_Request);
--
-- Replace_Settings -- Replace all application settings
--
--    File_Name - To get new settings from
--
-- The function removes all keys associated with the application in  the
-- default recent manager. Then keys from the file are pushed.
--
-- Returns :
--
--    List of trace messages to show when tracing becomes active
--
   function Replace_Settings (File_Name : String)
      return Messages_Queues.Doubly_Linked.List;

   Pages : Gtk_Notebook;

private
   type MAX_HTTP_Server is new Connections_Server with null record;
   overriding
      function Get_Server_Address
               (  Listener : MAX_HTTP_Server
               )  return Sock_Addr_Type;

   type MAX_HTTP_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Max_Connections : Positive
        )  is new MAX_IO.ELV_Factory with null record;
   overriding
      function Create
               (  Factory  : access MAX_HTTP_Factory;
                  Listener : access Connections_Server'Class;
                  From     : Sock_Addr_Type
               )  return Connection_Ptr;

   type Page_File is new Ada.Streams.Stream_IO.File_Type;
   procedure Write
             (  Stream : not null access Ada.Streams.
                                         Root_Stream_Type'Class;
                File   : Page_File
             );
   for Page_File'Write use Write;

   type MAX_HTTP_Client
        (  Listener        : access Connections_Server'Class;
           Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Factory         : access Connections_Factory'Class;
           Buffer_Size     : Buffer_Length
        )  is new HTTP_WebSocket_Client
                  (  Listener       => Listener,
                     Request_Length => Request_Length,
                     Input_Size     => Input_Size,
                     Output_Size    => Output_Size,
                     Factory        => Factory,
                     Buffer_Size    => Buffer_Size
                  )  with
   record
      Default : Page_File;
   end record;
   overriding
      procedure Body_Sent
                (  Client : in out MAX_HTTP_Client;
                   Stream : in out Ada.Streams.Root_Stream_Type'Class;
                   Get    : Boolean
                );
   overriding
      procedure Do_Get  (Client : in out MAX_HTTP_Client);
   overriding
      procedure Do_Head (Client : in out MAX_HTTP_Client);
   overriding
      procedure Do_Options (Client : in out MAX_HTTP_Client);
   overriding
      procedure Disconnected (Client : in out MAX_HTTP_Client);
   overriding
      procedure Initialize (Client : in out MAX_HTTP_Client);
   overriding
      procedure Finalize (Client : in out MAX_HTTP_Client);

   procedure Close_Default (Client : in out MAX_HTTP_Client);
   procedure Do_Get_Head
             (  Client : in out MAX_HTTP_Client;
                Get    : Boolean
             );

   type MAX_Settings_Record is new Gtk_VBox_Record with record
      Grid                   : Gtk_Grid;

      Enable_HTTP            : Gtk_Check_Button;
      HTTP_Port              : Gtk_GEntry;
      HTTP_Trace             : Gtk_Check_Button;
      HTTP_Address           : Gtk_GEntry;
      HTTP_CORS_Origin       : Gtk_GEntry;
      HTTP_CORS_Methods      : Gtk_GEntry;
      HTTP_CORS_Headers      : Gtk_GEntry;
      HTTP_CORS_Max_Age      : Gtk_GEntry;
      HTTP_Default_Page      : Gtk_GEntry;
      HTTP_Browse_Button     : Page_File_Buttons.Gtk_Style_Button;

      Enable_MQTT            : Gtk_Check_Button;
      MQTT_Port              : Gtk_GEntry;
      MQTT_Trace             : Gtk_Check_Button;
      MQTT_Address           : Gtk_GEntry;
      MQTT_Enable_Publishing : Gtk_Check_Button;
      MQTT_Topics_List       : Gtk_GEntry;
      MQTT_Max_Connections   : Gtk_GEntry;
      MQTT_Max_Messages      : Gtk_GEntry;
      MQTT_Max_Subscriptions : Gtk_GEntry;
      MQTT_Push              : Gtk_Check_Button;

      Enable_Web_MQTT        : Gtk_Check_Button;

      ELV_Trace              : Gtk_Check_Button;
      Decoded_Trace          : Gtk_Check_Button;
      DB_Trace               : Gtk_Check_Button;
      Poll                   : Gtk_GEntry;
      Graph_Width            : Gtk_GEntry;
      Graph_Low_Temperature  : Gtk_GEntry;
      Graph_High_Temperature : Gtk_GEntry;
      Graph_Add_Offset       : Gtk_Check_Button;
      Graph_Fixed_Scale      : Gtk_Check_Button;
      Scan                   : Gtk_Check_Button;
      Scan_Period            : Gtk_GEntry;
      Scan_Timeout           : Gtk_GEntry;
      Temperature_Timeout    : Gtk_GEntry;

      Enable_Discovery       : Gtk_Check_Button;
      Host_Address           : Gtk_GEntry;
      Cube_Address           : Gtk_GEntry;

      Enable_GNUTLS_Trace    : Gtk_Check_Button;
      GNUTLS_Trace_Level     : Gtk_GEntry;
      GNUTLS_Trace_File      : Gtk_GEntry;
      GNUTLS_Browse_File     : Trace_File_Buttons.Gtk_Style_Button;

      Export_Settings        : Export_Buttons.Gtk_Style_Button;
      Export_CSS             : Properties_Buttons.Gtk_Style_Button;
   end record;
   function Get_Topics_List
            (  Settings : not null access MAX_Settings_Record;
               Silent   : Boolean
            )  return Topic_Sets.Set;
   procedure Start_HTTP
             (  Settings : not null access MAX_Settings_Record
             );
   procedure Start_MQTT
             (  Settings    : not null access MAX_Settings_Record;
                Enable_List : Boolean;
                Topics_List : Topic_Sets.Set
             );
------------------------------------------------------------------------
   type MAX_MQTT_Server is new Connections_Server with null record;
   overriding
      function Get_Server_Address
               (  Listener : MAX_MQTT_Server
               )  return Sock_Addr_Type;

   type MAX_MQTT_Factory
        (  Input_Size  : Buffer_Length;
           Output_Size : Buffer_Length
        )  is new MAX_IO.ELV_Factory with
   record
      Max_Connections   : Positive := 100;
      Max_Subscriptions : Positive := 20;
      pragma Atomic (Max_Connections);
      pragma Atomic (Max_Subscriptions);
   end record;
   function Create
            (  Factory  : access MAX_MQTT_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;

   type MAX_MQTT_WebSocket_Factory is
      new MAX_MQTT_Factory with null record;
   function Create
            (  Factory  : access MAX_MQTT_WebSocket_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
------------------------------------------------------------------------
   procedure Check_Spelling (Name : String);
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;

   type Action is
        (  Get_Connection_Action,
           Reconnect_Action,
           Disconnect_Action,
           Get_Cubes_List_Action,
           Get_Cubes_List_JSON_Action,
           Get_Duty_Action,
           Get_Status_Action,
           Get_Status_CSV_Action,
           Get_Status_JSON_Action,
           Get_Temperature_Action,
           Get_Set_Temperature_Action,
           Get_Thermostat_Mode_Action,
           Get_Battery_Action,
           Get_Link_Action,
           Get_Summer_Time_Action,
           Get_Rooms_List_Action,
           Get_Rooms_List_JSON_Action,
           Get_Valve_Action,
           Get_Valve_Average_Action,
           Get_Valve_Max_Action,
           Get_Valve_Min_Action,
           Set_Automatic_Action,
           Set_Manual_Action,
           Set_Vacation_Action,
           Set_Boost_Action,
           Set_Eco_Temperature_Action,
           Set_Schedule_Action,
           Reboot_Action
        );
   subtype Cube_Mode_Action is
      Action range Get_Connection_Action..Disconnect_Action;
   subtype Get_Mode_Action is
      Action range Get_Duty_Action..Get_Valve_Min_Action;
   subtype Set_Mode_Action is
      Action range Set_Automatic_Action..Set_Boost_Action;

   package Action_Tables_Raw is new Tables (Action);
   package Action_Tables is new Action_Tables_Raw.Names;

   type Parameter is
        (  Callback_Parameter,
           Cube_Address_Parameter,
           Device_Address_Parameter,
           Temperature_Parameter,
           Inc_Temperature_Parameter,
           Dec_Temperature_Parameter,
           Airing_Parameter,
           Comfort_Parameter,
           Eco_Parameter,
           Weeks_Parameter,
           Days_Parameter,
           Hours_Parameter,
           Minutes_Parameter,
           Schedule_Parameter,
           Serial_Parameter
        );
   package Parameter_Tables_Raw is new Tables (Parameter);
   package Parameter_Tables is new Parameter_Tables_Raw.Names;

   procedure Stop_HTTP;
   procedure Stop_MQTT;

   package Settings_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             MAX_Settings
          );
   use Settings_Handlers;

   procedure Graph_Width_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Cube_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_DB_Trace_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Decoded_Trace_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Discovery_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_ELV_Trace_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Export_CSS
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Export_Settings
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_HTTP_Browse
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_GNUTLS_Browse
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_GNUTLS_Trace_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Graph_Add_Offset
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Graph_Fixed_Scale
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Host_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_HTTP_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_HTTP_Trace
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_MQTT_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_MQTT_Publishing_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_MQTT_Push
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_MQTT_Max_Connnections_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_MQTT_Max_Subscriptions_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_MQTT_Trace
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Scan
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure On_Web_MQTT_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure Poll_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure Scan_Period_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure Scan_Timeout_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );
   procedure Temperature_Timeout_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             );

   function Get_Value
            (  Edit    : Gtk_GEntry;
               Key     : String;
               Min     : Float;
               Max     : Float;
               Default : Float;
               Small   : Integer := -1
            )  return Float;

   function Get_Value
            (  Edit    : Gtk_GEntry;
               Key     : String;
               Min     : Integer;
               Max     : Integer;
               Default : Integer
            )  return Integer;

end MAX_Settings_Page;
