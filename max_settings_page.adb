--                                                                    --
--  package MAX_Settings_Page       Copyright (c)  Dmitry A. Kazakov  --
--  Settings page                                  Luebeck            --
--  Implementation                                 Autumn, 2015       --
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

with Ada.Calendar;                 use Ada.Calendar;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with GLib;                         use GLib;
with GLib.Error;                   use GLib.Error;
with GLib.Messages;                use GLib.Messages;
with GLib.Properties;              use GLib.Properties;
with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.MQTT.Server;     use GNAT.Sockets.MQTT.Server;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.File_Chooser;             use Gtk.File_Chooser;
with Gtk.File_Filter;              use Gtk.File_Filter;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Recent_Manager_Alt;       use Gtk.Recent_Manager_Alt;
with Gtk.Recent_Manager_Keys;      use Gtk.Recent_Manager_Keys;
with Gtk.Widget.Styles.CSS_Store;  use Gtk.Widget.Styles.CSS_Store;
with MAX_IO.Set_Mode;              use MAX_IO.Set_Mode;
with MAX_IO.Set_Schedule;          use MAX_IO.Set_Schedule;
with MAX_IO.Storing_Parameters;    use MAX_IO.Storing_Parameters;
with MAX_MQTT_Server;              use MAX_MQTT_Server;
with MAX_Trace;                    use MAX_Trace;
with Strings_Edit;                 use Strings_Edit;
with Strings_Edit.Floats;          use Strings_Edit.Floats;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Quoted;          use Strings_Edit.Quoted;

with Ada.Directories;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with GNUTLS;
with Generic_Indefinite_Map;
with Interfaces.C;

package body MAX_Settings_Page is
   use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
   use GNAT.Sockets.Server;
   use MAX_Icon_Factory;

   subtype Temperature_Scale is Centigrade range 0.0..30.0;
   Edit_Width     : constant := 20;
   WebSocket_Size : constant := 1024;
   Settings       : MAX_Settings;
   Actions_List   : Action_Tables.Dictionary;
   Parameter_List : Parameter_Tables.Dictionary;
   HTTP_Factory   : aliased MAX_HTTP_Factory
                            (  Request_Length  => 200,
                               Input_Size      => 1024,
                               Output_Size     => 1024,
                               Max_Connections => 100
                            );
   MQTT_Factory   : aliased MAX_MQTT_Factory
                            (  Input_Size  => 1024,
                               Output_Size => 1024
                            );
   MQTT_WebSockets_Factory : aliased MAX_MQTT_Factory
                                     (  Input_Size  => 1024,
                                        Output_Size => 1024
                                     );
   Home : constant String :=
             "http://www.dmitry-kazakov.de/ada/max_home_automation.htm";

   package MIME_Maps is new Generic_Indefinite_Map (String, String);
   MIME_Types : MIME_Maps.Map;

   function Where (Name : String) return String is
   begin
      return " in MAX_Settings_Page." & Name;
   end Where;

   function Encode_CSV
            (  Data      : Device_Data;
               Serial_No : String;
               Name      : String;
               Offset    : Centigrade
            )  return String is
      function Image (Value : Boolean) return String is
      begin
         if Value then
            return "1";
         else
            return "0";
         end if;
      end Image;
      Text    : String (1..1024);
      Pointer : Integer := Text'First;
   begin
      Put (Text, Pointer, Integer (Device_Type'Pos (Data.Kind_Of)));
      Put (Text, Pointer, ";");
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Integer (Data.Address),
         Base        => 16,
         Field       => 6,
         Fill        => '0',
         Justify     => Right
      );
      Put (Text, Pointer, ";");
      Put (Text, Pointer, Image (Data.Error));
      Put (Text, Pointer, ";");
      Put (Text, Pointer, Image (Data.Initialized));
      Put (Text, Pointer, ";");
      Put (Text, Pointer, Image (Data.Battery_Low));
      Put (Text, Pointer, ";");
      Put (Text, Pointer, Image (Data.Link_Error));
      Put (Text, Pointer, ";");
      Put (Text, Pointer, Image (Data.Panel_Locked));
      Put (Text, Pointer, ";");
      Put (Text, Pointer, Image (Data.Gateway_Known));
      Put (Text, Pointer, ";");
      Put (Text, Pointer, Image (Data.DST));
      Put (Text, Pointer, ";");
      case Data.Mode is
         when Automatic => Put (Text, Pointer, "0;");
         when Manual    => Put (Text, Pointer, "1;");
         when Boost     => Put (Text, Pointer, "2;");
         when Vacation  => Put (Text, Pointer, "3;");
      end case;
      case Data.Kind_Of is
         when Cube | Eco_Button | Unknown =>
            Put (Text, Pointer, "0;0;0;0;0");
         when Shutter_Contact =>
            Put (Text, Pointer, "0;0;0;0;");
            Put (Text, Pointer, Image (Data.Open));
         when Radiator_Thermostat..Wall_Thermostat =>
            Put (Text, Pointer, Image (Data.Set_Temperature));
            Put (Text, Pointer, ";");
            Put (Text, Pointer, Image (Data.New_Temperature));
            Put (Text, Pointer, ";");
            case Data.Kind_Of is
               when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                  Put
                  (  Text,
                     Pointer,
                     Image
                     (  Integer (Float (Data.Valve_Position) * 100.0)
                  )  );
                  Put (Text, Pointer, ";");
                  if Data.Temperature /= Centigrade'First then
                     Put (Text, Pointer, Image (Data.Temperature));
                  elsif Data.Latest_Temperature /= Centigrade'First then
                     Put
                     (  Text,
                        Pointer,
                        Image (Data.Latest_Temperature)
                     );
                  else
                     Put (Text, Pointer, "0");
                  end if;
                  Put (Text, Pointer, ";0");
               when Wall_Thermostat =>
                  Put (Text, Pointer, "0;");
                  Put (Text, Pointer, Image (Data.Temperature));
                  Put (Text, Pointer, ";0");
               when others =>
                  null;
            end case;
      end case;
      Put (Text, Pointer, ";" & Image (Offset) & ";" & Serial_No);
      Put (Text, Pointer, ";" & Name);
      Put (Text, Pointer, CRLF);
      return Text (Text'First..Pointer - 1);
   end Encode_CSV;

   procedure Free (List : Gtk_Recent_Info_Array) is
   begin
      for Index in List'Range loop
         Unref (List (Index));
      end loop;
   end Free;

   protected HTTP_Parameters is
      function Address return String;
      function Port return GNAT.Sockets.Port_Type;
      procedure Set (Address : String; Port : GNAT.Sockets.Port_Type);
   private
      HTTP_Address : Unbounded_String;
      HTTP_Port    : GNAT.Sockets.Port_Type := 80;
   end HTTP_Parameters;

   protected MQTT_Parameters is
      function Address return String;
      function Port return GNAT.Sockets.Port_Type;
      procedure Set (Address : String; Port : GNAT.Sockets.Port_Type);
   private
      MQTT_Address : Unbounded_String;
      MQTT_Port    : GNAT.Sockets.Port_Type :=
                     GNAT.Sockets.MQTT.MQTT_Port;
   end MQTT_Parameters;

   procedure Body_Sent
             (  Client : in out MAX_HTTP_Client;
                Stream : in out Ada.Streams.Root_Stream_Type'Class;
                Get    : Boolean
             )  is
   begin
      Client.Close_Default;
      HTTP_WebSocket_Client (Client).Body_Sent (Stream, Get);
   end Body_Sent;

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
   begin
      return True;
   end Check_Matched;

   procedure Close_Default (Client : in out MAX_HTTP_Client) is
   begin
      if Is_Open (Client.Default) then
         begin
            Close (Client.Default);
         exception
            when others =>
               null;
         end;
      end if;
   end Close_Default;

   function Create
            (  Factory  : access MAX_HTTP_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      if Get_Clients_Count (Listener.all) < Factory.Max_Connections then
         Result :=
            new MAX_HTTP_Client
                (  Listener       => Listener.all'Unchecked_Access,
                   Request_Length => Factory.Request_Length,
                   Input_Size     => Factory.Input_Size,
                   Output_Size    => Factory.Output_Size,
                   Buffer_Size    => WebSocket_Size,
                   Factory        =>
                                MQTT_WebSockets_Factory'Unchecked_Access
                );
--       Receive_Body_Tracing   (MAX_HTTP_Client (Result.all), True);
--       Receive_Header_Tracing (MAX_HTTP_Client (Result.all), True);
         return Result;
      else
         return null;
      end if;
   end Create;

   function Create
            (  Factory  : access MAX_MQTT_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      if Get_Clients_Count (Listener.all) < Factory.Max_Connections then
         Result :=
            new MAX_MQTT_Client
                (  Listener             => Listener,
                   Input_Size           => Factory.Input_Size,
                   Output_Size          => Factory.Output_Size,
                   Max_Subscribe_Topics => Factory.Max_Subscriptions,
                   Server               => MQTT_State
                );
         return Result;
      else
         return null;
      end if;
   end Create;

   function Create
            (  Factory  : access MAX_MQTT_WebSocket_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      if Settings.Enable_Web_MQTT.Get_Active then
         Result :=
            new MAX_MQTT_Client
                (  Listener             => Listener,
                   Input_Size           => Factory.Input_Size,
                   Output_Size          => Factory.Output_Size,
                   Max_Subscribe_Topics => Factory.Max_Subscriptions,
                   Server               => MQTT_State
                );
         return Result;
      else
         return null;
      end if;
   end Create;

   procedure Disconnected (Client : in out MAX_HTTP_Client)  is
   begin
      Client.Close_Default;
      HTTP_WebSocket_Client (Client).Disconnected;
   end Disconnected;

   procedure Do_CORS (Client : in out MAX_HTTP_Client) is
   begin
      if Trim (Settings.HTTP_CORS_Origin.Get_Text) /= "" then
         Send
         (  Client,
            (  "Access-Control-Allow-Origin: "
            &  Settings.HTTP_CORS_Origin.Get_Text
            &  CRLF
         )  );
         Send
         (  Client,
            (  "Access-Control-Allow-Methods: "
            &  Settings.HTTP_CORS_Methods.Get_Text
            &  CRLF
         )  );
         Send
         (  Client,
            (  "Access-Control-Allow-Headers: "
            &  Settings.HTTP_CORS_Headers.Get_Text
            &  CRLF
         )  );
         Send
         (  Client,
            (  "Access-Control-Max-Age: "
            &  Settings.HTTP_CORS_Max_Age.Get_Text
            &  CRLF
         )  );
      end if;
   end Do_CORS;

   procedure Do_Get_Head
             (  Client : in out MAX_HTTP_Client;
                Get    : Boolean
             )  is
      procedure Get_Address
                (  Source  : String;
                   Pointer : in out Integer;
                   Address : out RF_Address
                )  is
      begin
         Strings_Edit.Integers.Get
         (  Source  => Source,
            Pointer => Pointer,
            Value   => Integer (Address),
            First   => 1,
            Last    => 16#FFFFFF#,
            Base    => 16
         );
      exception
         when End_Error =>
            raise Data_Error with "Missing device RF-address";
         when others =>
            raise Data_Error with "Invalid device RF-address";
      end Get_Address;

      procedure Get_Ampersand
                (  Source  : String;
                   Pointer : in out Integer
                )  is
      begin
         if Source (Pointer) = '&' then
            Pointer := Pointer + 1;
         else
            raise Data_Error with
                  "Missing parameter separating ampersand";
         end if;
      end Get_Ampersand;

      procedure Do_Connection
                (  Query   : String;
                   Command : Cube_Mode_Action
                )  is
         Pointer        : Integer := Query'First;
         Token          : Parameter;
         Cube           : RF_Address;
         Callback_Start : Integer := Query'Last + 1;
         Callback_Stop  : Integer;
         Got_Cube       : Boolean := False;

         procedure Reply (Message : String; Quote : Boolean) is
         begin
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            Send_Connection (Client, False);
            if Callback_Start > Query'Last then
               Send_Content_Type (Client, "text/plain");
               Send_Body (Client, Message);
            else
               Do_CORS (Client);
               Send_Content_Type (Client, "application/json");
               if Quote then
                  Send_Body
                  (  Client,
                     (  Query (Callback_Start..Callback_Stop)
                     &  "("""
                     &  Message
                     &  """);"
                  )  );
               else
                  Send_Body
                  (  Client,
                     (  Query (Callback_Start..Callback_Stop)
                     &  "("
                     &  Message
                     &  ");"
                  )  );
               end if;
            end if;
         end Reply;
      begin
         loop
            begin
               Parameter_Tables.Get
               (  Query,
                  Pointer,
                  Parameter_List,
                  Token
               );
            exception
               when End_Error =>
                  raise Data_Error with
                        "Unknown parameter starting at " &
                        Quote (Query (Pointer..Query'Last));
            end;
            case Token is
               when Callback_Parameter =>
                  Callback_Start := Pointer;
                  Callback_Stop  := Pointer - 1;
                  while Pointer <= Query'Last loop
                     exit when Query (Pointer) = '&';
                     Pointer := Pointer + 1;
                  end loop;
                  Callback_Stop := Pointer - 1;
               when Cube_Address_Parameter =>
                  if Got_Cube then
                     raise Data_Error with
                           "Cube RF-Address is specified twice";
                  else
                     Got_Cube := True;
                  end if;
                  Get_Address (Query, Pointer, Cube);
               when Device_Address_Parameter =>
                  raise Data_Error with
                        "Unexpected device address parameter";
               when Temperature_Parameter..Eco_Parameter =>
                  raise Data_Error with
                        "Unexpected temperature parameter";
               when Schedule_Parameter =>
                  raise Data_Error with
                        "Unexpected schedule parameter";
               when Weeks_Parameter |
                    Days_Parameter  |
                    Hours_Parameter |
                    Minutes_Parameter =>
                  raise Data_Error with "Unexpected time parameter";
               when Serial_Parameter =>
                  raise Data_Error with "Unexpected serial parameter";
            end case;
            exit when Pointer > Query'Last;
            Get_Ampersand (Query, Pointer);
         end loop;
         if not Got_Cube then
            raise Data_Error with "No cube RF-Address specified";
         end if;
         declare
            This : constant Cube_Client_Handle := Get_Cube (Cube);
         begin
            if not This.Is_Valid then
               raise Data_Error with
                     "No cube " & Image (Cube) & " found";
            end if;
            case Command is
               when Get_Connection_Action =>
                  if This.Ptr.Is_Connected then
                     Reply ("connected", True);
                  else
                     Reply ("disconnected", True);
                  end if;
               when Reconnect_Action =>
                  if This.Ptr.Is_Connected then
                     Reply ("connected", True);
                  else
                     This.Ptr.Reconnect (True, True);
                     Reply ("connecting to " & Image (Cube), True);
                  end if;
               when Disconnect_Action =>
                  if This.Ptr.Is_Connected then
                     This.Ptr.Reconnect (False, True);
                     Reply ("disconnecting from " & Image (Cube), True);
                  else
                     Reply ("disconnected", True);
                  end if;
            end case;
         end;
      end Do_Connection;

      procedure Do_File (Name : String) is
         use Ada.Streams.Stream_IO;

         function MIME_Type return String is
         begin
            for Index in reverse Name'Range loop
               if Name (Index) = '.' then
                  declare
                     Offset : constant Integer :=
                                       MIME_Types.Find
                                       (  Name (Index + 1..Name'Last)
                                       );
                  begin
                     exit when Offset <= 0;
                     return MIME_Types.Get (Offset) & ";charset=UTF-8";
                  end;
               end if;
            end loop;
            return "text/plain;charset=UTF-8";
         end MIME_Type;
      begin
         Client.Close_Default;
         begin
            Open (Client.Default, In_File, Name);
         exception
            when others =>
               Client.Reply_Text
               (  404,
                  "Not found",
                  "Page " & Quote (Name) & " file does not exist"
               );
               return;
         end;
         Client.Send_Status_Line (200, "OK");
         Client.Send_Date;
         Client.Send_Server;
         Client.Send_Content_Type (MIME_Type);
         Client.Send_Body (Stream (Client.Default), Get);
      end Do_File;

      procedure Do_Get_Cubes (Query : String) is
         List    : constant RF_Address_Sets.Set := Get_Cubes_List;
         Text    : String (1..List.Get_Size * 7);
         Pointer : Integer := Text'First;
      begin
         for Index in 1..List.Get_Size loop
            if Index > 1 then
               Put (Text, Pointer, " ");
            end if;
            Put (Text, Pointer, Image (List.Get (Index)));
         end loop;
         Send_Status_Line (Client, 200, "OK");
         Send_Date (Client);
         Send_Content_Type (Client, "text/plain");
         Send_Connection (Client, False);
         Send_Body (Client, Text (1..Pointer - 1));
      end Do_Get_Cubes;

      procedure Do_Get_Cubes_JSON (Query : String) is
         Pointer        : Integer := Query'First;
         Token          : Parameter;
         Callback_Start : Integer := Query'Last + 1;
         Callback_Stop  : Integer;

         procedure Reply (Message : String; Quote : Boolean) is
         begin
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            Send_Connection (Client, False);
            if Callback_Start > Query'Last then
               Send_Content_Type (Client, "text/plain");
               Send_Body (Client, Message);
            else
               Do_CORS (Client);
               Send_Content_Type (Client, "application/json");
               if Quote then
                  Send_Body
                  (  Client,
                     (  Query (Callback_Start..Callback_Stop)
                     &  "("""
                     &  Message
                     &  """);"
                  )  );
               else
                  Send_Body
                  (  Client,
                     (  Query (Callback_Start..Callback_Stop)
                     &  "("
                     &  Message
                     &  ");"
                  )  );
               end if;
            end if;
         end Reply;
      begin
         while Pointer <= Query'Last loop
            begin
               Parameter_Tables.Get
               (  Query,
                  Pointer,
                  Parameter_List,
                  Token
               );
            exception
               when End_Error =>
                  raise Data_Error with
                        "Unknown parameter starting at " &
                        Quote (Query (Pointer..Query'Last));
            end;
            case Token is
               when Callback_Parameter =>
                  Callback_Start := Pointer;
                  Callback_Stop  := Pointer - 1;
                  while Pointer <= Query'Last loop
                     exit when Query (Pointer) = '&';
                     Pointer := Pointer + 1;
                  end loop;
                  Callback_Stop := Pointer - 1;
               when Cube_Address_Parameter =>
                  raise Data_Error with
                        "Unexpected cube address parameter";
               when Device_Address_Parameter =>
                  raise Data_Error with
                        "Unexpected device address parameter";
               when Temperature_Parameter..Eco_Parameter =>
                  raise Data_Error with
                        "Unexpected temperature parameter";
               when Schedule_Parameter =>
                  raise Data_Error with
                        "Unexpected schedule parameter";
               when Weeks_Parameter |
                    Days_Parameter  |
                    Hours_Parameter |
                    Minutes_Parameter =>
                  raise Data_Error with "Unexpected time parameter";
               when Serial_Parameter =>
                  raise Data_Error with "Unexpected serial parameter";
            end case;
            exit when Pointer > Query'Last;
            Get_Ampersand (Query, Pointer);
         end loop;
         declare
            List : constant RF_Address_Sets.Set := Get_Cubes_List;
            Text : String (1..List.Get_Size * 10 + 2);
         begin
            Pointer := Text'First;
            Put (Text, Pointer, "[");
            for Index in 1..List.Get_Size loop
               if Index > 1 then
                  Put (Text, Pointer, ",");
               end if;
               Put (Text, Pointer, Image (Integer (List.Get (Index))));
            end loop;
            Put (Text, Pointer, "]");
            Reply (Text (1..Pointer - 1), True);
         end;
      end Do_Get_Cubes_JSON;

      procedure Do_Get_Status
                (  Query : String;
                   Mode  : Get_Mode_Action
                )  is
         Pointer        : Integer := Query'First;
         Token          : Parameter;
         Cube           : RF_Address;
         Device         : RF_Address;
         Callback_Start : Integer := Query'Last + 1;
         Callback_Stop  : Integer;
         Got_Cube       : Boolean := False;
         Got_Device     : Boolean := False;

         procedure Reply (Message : String; Quote : Boolean) is
         begin
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            Send_Connection (Client, False);
            if Callback_Start > Query'Last then
               Send_Content_Type (Client, "text/plain");
               Send_Body (Client, Message);
            else
               Do_CORS (Client);
               Send_Content_Type (Client, "application/json");
               if Quote then
                  Send_Body
                  (  Client,
                     (  Query (Callback_Start..Callback_Stop)
                     &  "("""
                     &  Message
                     &  """);"
                  )  );
               else
                  Send_Body
                  (  Client,
                     (  Query (Callback_Start..Callback_Stop)
                     &  "("
                     &  Message
                     &  ");"
                  )  );
               end if;
            end if;
         end Reply;

      begin
         loop
            begin
               Parameter_Tables.Get
               (  Query,
                  Pointer,
                  Parameter_List,
                  Token
               );
            exception
               when End_Error =>
                  raise Data_Error with
                        "Unknown parameter starting at " &
                        Quote (Query (Pointer..Query'Last));
            end;
            case Token is
               when Callback_Parameter =>
                  Callback_Start := Pointer;
                  Callback_Stop  := Pointer - 1;
                  while Pointer <= Query'Last loop
                     exit when Query (Pointer) = '&';
                     Pointer := Pointer + 1;
                  end loop;
                  Callback_Stop := Pointer - 1;
               when Cube_Address_Parameter =>
                  if Got_Cube then
                     raise Data_Error with
                           "Cube RF-Address is specified twice";
                  else
                     Got_Cube := True;
                  end if;
                  Get_Address (Query, Pointer, Cube);
               when Device_Address_Parameter =>
                  if Got_Device then
                     raise Data_Error with
                           "Device RF-Address is specified twice";
                  elsif (  Mode = Get_Duty_Action
                        or else
                            Mode in Get_Valve_Average_Action
                                 .. Get_Valve_Min_Action
                        )  then
                     raise Data_Error with
                           "Device RF-Address is specified " &
                           "in a duty, valve average, min, " &
                           "or max command";
                  else
                     Got_Device := True;
                  end if;
                  Get_Address (Query, Pointer, Device);
               when Temperature_Parameter..Eco_Parameter =>
                  raise Data_Error with
                        "Unexpected temperature parameter";
               when Schedule_Parameter =>
                  raise Data_Error with
                        "Unexpected schedule parameter";
               when Weeks_Parameter |
                    Days_Parameter  |
                    Hours_Parameter |
                    Minutes_Parameter =>
                  raise Data_Error with "Unexpected time parameter";
               when Serial_Parameter =>
                  raise Data_Error with "Unexpected serial parameter";
            end case;
            exit when Pointer > Query'Last;
            Get_Ampersand (Query, Pointer);
         end loop;
         if not Got_Cube then
            raise Data_Error with "No cube RF-Address specified";
         elsif not Got_Device then
            case Mode is
               when Get_Duty_Action =>
                  Reply
                  (  (  Image
                        (  Float (Get_Cube (Cube).Ptr.Get_Duty) * 100.0,
                           AbsSmall => 0
                        )
                     &  "%"
                     ),
                     True
                  );
               when Get_Status_Action =>
                  Send_Status_Line (Client, 200, "OK");
                  Send_Date (Client);
                  Send_Server (Client);
                  if Callback_Start > Query'Last then
                     Send_Content_Type (Client, "text/plain");
                  else
                     Do_CORS (Client);
                     Send_Content_Type (Client, "application/json");
                     Client.Accumulate_Body
                     (  Query (Callback_Start..Callback_Stop)
                     & "("""
                     );
                  end if;
                  declare
                     Index : aliased Positive := 1;
                  begin
                     loop
                        declare
                           None : aliased Boolean := True;
                           Data : constant Device_Data :=
                                  Get_Device_Data
                                  (  Cube,
                                     Index'Access,
                                     None'Access
                                  );
                        begin
                           exit when None;
                           if (  Data.Kind_Of /= Unknown
                              and then
                                 Data.Address /= 0
                              )  then
                              if Index /= 1 then
                                 Client.Accumulate_Body
                                 (  Character'Val (13)
                                 &  Character'Val (10)
                                 );
                              end if;
                              Client.Accumulate_Body (Image (Data));
                           end if;
                        end;
                        Index := Index + 1;
                     end loop;
                  end;
                  if Callback_Start <= Query'Last then
                     Client.Accumulate_Body (""");");
                  end if;
                  Send_Body (Client, True);
               when Get_Rooms_List_Action =>
                  Send_Status_Line (Client, 200, "OK");
                  Send_Date (Client);
                  Send_Server (Client);
                  if Callback_Start > Query'Last then
                     Send_Content_Type (Client, "text/plain");
                  else
                     Do_CORS (Client);
                     Send_Content_Type (Client, "application/json");
                     Client.Accumulate_Body
                     (  Query (Callback_Start..Callback_Stop)
                     & "("""
                     );
                  end if;
                  begin
                     declare
                        Rooms : constant Room_To_Device_List.Map :=
                                         Get_Rooms_List (Cube);
                     begin
                        for Room in 1..Rooms.Get_Size loop
                           declare
                              This : Room_Devices_List'Class renames
                                     Rooms.Get (Room).Ptr.all;
                           begin
                              for Device in 1..This.List.Get_Size loop
                                 Client.Accumulate_Body
                                 (  Image (This.List.Get_Key (Device))
                                 &  ' '
                                 );
                              end loop;
                              Client.Accumulate_Body
                              (  "#"
                              &  Image (Integer (Rooms.Get_Key (Room)))
                              &  " - "
                              &  This.Room
                              &  CRLF
                              );
                           end;
                        end loop;
                     end;
                  exception
                     when End_Error =>
                        null;
                  end;
                  if Callback_Start <= Query'Last then
                     Client.Accumulate_Body (""");");
                  end if;
                  Send_Body (Client, True);
               when Get_Rooms_List_JSON_Action =>
                  Send_Status_Line (Client, 200, "OK");
                  Send_Date (Client);
                  Send_Server (Client);
                  if Callback_Start > Query'Last then
                     Send_Content_Type (Client, "text/plain");
                  else
                     Do_CORS (Client);
                     Send_Content_Type (Client, "application/json");
                     Client.Accumulate_Body
                     (  Query (Callback_Start..Callback_Stop)
                     & "("""
                     );
                  end if;
                  Client.Accumulate_Body ("[");
                  begin
                     declare
                        Rooms : constant Room_To_Device_List.Map :=
                                         Get_Rooms_List (Cube);
                     begin
                        for Room in 1..Rooms.Get_Size loop
                           declare
                              This : Room_Devices_List'Class renames
                                     Rooms.Get (Room).Ptr.all;
                           begin
                              Client.Accumulate_Body
                              (  "{""name"": "
                              &  Quote (This.Room)
                              &  ", ""id"": "
                              &  Image (Integer (Rooms.Get_Key (Room)))
                              &  ", ""devices"": ["
                              );
                              for Device in 1..This.List.Get_Size loop
                                 if Device /= 1 then
                                    Client.Accumulate_Body (", ");
                                 end if;
                                 Client.Accumulate_Body
                                 (  Image
                                    (  Integer
                                       (  This.List.Get_Key (Device)
                                 )  )  );
                              end loop;
                              if Room = Rooms.Get_Size then
                                 Client.Accumulate_Body ("]}");
                              else
                                 Client.Accumulate_Body ("]}, ");
                              end if;
                           end;
                        end loop;
                        Client.Accumulate_Body ("]");
                     end;
                  exception
                     when End_Error =>
                        null;
                  end;
                  if Callback_Start <= Query'Last then
                     Client.Accumulate_Body (""");");
                  end if;
                  Send_Body (Client, True);
               when Get_Status_CSV_Action =>
                  Send_Status_Line (Client, 200, "OK");
                  Send_Date (Client);
                  Send_Server (Client);
                  if Callback_Start > Query'Last then
                     Send_Content_Type (Client, "text/plain");
                  else
                     Do_CORS (Client);
                     Send_Content_Type (Client, "application/json");
                     Client.Accumulate_Body
                     (  Query (Callback_Start..Callback_Stop)
                     & "("""
                     );
                  end if;
                  declare
                     Index : aliased Positive := 1;
                  begin
                     loop
                        declare
                           None : aliased Boolean := True;
                           Data : constant Device_Data :=
                                  Get_Device_Data
                                  (  Cube,
                                     Index'Access,
                                     None'Access
                                  );
                        begin
                           exit when None;
                           declare
                              Info : constant
                                  Device_Parameters_Data_Handles.Handle :=
                                  Get_Parameters (Cube, Data.Address);
                           begin
                              if (  Data.Kind_Of /= Unknown
                                 and then
                                    Data.Address /= 0
                                 and then
                                    Info.Is_Valid
                                 )  then
                                 declare
                                    Parameters : Device_Parameters renames
                                                 Info.Ptr.Parameters;
                                 begin
                                    case Parameters.Kind_Of is
                                       when Radiator_Thermostat..
                                            Wall_Thermostat =>
                                          Client.Accumulate_Body
                                          (  Encode_CSV
                                             (  Data,
                                                Parameters.Serial_No,
                                                Parameters.Name,
                                                Parameters.Offset
                                          )  );
                                       when others =>
                                          Client.Accumulate_Body
                                          (  Encode_CSV
                                             (  Data,
                                                Parameters.Serial_No,
                                                Parameters.Name,
                                                0.0
                                          )  );
                                    end case;
                                 end;
                              end if;
                           end;
                        end;
                        Index := Index + 1;
                     end loop;
                  end;
                  if Callback_Start <= Query'Last then
                     Client.Accumulate_Body (""");");
                  end if;
                  Send_Body (Client, True);
               when Get_Status_JSON_Action =>
                  Send_Status_Line (Client, 200, "OK");
                  Send_Date (Client);
                  Send_Server (Client);
                  if Callback_Start > Query'Last then
                     Send_Content_Type (Client, "application/json");
                  else
                     Do_CORS (Client);
                     Send_Content_Type (Client, "application/json");
                     Client.Accumulate_Body
                     (  Query (Callback_Start..Callback_Stop)
                     & "("
                     );
                  end if;
                  Client.Accumulate_Body ("{""devices"" : [");
                  declare
                     Index : aliased Positive := 1;
                  begin
                     loop
                        declare
                           None : aliased Boolean := True;
                           Data : constant Device_Data :=
                                  Get_Device_Data
                                  (  Cube,
                                     Index'Access,
                                     None'Access
                                  );
                        begin
                           exit when None;
                           declare
                              Info : constant
                                 Device_Parameters_Data_Handles.
                                 Handle :=
                                    Get_Parameters (Cube, Data.Address);
                           begin
                              if (  Data.Kind_Of /= Unknown
                                 and then
                                    Data.Address /= 0
                                 and then
                                    Info.Is_Valid
                                 )  then
                                 declare
                                    Parameters : Device_Parameters
                                       renames Info.Ptr.Parameters;
                                 begin
                                    if Index /= 1 then
                                       Client.Accumulate_Body (",");
                                    end if;
                                    case Parameters.Kind_Of is
                                       when Radiator_Thermostat..
                                            Wall_Thermostat =>
                                          Client.Accumulate_Body
                                          (  Encode
                                             (  Data,
                                                Parameters.Serial_No,
                                                Parameters.Name,
                                                Parameters.Offset
                                          )  );
                                       when others =>
                                          Client.Accumulate_Body
                                          (  Encode
                                             (  Data,
                                                Parameters.Serial_No,
                                                Parameters.Name,
                                                0.0
                                          )  );
                                    end case;
                                 end;
                              end if;
                           end;
                        end;
                        Index := Index + 1;
                     end loop;
                  end;
                  Client.Accumulate_Body ("]}");
                  if Callback_Start <= Query'Last then
                     Client.Accumulate_Body (");");
                  end if;
                  Send_Body (Client, True);
               when Get_Valve_Average_Action..Get_Valve_Min_Action =>
                  declare
                     Average : Natural;
                     Min     : Natural;
                     Max     : Natural;
                  begin
                     Get_Valves (Cube, Average, Min, Max);
                     case Mode is
                        when Get_Valve_Max_Action =>
                           Reply (Image (Max) & '%', True);
                        when Get_Valve_Min_Action =>
                           Reply (Image (Min) & '%', True);
                        when others =>
                           Reply (Image (Average) & '%', True);
                     end case;
                  end;
               when others =>
                  raise Data_Error with
                        "No Device RF-Address specified";
            end case;
            return;
         end if;
         begin
            declare
               None : aliased Boolean := False;
               Data : constant Device_Data :=
                      Get_Device_Data (Cube, Device, None'Access);
            begin
               case Mode is
                  when Get_Status_Action =>
                     Reply (Image (Data), True);
                  when Get_Status_CSV_Action =>
                     declare
                        Info : constant Device_Parameters_Data_Handles.
                                        Handle :=
                               Get_Parameters (Cube, Data.Address);
                        Parameters : Device_Parameters renames
                                     Info.Ptr.Parameters;
                     begin
                        case Parameters.Kind_Of is
                           when Radiator_Thermostat..Wall_Thermostat =>
                              Reply
                              (  Encode_CSV
                                 (  Data,
                                    Parameters.Serial_No,
                                    Parameters.Name,
                                    Parameters.Offset
                                 ),
                                 True
                              );
                           when others =>
                              Reply
                              (  Encode_CSV
                                 (  Data,
                                    Parameters.Serial_No,
                                    Parameters.Name,
                                    0.0
                                 ),
                                 True
                              );
                        end case;
                     end;
                  when Get_Status_JSON_Action =>
                     declare
                        Info : constant Device_Parameters_Data_Handles.
                                        Handle :=
                               Get_Parameters (Cube, Data.Address);
                        Parameters : Device_Parameters renames
                                     Info.Ptr.Parameters;
                     begin
                        case Parameters.Kind_Of is
                           when Radiator_Thermostat..Wall_Thermostat =>
                              Reply
                              (  Encode
                                 (  Data,
                                    Parameters.Serial_No,
                                    Parameters.Name,
                                    Parameters.Offset
                                 ),
                                 False
                              );
                           when others =>
                              Reply
                              (  Encode
                                 (  Data,
                                    Parameters.Serial_No,
                                    Parameters.Name,
                                    0.0
                                 ),
                                 False
                              );
                        end case;
                     end;
                  when Get_Temperature_Action =>
                     if Data.Kind_Of in Radiator_Thermostat
                                     .. Wall_Thermostat then
                        if Data.Temperature = Centigrade'First then
                           Reply ("no temperature", True);
                        else
                           Reply (Image (Data.Temperature), True);
                        end if;
                     else
                        Client.Reply_Text
                        (  400,
                           "Bad request",
                           (  "Device "
                           &  Image (Device)
                           &  " is not a thermostat"
                        )  );
                     end if;
                  when Get_Set_Temperature_Action =>
                     if Data.Kind_Of in Radiator_Thermostat
                                     .. Wall_Thermostat
                     then
                        Reply (Image (Data.Set_Temperature), True);
                     else
                        Client.Reply_Text
                        (  400,
                           "Bad request",
                           (  "Device "
                           &  Image (Device)
                           &  " is not a thermostat"
                        )  );
                     end if;
                  when Get_Battery_Action =>
                     if Data.Battery_Low then
                        Reply ("low", True);
                     else
                        Reply ("high", True);
                     end if;
                  when Get_Link_Action =>
                     if Data.Link_Error then
                        Reply ("error", True);
                     else
                        Reply ("ok", True);
                     end if;
                  when Get_Summer_Time_Action =>
                     if Data.DST then
                        Reply ("yes", True);
                     else
                        Reply ("no", True);
                     end if;
                  when Get_Thermostat_Mode_Action =>
                     if Data.Kind_Of in Radiator_Thermostat
                                     .. Wall_Thermostat
                     then
                        case Data.Mode is
                           when Automatic =>
                              Reply ("automatic", True);
                           when Manual =>
                              Reply ("manual", True);
                           when Vacation =>
                              Reply ("vacation", True);
                           when Boost =>
                              Reply ("boost", True);
                        end case;
                     else
                        Client.Reply_Text
                        (  400,
                           "Bad request",
                           (  "Device "
                           &  Image (Device)
                           &  " is not a thermostat"
                        )  );
                     end if;
                  when Get_Duty_Action       |
                       Get_Rooms_List_Action |
                       Get_Rooms_List_JSON_Action =>
                     Client.Reply_Text
                     (  400,
                        "Bad request",
                        "Unexpected device address " & Image (Device)
                     );
                  when Get_Valve_Action..Get_Valve_Min_Action =>
                     if Data.Kind_Of in Radiator_Thermostat
                                     .. Radiator_Thermostat_Plus
                     then
                        Reply
                        (  (  Image
                              (  Integer (Data.Valve_Position * 100.0)
                              )
                           &  '%'
                           ),
                           True
                        );
                     else
                        Client.Reply_Text
                        (  400,
                           "Bad request",
                           (  "Device "
                           &  Image (Device)
                           &  " is not a radiator thermostat"
                        )  );
                     end if;
               end case;
            end;
         exception
            when Error : End_Error =>
               raise Data_Error with Exception_Message (Error);
         end;
      end Do_Get_Status;

      procedure Do_None is
         File_Name : constant String :=
                              Settings.HTTP_Default_Page.Get_Text;
      begin
         if File_Name = "" then
            Client.Reply_Text (404, "Not found", "Nothing here");
            return;
         end if;
         Do_File (File_Name);
      end Do_None;

      procedure Do_Reboot (Query : String) is
         Pointer        : Integer := Query'First;
         Token          : Parameter;
         Cube           : RF_Address;
         Serial_Start   : Integer;
         Callback_Start : Integer := Query'Last + 1;
         Callback_Stop  : Integer;
         Got_Cube       : Boolean := False;

         procedure Reply (Message : String; Quote : Boolean) is
         begin
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            Send_Connection (Client, False);
            if Callback_Start > Query'Last then
               Send_Content_Type (Client, "text/plain");
               Send_Body (Client, Message);
            else
               Do_CORS (Client);
               Send_Content_Type (Client, "application/json");
               if Quote then
                  Send_Body
                  (  Client,
                     (  Query (Callback_Start..Callback_Stop)
                     &  "("""
                     &  Message
                     &  """);"
                  )  );
               else
                  Send_Body
                  (  Client,
                     (  Query (Callback_Start..Callback_Stop)
                     &  "("
                     &  Message
                     &  ");"
                  )  );
               end if;
            end if;
         end Reply;
      begin
         loop
            begin
               Parameter_Tables.Get
               (  Query,
                  Pointer,
                  Parameter_List,
                  Token
               );
            exception
               when End_Error =>
                  raise Data_Error with
                        "Unknown parameter starting at " &
                        Quote (Query (Pointer..Query'Last));
            end;
            case Token is
               when Callback_Parameter =>
                  Callback_Start := Pointer;
                  Callback_Stop  := Pointer - 1;
                  while Pointer <= Query'Last loop
                     exit when Query (Pointer) = '&';
                     Pointer := Pointer + 1;
                  end loop;
                  Callback_Stop := Pointer - 1;
               when Cube_Address_Parameter | Device_Address_Parameter =>
                  raise Data_Error with
                        "Unexpected address parameter";
               when Temperature_Parameter..Eco_Parameter =>
                  raise Data_Error with
                        "Unexpected temperature parameter";
               when Schedule_Parameter =>
                  raise Data_Error with
                        "Unexpected schedule parameter";
               when Weeks_Parameter |
                    Days_Parameter  |
                    Hours_Parameter |
                    Minutes_Parameter =>
                  raise Data_Error with "Unexpected time parameter";
               when Serial_Parameter =>
                  if Got_Cube then
                     raise Data_Error with
                           "Cube serial nuumber is specified twice";
                  end if;
                  Serial_Start := Pointer;
                  while Pointer <= Query'Last loop
                     case Query (Pointer) is
                        when 'A'..'Z' | 'a'..'z' | '0'..'9' =>
                           Pointer := Pointer + 1;
                        when others =>
                           exit;
                     end case;
                  end loop;
                  if Pointer <= Serial_Start then
                     raise Data_Error with "Missing serial nuumber";
                  elsif Pointer - Serial_Start /= 10 then
                     raise Data_Error with "Invalid serial nuumber";
                  end if;
                  Got_Cube := True;
            end case;
            exit when Pointer > Query'Last;
            Get_Ampersand (Query, Pointer);
         end loop;
         if not Got_Cube then
            raise Data_Error with "No cube serial number specified";
         end if;
         Reboot (Query (Serial_Start..Serial_Start + 9));
         Reply
         (  "rebooting " & Query (Serial_Start..Serial_Start + 9),
            True
         );
      end Do_Reboot;

      procedure Do_Set_Eco_Temperature (Query : String) is
         Pointer         : Integer := Query'First;
         Temperature     : Centigrade;
         Token           : Parameter;
         Cube            : RF_Address;
         Device          : RF_Address;
         Got_Cube        : Boolean := False;
         Got_Device      : Boolean := False;
         Got_Temperature : Boolean := False;
      begin
         loop
            begin
               Parameter_Tables.Get
               (  Query,
                  Pointer,
                  Parameter_List,
                  Token
               );
            exception
               when End_Error =>
                  raise Data_Error with
                        "Unknown parameter starting at " &
                         Quote (Query (Pointer..Query'Last));
            end;
            case Token is
               when Callback_Parameter =>
                  raise Data_Error with
                        "JSONP callback is not allowed when " &
                        "setting eco temperature";
               when Cube_Address_Parameter =>
                  if Got_Cube then
                     raise Data_Error with
                           "Cube RF-Address is specified twice";
                  else
                     Got_Cube := True;
                  end if;
                  Get_Address (Query, Pointer, Cube);
               when Device_Address_Parameter =>
                  if Got_Device then
                     raise Data_Error with
                           "Device RF-Address is specified twice";
                  else
                     Got_Device := True;
                  end if;
                  Get_Address (Query, Pointer, Device);
               when Schedule_Parameter =>
                  raise Data_Error with
                        "Schedule is not allowed when setting " &
                        "setting eco temperature";
               when Temperature_Parameter =>
                  if Got_Temperature then
                     raise Data_Error with
                           "Temperature is specified twice";
                  else
                     Got_Temperature := True;
                  end if;
                  declare
                     Value : Float;
                  begin
                     Strings_Edit.Floats.Get
                     (  Source  => Query,
                        Pointer => Pointer,
                        Value   => Value,
                        First   => 0.0,
                        Last    => 60.0
                     );
                     Temperature := Centigrade (Value);
                  exception
                     when End_Error =>
                        raise Data_Error with
                              "Missing temperature value";
                     when others =>
                        raise Data_Error with
                              "Invalid temperature value";
                  end;
               when Inc_Temperature_Parameter..Eco_Parameter =>
                  raise Data_Error with
                        "Eco temperature must be in absolute format";
               when Weeks_Parameter |
                    Days_Parameter  |
                    Hours_Parameter |
                    Minutes_Parameter =>
                  raise Data_Error with
                        "Time parameter is not allowed when " &
                        "setting eco temperature";
               when Serial_Parameter =>
                  raise Data_Error with
                        "Serial parameter is not allowed when " &
                        "setting eco temperature";
            end case;
            exit when Pointer > Query'Last;
            Get_Ampersand (Query, Pointer);
         end loop;
         if not Got_Cube then
            raise Data_Error with "No cube RF-Address specified";
         elsif not Got_Device then
            raise Data_Error with "No thermostat RF-Address specified";
         elsif not Got_Temperature then
            raise Data_Error with "No temperature specified";
         else
            declare
               None : aliased Boolean := False;
            begin
               if Get_Device_Data (Cube, Device, None'Access).Kind_Of
                  not in Radiator_Thermostat..Wall_Thermostat
               then
                  raise Data_Error with
                        "Device "      &
                        Image (Device) &
                        " is not a thermostat";
               end if;
            exception
               when Error : End_Error =>
                  raise Data_Error with Exception_Message (Error);
            end;
         end if;
         declare
            Parameters : Device_Parameters :=
                         Get_Parameters (Cube, Device).Ptr.Parameters;
         begin
            Parameters.Eco := Temperature;
            Store_Configuration
            (  Box        => Cube,
               Device     => Device,
               Parameters => Parameters,
               Action     => Store_Parameters
            );
         end;
         Client.Reply_Text
         (  200,
            "OK",
            (  "Setting eco temperature of "
            &  Image (Device)
            &  " on "
            &  Image (Cube)
            &  " to "
            &  Image (Temperature)
         )  );
      end Do_Set_Eco_Temperature;

      procedure Do_Set_Schedule (Query : String) is
         Pointer      : Integer := Query'First;
         Token        : Parameter;
         Cube         : RF_Address;
         Device       : RF_Address;
         Schedule     : Week_Schedule;
         Got_Cube     : Boolean := False;
         Got_Device   : Boolean := False;
         Got_Schedule : Boolean := False;
      begin
         loop
            begin
               Parameter_Tables.Get
               (  Query,
                  Pointer,
                  Parameter_List,
                  Token
               );
            exception
               when End_Error =>
                  raise Data_Error with
                        "Unknown parameter starting at " &
                         Quote (Query (Pointer..Query'Last));
            end;
            case Token is
               when Callback_Parameter =>
                  raise Data_Error with
                        "JSONP callback is not allowed when " &
                        "setting schedule mode";
               when Cube_Address_Parameter =>
                  if Got_Cube then
                     raise Data_Error with
                           "Cube RF-Address is specified twice";
                  else
                     Got_Cube := True;
                  end if;
                  Get_Address (Query, Pointer, Cube);
               when Device_Address_Parameter =>
                  if Got_Device then
                     raise Data_Error with
                           "Device RF-Address is specified twice";
                  else
                     Got_Device := True;
                  end if;
                  Get_Address (Query, Pointer, Device);
               when Schedule_Parameter =>
                  if Got_Schedule then
                     raise Data_Error with
                           "Device schedule is specified twice";
                  else
                     Got_Schedule := True;
                  end if;
                  MAX_IO.Get (Query, Pointer, Schedule);
               when Temperature_Parameter..Eco_Parameter =>
                  raise Data_Error with
                        "Temperature is not allowed when " &
                        "setting schedule mode";
               when Weeks_Parameter |
                    Days_Parameter |
                    Hours_Parameter |
                    Minutes_Parameter =>
                  raise Data_Error with
                        "Time parameter is not allowed when " &
                        "setting schedule mode";
               when Serial_Parameter =>
                  raise Data_Error with
                        "Serial parameter is not allowed when " &
                        "setting schedule mode";
            end case;
            exit when Pointer > Query'Last;
            Get_Ampersand (Query, Pointer);
         end loop;
         if not Got_Cube then
            raise Data_Error with "No cube RF-Address specified";
         elsif not Got_Device then
            raise Data_Error with "No thermostat RF-Address specified";
         elsif not Got_Schedule then
            raise Data_Error with "No schedule specified";
         else
            declare
               None : aliased Boolean := False;
            begin
               if Get_Device_Data (Cube, Device, None'Access).Kind_Of
                  not in Radiator_Thermostat..Wall_Thermostat
               then
                  raise Data_Error with
                        "Device " & Image (Device) &
                        " is not a thermostat";
               end if;
            exception
               when Error : End_Error =>
                  raise Data_Error with Exception_Message (Error);
            end;
         end if;
         Set_Thermostat_Schedule
         (  Box      => Cube,
            Device   => Device,
            Schedule => Schedule,
            Silent   => True
         );
         Client.Reply_Text
         (  200,
            "OK",
            (  "Setting "
            &  Image (Device)
            &  " on "
            &  Image (Cube)
            &  " to "
            &  Image (Schedule)
         )  );
      end Do_Set_Schedule;

      procedure Do_Set_Thermostat_Mode
                (  Query : String;
                   Mode  : Set_Mode_Action
                )  is
         Pointer         : Integer := Query'First;
         Token           : Parameter;
         Cube            : RF_Address;
         Device          : RF_Address;
         Temperature     : Centigrade;
         Disposition     : Temperature_Mode;
         Up_Until        : Time;
         Got_Cube        : Boolean := False;
         Got_Device      : Boolean := False;
         Got_Temperature : Boolean := False;
         Got_Duration    : Boolean := False;
      begin
         loop
            begin
               Parameter_Tables.Get
               (  Query,
                  Pointer,
                  Parameter_List,
                  Token
               );
            exception
               when End_Error =>
                  raise Data_Error with
                        "Unknown parameter starting at " &
                         Quote (Query (Pointer..Query'Last));
            end;
            case Token is
               when Callback_Parameter =>
                  raise Data_Error with
                        "JSONP callback is not allowed when " &
                        "setting thermostat mode";
               when Cube_Address_Parameter =>
                  if Got_Cube then
                     raise Data_Error with
                           "Cube RF-Address is specified twice";
                  else
                     Got_Cube := True;
                  end if;
                  Get_Address (Query, Pointer, Cube);
               when Device_Address_Parameter =>
                  if Got_Device then
                     raise Data_Error with
                           "Device RF-Address is specified twice";
                  else
                     Got_Device := True;
                  end if;
                  Get_Address (Query, Pointer, Device);
               when Schedule_Parameter =>
                  raise Data_Error with
                        "Unexpected schedule parameter";
               when Temperature_Parameter..Eco_Parameter =>
                  if Mode not in Set_Automatic_Action
                              .. Set_Vacation_Action
                  then
                     raise Data_Error with
                          "Unexpected temperature parameter";
                  end if;
                  if Got_Temperature then
                     raise Data_Error with
                           "Temperature is specified twice";
                  else
                     Got_Temperature := True;
                  end if;
                  case Token is
                     when Dec_Temperature_Parameter =>
                        Disposition := Decrement;
                     when Inc_Temperature_Parameter =>
                        Disposition := Increment;
                     when Airing_Parameter =>
                        Disposition := Airing;
                     when Comfort_Parameter =>
                        Disposition := Comfort;
                     when Eco_Parameter =>
                        Disposition := Eco;
                     when others =>
                        Disposition := Absolute;
                  end case;
                  if Disposition in Absolute..Decrement then
                     declare
                        Value : Float;
                     begin
                        Strings_Edit.Floats.Get
                        (  Source  => Query,
                           Pointer => Pointer,
                           Value   => Value,
                           First   => 0.0,
                           Last    => 60.0
                        );
                        Temperature := Centigrade (Value);
                     exception
                        when End_Error =>
                           raise Data_Error with
                                 "Missing temperature value";
                        when others =>
                           raise Data_Error with
                                 "Invalid temperature value";
                     end;
                  else
                     Temperature := 19.0;
                  end if;
               when Weeks_Parameter |
                    Days_Parameter  |
                    Hours_Parameter |
                    Minutes_Parameter =>
                  if Mode /= Set_Vacation_Action then
                     raise Data_Error with "Unexpected time parameter";
                  end if;
                  if Got_Duration then
                     raise Data_Error with
                           "Duration is specified twice";
                  else
                     Got_Duration := True;
                  end if;
                  declare
                     Value : Float;
                  begin
                     Strings_Edit.Floats.Get
                     (  Source  => Query,
                        Pointer => Pointer,
                        Value   => Value,
                        First   => 0.0,
                        Last    => 100_000.0
                     );
                     case Token is
                        when Weeks_Parameter =>
                           Value := Value * 60.0 * 60.0 * 24.0 * 7.0;
                        when Days_Parameter =>
                           Value := Value * 60.0 * 60.0 * 24.0;
                        when Hours_Parameter =>
                           Value := Value * 60.0 * 60.0;
                        when others =>
                           Value := Value * 60.0;
                     end case;
                     Up_Until := Clock + Duration (Value);
                  exception
                     when End_Error =>
                        raise Data_Error with
                              "Missing duration value";
                     when others =>
                        raise Data_Error with
                              "Invalid duration value";
                  end;
               when Serial_Parameter =>
                  raise Data_Error with
                        "Serial parameter is not allowed when " &
                        "setting thermostat mode";
            end case;
            exit when Pointer > Query'Last;
            Get_Ampersand (Query, Pointer);
         end loop;
         if not Got_Cube then
            raise Data_Error with "No cube RF-Address specified";
         elsif not Got_Device then
            Device := 0;
         else
            declare
               None : aliased Boolean := False;
            begin
               if Get_Device_Data (Cube, Device, None'Access).Kind_Of
                  not in Radiator_Thermostat..Wall_Thermostat
               then
                  raise Data_Error with
                        "Device "      &
                        Image (Device) &
                        " is not a thermostat";
               end if;
            exception
               when Error : End_Error =>
                  raise Data_Error with Exception_Message (Error);
            end;
         end if;
         case Mode is
            when Set_Automatic_Action =>
               if Got_Temperature then
                  Set_Thermostat_Mode
                  (  Box         => Cube,
                     Device      => Device,
                     Mode        => Automatic,
                     Temperature => Temperature,
                     Disposition => Disposition,
                     Silent      => True
                  );
               else
                  Set_Thermostat_Mode
                  (  Box         => Cube,
                     Device      => Device,
                     Mode        => Automatic,
                     Silent      => True
                  );
               end if;
            when Set_Manual_Action =>
               if not Got_Temperature then
                  raise Data_Error with "No temperature specified";
               end if;
               Set_Thermostat_Mode
               (  Box         => Cube,
                  Device      => Device,
                  Mode        => Manual,
                  Temperature => Temperature,
                  Disposition => Disposition,
                  Silent      => True
               );
            when Set_Vacation_Action =>
               if not Got_Temperature then
                  raise Data_Error with "No temperature specified";
               end if;
               if not Got_Duration then
                  raise Data_Error with
                        "No vacation end time specified";
               end if;
               Set_Thermostat_Mode
               (  Box         => Cube,
                  Device      => Device,
                  Mode        => Vacation,
                  Temperature => Temperature,
                  Disposition => Disposition,
                  Up_Until    => Up_Until,
                  Silent      => True
               );
            when Set_Boost_Action =>
               Set_Thermostat_Mode
               (  Box    => Cube,
                  Device => Device,
                  Mode   => Boost,
                  Silent => True
               );
         end case;
         if Device = 0 then
            Client.Reply_Text
            (  200,
               "OK",
               "Setting all thermostats on " & Image (Cube)
            );
         else
            Client.Reply_Text
            (  200,
               "OK",
               "Setting " & Image (Device) & " on " & Image (Cube)
            );
         end if;
      end Do_Set_Thermostat_Mode;
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      case Status.Kind is
         when None =>
            Do_None;
         when File =>
            if Status.File = "" then
               Do_None;
               return;
            end if;
            declare
               From    : Integer := Status.File'First;
               Command : Action;
               Offset  : Integer;
            begin
               while From <= Status.File'Last loop
                  exit when Status.File (From) /= '/' and then
                            Status.File (From) /= '\';
                  From := From + 1;
               end loop;
               declare
                  File : String renames
                         Status.File (From..Status.File'Last);
               begin
                  Offset := Actions_List.Locate (File);
                  if Offset > 0 then
                     Command := Actions_List.GetTag (Offset);
                     case Command is
                        when Cube_Mode_Action =>
                           Do_Connection (Status.Query, Command);
                        when Get_Cubes_List_Action =>
                           Do_Get_Cubes (Status.Query);
                        when Get_Cubes_List_JSON_Action =>
                           Do_Get_Cubes_JSON (Status.Query);
                        when Get_Mode_Action =>
                           Do_Get_Status (Status.Query, Command);
                        when Set_Mode_Action =>
                           Do_Set_Thermostat_Mode
                           (  Status.Query,
                              Command
                           );
                        when Set_Eco_Temperature_Action =>
                           Do_Set_Eco_Temperature (Status.Query);
                        when Set_Schedule_Action =>
                           Do_Set_Schedule (Status.Query);
                        when Reboot_Action =>
                           Do_Reboot (Status.Query);
                     end case;
                  else
                     declare
                        Page : constant String :=
                               Settings.HTTP_Default_Page.Get_Text;
                     begin
                        if Page = "" then
                           Client.Reply_Text
                           (  404,
                              "Not found",
                              "No file " & Quote (File) & " found"
                           );
                           return;
                        end if;
                        for Index in reverse Page'Range loop
                           case Page (Index) is
                              when '/' | '\' =>
                                 Do_File
                                 (  Page (Page'First..Index)
                                 &  File
                                 );
                                 return;
                              when others =>
                                  null;
                           end case;
                        end loop;
                     end;
                     Do_File (File);
                  end if;
               end;
            exception
               when Error : others =>
                  Client.Reply_Text
                  (  400,
                     "Bad request",
                     Exception_Message (Error)
                  );
            end;
         when URI =>
            Client.Reply_Text
            (  404,
               "Not found",
               "No URI " & Quote (Status.Path) & " found"
            );
      end case;
   end Do_Get_Head;

   procedure Do_Get (Client : in out MAX_HTTP_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Get;

   procedure Do_Head (Client : in out MAX_HTTP_Client) is
   begin
      Do_Get_Head (Client, False);
   end Do_Head;

   procedure Do_Options (Client : in out MAX_HTTP_Client) is
      Response : constant String := "";
   begin
      Client.Send_Status_Line (200, "OK");
      Client.Send_Date (Clock);
      Client.Send_Server;
      Client.Send_Length (Natural'(Response'Length));
      Client.Send_Connection (False);
      Client.Send_Allow (Client.Get_Allowed);
      Do_CORS (Client);
      Client.Send (CRLF);
      Client.Send (Response);
   end Do_Options;

   procedure Finalize (Client : in out MAX_HTTP_Client)  is
   begin
      Client.Close_Default;
      HTTP_WebSocket_Client (Client).Finalize;
   end Finalize;

   function Get_Server_Address
            (  Listener : MAX_HTTP_Server
            )  return Sock_Addr_Type is
      Result : Sock_Addr_Type;
   begin
      Result.Addr := Any_Inet_Addr;
      Result.Port := HTTP_Parameters.Port;
      declare
         Address : constant String := HTTP_Parameters.Address;
      begin
         if Address'Length /= 0 then
            Result.Addr := To_Addr (Address);
         end if;
      end;
      return Result;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (HTTP Get_Address_Request)")
         )  );
         return Result;
   end Get_Server_Address;

   function Get_Server_Address
            (  Listener : MAX_MQTT_Server
            )  return Sock_Addr_Type is
      Result : Sock_Addr_Type;
   begin
      Result.Addr := Any_Inet_Addr;
      Result.Port := MQTT_Parameters.Port;
      declare
         Address : constant String := MQTT_Parameters.Address;
      begin
         if Address'Length /= 0 then
            Result.Addr := To_Addr (Address);
         end if;
      end;
      return Result;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (MQTT Get_Address_Request)")
         )  );
         return Result;
   end Get_Server_Address;

   function Get_Topics_List
            (  Settings : not null access MAX_Settings_Record;
               Silent   : Boolean
            )  return Topic_Sets.Set is
      Result : Topic_Sets.Set;
   begin
      if not Settings.MQTT_Enable_Publishing.Get_Active then
         return Result;
      end if;
      declare
         List    : constant String :=
                            Settings.MQTT_Topics_List.Get_Text;
         Pointer : Integer := List'First;
         Start   : Integer;
      begin
         Get (List, Pointer);
         while Pointer <= List'Last loop
            Start := Pointer;
            while Pointer <= List'Last loop
               exit when List (Pointer) = ',';
               Pointer := Pointer + 1;
            end loop;
            declare
               Topic : constant String :=
                                Trim (List (Start..Pointer - 1));
            begin
               if Check_Topic (Topic) then
                  null;
               end if;
               Result.Add (Topic);
            exception
               when Error : Constraint_Error =>
                  if not Silent then
                     raise Constraint_Error with
                           "Invalid topic " &
                           Quote (Topic) &
                           ": " &
                           Exception_Message (Error);
                  end if;
            end;
            exit when Pointer > List'Last;
            Pointer := Pointer + 1;
            Get (List, Pointer);
         end loop;
      end;
      return Result;
   end Get_Topics_List;

   function Get_Value
            (  Edit    : Gtk_GEntry;
               Key     : String;
               Min     : Float;
               Max     : Float;
               Default : Float;
               Small   : Integer := -1
            )  return Float is
      Result  : Float;
      Changed : Boolean := False;
      Text    : constant String := Trim (Edit.Get_Text);
   begin
      if Text'Length = 0 then
         Result := Default;
      else
         begin
            Result := Value (Text);
            if Result < Min then
               Result  := Min;
               Changed := Min < 0.0;
            elsif Result > Max then
               Result  := Max;
               Changed := True;
            end if;
         exception
            when others =>
               Result  := Default;
               Changed := True;
         end;
      end if;
      if Changed then
         Edit.Set_Text (Image (Value => Result, AbsSmall => Small));
      end if;
      Store (Key, Image (Result, AbsSmall => Small));
      return Result;
   end Get_Value;

   function Get_Value
            (  Edit    : Gtk_GEntry;
               Key     : String;
               Min     : Integer;
               Max     : Integer;
               Default : Integer
            )  return Integer is
      Result  : Integer;
      Changed : Boolean := False;
      Text    : constant String := Trim (Edit.Get_Text);
   begin
      if Text'Length = 0 then
         Result := Default;
      else
         begin
            Result := Value (Text);
            if Result < Min then
               Result  := Min;
               Changed := Min < 0;
            elsif Result > Max then
               Result  := Max;
               Changed := True;
            end if;
         exception
            when others =>
               Result  := Default;
               Changed := True;
         end;
      end if;
      if Changed then
         Edit.Set_Text (Image (Result));
      end if;
      Store (Key, Image (Result));
      return Result;
   end Get_Value;

   function Gtk_Settings_New return MAX_Settings is
      Result : constant MAX_Settings := new MAX_Settings_Record;
      Label  : Gtk_Label;
      Unit   : Gtk_Label;
      Row    : Gint := 0;
   begin
      Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
      Gtk_New (Result.Grid);
      Result.Grid.Set_Row_Homogeneous (True);
      Result.Grid.Set_Column_Homogeneous (False);
      Result.Grid.Set_Border_Width (5);
      Result.Grid.Set_Row_Spacing (3);
      Result.Grid.Set_Column_Spacing (3);
      Result.Pack_Start (Result.Grid, Expand => False);
      -- Row 1 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Label, "Enable HTTP server");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 0, 0);
      Gtk_New (Result.Enable_HTTP);
      Result.Grid.Attach_Next_To (Result.Enable_HTTP, Label, Pos_Right);
      Result.Enable_HTTP.Set_Tooltip_Text
      (  "HTTP server allows querying the thermostats data and "
      &  "control their mode via HTTP client, e.g. a web browser"
      );
      Gtk_New (Unit);
      Result.Grid.Attach_Next_To (Unit, Result.Enable_HTTP, Pos_Right);
      ------------------------------------------------------------------
      Gtk_New (Label, "HTTP server port");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Unit, Pos_Right);
      Gtk_New (Result.HTTP_Port);
      Result.HTTP_Port.Set_Tooltip_Text
      (  "The port to be used by the HTTP server. "
      &  "When there is more than one HTTP server running on the "
      &  "computer you might wish to set an alternate port number "
      &  "for this one."
      );
      Result.Grid.Attach_Next_To (Result.HTTP_Port, Label, Pos_Right);
      Result.HTTP_Port.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.HTTP_Port, "max-width-chars") /= null
      then
         Set_Property
         (  Result.HTTP_Port,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      begin
         Result.HTTP_Port.Set_Text
         (  Image (Integer'(Restore ("http-port", 80)))
         );
      exception
         when others =>
            Result.HTTP_Port.Set_Text ("80");
      end;
      -- Row 2 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.HTTP_Trace);
      Result.Grid.Attach_Next_To
      (  Result.HTTP_Trace,
         Result.Enable_HTTP,
         Pos_Bottom
      );
      Gtk_New (Label, "HTTP trace");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.HTTP_Trace, Pos_Left);
      Result.HTTP_Trace.Set_Tooltip_Text
      (  "When turned on, the HTTP server's exchange is traced along "
      &  "with the exchange with the cube(s)"
      );
      Gtk_New (Unit);
      Result.Grid.Attach_Next_To (Unit, Result.HTTP_Trace, Pos_Right);
      ------------------------------------------------------------------
      Gtk_New (Label, "HTTP server address");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Unit, Pos_Right);
      Gtk_New (Result.HTTP_Address);
      Result.Grid.Attach_Next_To
      (  Result.HTTP_Address,
         Label,
         Pos_Right
      );
      Result.HTTP_Address.Set_Tooltip_Text
      (  "When the computer has multiple network adapters, "
      &  "default one's address is choosed for the HTTP server "
      &  "to listen to. "
      &  "You might wish to specify the address or name of another "
      &  "adapter here."
      );
      Result.HTTP_Address.Set_Text (Restore ("http-address", ""));
      Result.HTTP_Address.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.HTTP_Address, "max-width-chars") /= null
      then
         Set_Property
         (  Result.HTTP_Address,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      -- Row 3 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.HTTP_CORS_Origin);
      Result.Grid.Attach_Next_To
      (  Result.HTTP_CORS_Origin,
         Result.HTTP_Trace,
         Pos_Bottom
      );
      Result.HTTP_CORS_Origin.Set_Tooltip_Text
      (  "This value specifies origins involved in HTTP cross-origin "
      &  "sharing (CORS). The default is no sharing. The value can "
      &  "be a list of hostnames or * to indicate that resources of "
      &  "any origin are allowed"
      );
      Result.HTTP_CORS_Origin.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.HTTP_CORS_Origin,
               "max-width-chars"
            )
         /= null
         )
      then
         Set_Property
         (  Result.HTTP_CORS_Origin,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.HTTP_CORS_Origin.Set_Text (Restore ("cors-origin", ""));
      Gtk_New (Label, "Allowed CORS origins");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.HTTP_CORS_Origin,
         Pos_Left
      );
      ------------------------------------------------------------------
      Gtk_New (Result.HTTP_CORS_Methods);
      Result.Grid.Attach_Next_To
      (  Result.HTTP_CORS_Methods,
         Result.HTTP_Address,
         Pos_Bottom
      );
      Result.HTTP_CORS_Methods.Set_Tooltip_Text
      (  "This value specifies HTTP commands accepted in cross-origin "
      &  "sharing (CORS). The list is comma-separated"
      );
      Result.HTTP_CORS_Methods.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.HTTP_CORS_Methods,
               "max-width-chars"
            )
         /= null
         )
      then
         Set_Property
         (  Result.HTTP_CORS_Methods,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.HTTP_CORS_Methods.Set_Text
      (  Restore
         (  "cors-methods",
            "POST, GET, OPTIONS"
      )  );
      Gtk_New (Label, "Accepted CORS methods");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.HTTP_CORS_Methods,
         Pos_Left
      );
      -- Row 4 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.HTTP_CORS_Headers);
      Result.Grid.Attach_Next_To
      (  Result.HTTP_CORS_Headers,
         Result.HTTP_CORS_Origin,
         Pos_Bottom
      );
      Result.HTTP_CORS_Headers.Set_Tooltip_Text
      (  "The list of headers that clients may use when issuing "
      &  "HTTP requests in order to make use of the cross-origin "
      &  "sharing (CORS)"
      );
      Result.HTTP_CORS_Headers.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.HTTP_CORS_Headers,
               "max-width-chars"
            )
         /= null
         )
      then
         Set_Property
         (  Result.HTTP_CORS_Headers,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.HTTP_CORS_Headers.Set_Text
      (  Restore
         (  "cors-headers",
            "X-PINGOTHER, Content-Type"
      )  );
      Gtk_New (Label, "Accepted CORS headers");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.HTTP_CORS_Headers,
         Pos_Left
      );
      ------------------------------------------------------------------
      Gtk_New (Result.HTTP_CORS_Max_Age);
      Result.Grid.Attach_Next_To
      (  Result.HTTP_CORS_Max_Age,
         Result.HTTP_CORS_Methods,
         Pos_Bottom
      );
      Result.HTTP_CORS_Max_Age.Set_Tooltip_Text
      (  "How long the results of a preflight request "
      &  "can be cached, used for HTTP cross-origin "
      &  "sharing (CORS)"
      );
      Result.HTTP_CORS_Max_Age.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.HTTP_CORS_Max_Age,
               "max-width-chars"
            )
         /= null
         )
      then
         Set_Property
         (  Result.HTTP_CORS_Max_Age,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.HTTP_CORS_Max_Age.Set_Text
      (  Restore
         (  "cors-max-age",
            "86400"
      )  );
      Gtk_New (Label, "CORS cache age");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.HTTP_CORS_Max_Age,
         Pos_Left
      );
      Gtk_New (Unit, "s");
      Result.Grid.Attach_Next_To
      (  Unit,
         Result.HTTP_CORS_Max_Age,
         Pos_Right
      );
      Unit.Set_Halign (Align_Start);
      Unit.Set_Valign (Align_Center); --Unit.Set_Alignment (0.0, 0.5);
      ------------------------------------------------------------------
      Gtk_New (Label, "UI HTTP page");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach (Label, 3, Row);
      Gtk_New (Result.HTTP_Default_Page);
      Result.Grid.Attach (Result.HTTP_Default_Page, 4, Row);
      Result.HTTP_Default_Page.Set_Width_Chars (Edit_Width);
      if (  Find_Property (Result.HTTP_Default_Page, "max-width-chars")
         /= null
         )  then
         Set_Property
         (  Result.HTTP_Default_Page,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.HTTP_Default_Page.Set_Tooltip_Text
      (  "This file is used for the default page shown by "
      &  "the HTTP server. "
      &  "It can serve as a custom web-based user interface (UI)"
      );
      Result.HTTP_Default_Page.Set_Text
      (  Restore ("default-http-page", "")
      );
      Page_File_Buttons.Gtk_New (Result.HTTP_Browse_Button);
      Result.Grid.Attach (Result.HTTP_Browse_Button, 5, Row);
      Result.HTTP_Browse_Button.Set_Halign (Align_Start);
      Result.HTTP_Browse_Button.Set_Valign (Align_Center);
      -- Row 5 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.Poll);
      Result.Grid.Attach_Next_To
      (  Result.Poll,
         Result.HTTP_CORS_Headers,
         Pos_Bottom
      );
      Result.Poll.Set_Tooltip_Text
      (  "This value specifies how frequently the cubes must be "
      &  "queried to report the actual state of connected devices"
      );
      Result.Poll.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.Poll, "max-width-chars") /= null then
         Set_Property
         (  Result.Poll,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.Poll.Set_Text (Restore ("poll", "1.0"));
      Poll_Changed (Result, Result);
      Gtk_New (Label, "Polling period");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.Poll, Pos_Left);
      Gtk_New (Unit, "s");
      Result.Grid.Attach_Next_To (Unit, Result.Poll, Pos_Right);
      Unit.Set_Halign (Align_Start);
      Unit.Set_Valign (Align_Center); --Unit.Set_Alignment (0.0, 0.5);
      -- Row 6 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.Scan);
      Result.Scan.Set_Active (Scan_On);
      Result.Grid.Attach_Next_To
      (  Result.Scan,
         Result.Poll,
         Pos_Bottom
      );
      Gtk_New (Label, "Scan thermostats");
      Result.Scan.Set_Tooltip_Text
      (  "Radiator thermostats rarely send the measured temperature. "
      &  "This happens only when the valve position or mode is "
      &  "changed. "
      &  "When scanning is allowed, the program periodically "
      &  "toggles the radiator thermostats operating mode "
      &  "in order to force them reporting the temperature. "
      &  "Later the thermostats are switched back into the original "
      &  "mode. "
      &  "Note that this applies only to the radiator thermostats "
      &  "in the rooms with no wall-mounted thermostat. "
      &  "When a wall-mounted thermostat is present, "
      &  "all radiator thermostats "
      &  "in the room report the temperature measured by the "
      &  "wall-mounted thermostat instead."
      );
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.Scan, Pos_Left);
      Gtk_New (Unit);
      Result.Grid.Attach_Next_To (Unit, Result.Scan, Pos_Right);
      Unit.Set_Halign (Align_Start);
      Unit.Set_Valign (Align_Center);
      ------------------------------------------------------------------
      Gtk_New (Label, "Scan period");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Unit, Pos_Right);
      Gtk_New (Result.Scan_Period);
      Result.Grid.Attach_Next_To (Result.Scan_Period, Label, Pos_Right);
      Result.Scan_Period.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.Scan_Period, "max-width-chars") /= null
      then
         Set_Property
         (  Result.Scan_Period,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.Scan_Period.Set_Text
      (  Image (Float (Scan_Period) / 60.0, AbsSmall => -2)
      );
      Result.Scan_Period.Set_Tooltip_Text
      (  "This value specifies how frequently radiator thermostats "
      &  "must be scanned"
      );
      Gtk_New (Unit, "min");
      Result.Grid.Attach_Next_To (Unit, Result.Scan_Period, Pos_Right);
      Unit.Set_Halign (Align_Start);
      Unit.Set_Valign (Align_Center); --Unit.Set_Alignment (0.0, 0.5);
      -- Row 7 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.Scan_Timeout);
      Result.Grid.Attach_Next_To
      (  Result.Scan_Timeout,
         Result.Scan_Period,
         Pos_Bottom
      );
      Result.Scan_Timeout.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.Scan_Timeout, "max-width-chars") /= null
      then
         Set_Property
         (  Result.Scan_Timeout,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.Scan_Timeout.Set_Text
      (  Image (Float (Scan_Timeout) / 60.0, AbsSmall => -2)
      );
      Result.Scan_Timeout.Set_Tooltip_Text
      (  "This value controls when a radiator thermostat is switched "
      &  "back to the original mode. "
      &  "This happens when either "
      &  "this timeout expires or "
      &  "the measured temperature is reported or"
      &  "a half of scanning period is spent."
      );
      Gtk_New (Label, "Scan timeout");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.Scan_Timeout, Pos_Left);
      Gtk_New (Unit, "min");
      Result.Grid.Attach_Next_To (Unit, Result.Scan_Timeout, Pos_Right);
      Unit.Set_Halign (Align_Start);
      Unit.Set_Valign (Align_Center); --Unit.Set_Alignment (0.0, 0.5);
      ------------------------------------------------------------------
      Gtk_New (Unit, "min");
      Result.Grid.Attach_Next_To (Unit, Label, Pos_Left);
      Unit.Set_Halign (Align_Start);
      Unit.Set_Valign (Align_Center); --Unit.Set_Alignment (0.0, 0.5);
      Gtk_New (Result.Temperature_Timeout);
      Result.Grid.Attach_Next_To
      (  Result.Temperature_Timeout,
         Unit,
         Pos_Left
      );
      Result.Temperature_Timeout.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.Temperature_Timeout,
               "max-width-chars"
            )
         /= null
         )
      then
         Set_Property
         (  Result.Temperature_Timeout,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.Temperature_Timeout.Set_Text
      (  Image (Float (Temperature_Timeout) / 60.0, AbsSmall => -2)
      );
      Result.Temperature_Timeout.Set_Tooltip_Text
      (  "This value specifies the timeout after which the "
      &  "measured temperature reported by a radiator thermostat "
      &  "is considered outdated"
      );
      Gtk_New (Label, "Temperature timeout");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.Temperature_Timeout,
         Pos_Left
      );
      -- Row 8 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.Graph_Add_Offset);
      Result.Grid.Attach (Result.Graph_Add_Offset, 1, Row);
      Result.Graph_Add_Offset.Set_Tooltip_Text
      (  "Add the thermostat offset temperature to the set "
      &  "temperature when shown in as the curve."
      );
      Gtk_New (Label, "Use offset in graphs");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.Graph_Add_Offset,
         Pos_Left
      );
      ------------------------------------------------------------------
      Gtk_New (Label, "Graph width");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);
      Gtk_New (Result.Graph_Width);
      Result.Grid.Attach_Next_To (Result.Graph_Width, Label, Pos_Right);
      Result.Graph_Width.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.Graph_Width, "max-width-chars") /= null
      then
         Set_Property
         (  Result.Graph_Width,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.Graph_Width.Set_Text
      (  Image (Float (Graph_Width) / 60.0, AbsSmall => -2)
      );
      Result.Graph_Width.Set_Tooltip_Text
      (  "This value specifies default time interval shown "
      &  "by the temperature graphs"
      );
      -- Row 9 ---------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.Graph_Fixed_Scale);
      Result.Grid.Attach (Result.Graph_Fixed_Scale, 1, Row);
      Result.Graph_Fixed_Scale.Set_Tooltip_Text
      (  "Use fixed temperature scale. Otherwise the temperature "
      &  "scale is automatially adjusted."
      );
      Gtk_New (Label, "Fixed temperature scale");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.Graph_Fixed_Scale,
         Pos_Left
      );
      ------------------------------------------------------------------
      Gtk_New (Label, "Temperature low");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);
      Gtk_New (Result.Graph_Low_Temperature);
      Result.Grid.Attach_Next_To
      (  Result.Graph_Low_Temperature,
         Label,
         Pos_Right
      );
      Result.Graph_Low_Temperature.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.Graph_Low_Temperature,
               "max-width-chars"
            )
         /= null
         )  then
         Set_Property
         (  Result.Graph_Low_Temperature,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.Graph_Low_Temperature.Set_Text
      (  Image (Float (Graph_Width) / 60.0, AbsSmall => -2)
      );
      Result.Graph_Low_Temperature.Set_Tooltip_Text
      (  "The low temperature scale value"
      );

      Gtk_New (Label, Degree & "C");
      Label.Set_Halign (Align_Start);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.Graph_Low_Temperature,
         Pos_Right
      );
      -- Row 10 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.ELV_Trace);
      Result.Grid.Attach (Result.ELV_Trace, 1, Row);
      Result.ELV_Trace.Set_Tooltip_Text
      (  "Trace MAX! cube protocol exchange"
      );
      Gtk_New (Label, "MAX! cube trace");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.ELV_Trace,
         Pos_Left
      );
      Result.ELV_Trace.Set_Active (ELV_Trace);
      ------------------------------------------------------------------
      Gtk_New (Label, "Temperature high");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);
      Gtk_New (Result.Graph_High_Temperature);
      Result.Grid.Attach_Next_To
      (  Result.Graph_High_Temperature,
         Label,
         Pos_Right
      );
      Result.Graph_High_Temperature.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.Graph_High_Temperature,
               "max-width-chars"
            )
         /= null
         )  then
         Set_Property
         (  Result.Graph_High_Temperature,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Result.Graph_High_Temperature.Set_Text
      (  Image (Float (Graph_Width) / 60.0, AbsSmall => -2)
      );
      Result.Graph_High_Temperature.Set_Tooltip_Text
      (  "The high temperature scale value"
      );

      Gtk_New (Label, Degree & "C");
      Label.Set_Halign (Align_Start);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.Graph_High_Temperature,
         Pos_Right
      );
      -- Row 11 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.Decoded_Trace);
      Result.Grid.Attach (Result.Decoded_Trace, 1, Row);
      Result.Decoded_Trace.Set_Tooltip_Text
      (  "Trace decoded device data and status responses "
      &  "from the cube. "
      );
      Gtk_New (Label, "Decoded trace");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.Decoded_Trace,
         Pos_Left
      );
      ------------------------------------------------------------------
      Gtk_New (Result.DB_Trace);
      Result.Grid.Attach (Result.DB_Trace, 4, Row);
      Result.DB_Trace.Set_Tooltip_Text
      (  "Trace database operations. "
      );
      Gtk_New (Label, "DB trace");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.DB_Trace,
         Pos_Left
      );
      -- Row 12 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.Enable_MQTT);
      Result.Grid.Attach (Result.Enable_MQTT, 1, Row);
      Result.Enable_MQTT.Set_Tooltip_Text
      (  "MQTT server allows subscribing to the thermostats data and "
      &  "control their mode via MQTT protocol"
      );
      Gtk_New (Label, "Enable MQTT server");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.Enable_MQTT, Pos_Left);
      Gtk_New (Unit);
      Result.Grid.Attach_Next_To (Unit, Result.Enable_MQTT, Pos_Right);
      ------------------------------------------------------------------
      Gtk_New (Label, "MQTT server port");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Unit, Pos_Right);
      Gtk_New (Result.MQTT_Port);
      Result.MQTT_Port.Set_Tooltip_Text
      (  "The port to be used by the MQTT server. "
      &  "When there is more than one MQTT server running on the "
      &  "computer you might wish to set an alternate port number "
      &  "for this one."
      );
      Result.Grid.Attach_Next_To (Result.MQTT_Port, Label, Pos_Right);
      Result.MQTT_Port.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.HTTP_Port, "max-width-chars") /= null
      then
         Set_Property
         (  Result.MQTT_Port,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      begin
         Result.MQTT_Port.Set_Text
         (  Image
            (  Integer'
               (  Restore
                  (  "mqtt-port",
                     GNAT.Sockets.MQTT.MQTT_Port
         )  )  )  );
      exception
         when others =>
            Result.HTTP_Port.Set_Text
            (  Image
               (  Integer (GNAT.Sockets.MQTT.MQTT_Port)
            )  );
      end;
      -- Row 13 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Result.MQTT_Trace);
      Result.Grid.Attach (Result.MQTT_Trace, 1, Row);
      Gtk_New (Label, "MQTT trace");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.MQTT_Trace, Pos_Left);
      Result.MQTT_Trace.Set_Tooltip_Text
      (  "When turned on, the MQTT server's exchange is traced along "
      &  "with the exchange with the cube(s). Note that this has no "
      &  "effect on MQTT over WebSockets. The later are traced when "
      &  "HTTP tracing is enabled"
      );
      Gtk_New (Unit);
      Result.Grid.Attach_Next_To (Unit, Result.MQTT_Trace, Pos_Right);
      ------------------------------------------------------------------
      Gtk_New (Label, "MQTT server address");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Unit, Pos_Right);
      Gtk_New (Result.MQTT_Address);
      Result.Grid.Attach_Next_To
      (  Result.MQTT_Address,
         Label,
         Pos_Right
      );
      Result.MQTT_Address.Set_Tooltip_Text
      (  "When the computer has multiple network adapters, "
      &  "The default one's address is choosed for the MQTT server "
      &  "to listen to. "
      &  "You might wish to specify the address or name of another "
      &  "adapter here."
      );
      Result.MQTT_Address.Set_Text (Restore ("mqtt-address", ""));
      Result.MQTT_Address.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.MQTT_Address, "max-width-chars") /= null
      then
         Set_Property
         (  Result.MQTT_Address,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      -- Row 14 -------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Label, "MQTT publishing");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 0, Row);
      Gtk_New (Result.MQTT_Enable_Publishing);
      Result.Grid.Attach_Next_To
      (  Result.MQTT_Enable_Publishing,
         Label,
         Pos_Right
      );
      Result.MQTT_Enable_Publishing.Set_Tooltip_Text
      (  "When not checked the MQTT clients are not allowed to publish "
      &  "any topics except ones defined by MAX! home automation. "
      &  "When checked the MQTT clients can publish topics matching "
      &  "patterns in the list of the right."
      );
      ------------------------------------------------------------------
      Gtk_New (Label, "Allowed topics");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);
      Gtk_New (Result.MQTT_Topics_List);
      Result.Grid.Attach (Result.MQTT_Topics_List, 4, Row);
      Result.MQTT_Topics_List.Set_Tooltip_Text
      (  "When empty all topcis are allowed. "
      &  "Otherwise it is a list of comma separated list of topics "
      &  "allowed for publishing. "
      &  "List items are MQTT topic patterns. "
      &  "a topic is allowed when matched by any list item."
      );
      -- Row 15 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Label, "MQTT max connections");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 0, Row);
      Gtk_New (Result.MQTT_Max_Connections);
      Result.Grid.Attach_Next_To
      (  Result.MQTT_Max_Connections,
         Label,
         Pos_Right
      );
      Result.MQTT_Max_Connections.Set_Tooltip_Text
      (  "The maximum number of MQTT clients allowed to connect "
      &  "to the MQTT server."
      );
      Result.MQTT_Max_Connections.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.MQTT_Max_Connections,
               "max-width-chars"
            )
         /= null
         )
      then
         Set_Property
         (  Result.MQTT_Max_Connections,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      ------------------------------------------------------------------
      Gtk_New (Label, "MQTT max subscriptions");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);
      Gtk_New (Result.MQTT_Max_Subscriptions);
      Result.Grid.Attach_Next_To
      (  Result.MQTT_Max_Subscriptions,
         Label,
         Pos_Right
      );
      Result.MQTT_Max_Subscriptions.Set_Tooltip_Text
      (  "The maximum number of MQTT topics a client is allowed "
      &  "to subscribe or unsubscribe in a single request. "
      &  "Note that this is not a limitation on the overal number of "
      &  "subscriptions, but on bulk subscription requests. "
      &  "Clients that subscribe topics one by one are not affected. "
      &  "When changed this setting has effect on newly connected "
      &  "clients only."
      );
      Result.MQTT_Max_Subscriptions.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.MQTT_Max_Subscriptions,
               "max-width-chars"
            )
         /= null
         )
      then
         Set_Property
         (  Result.MQTT_Max_Subscriptions,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      -- Row 16 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Label, "MQTT messages queue");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 0, Row);
      Gtk_New (Result.MQTT_Max_Messages);
      Result.Grid.Attach_Next_To
      (  Result.MQTT_Max_Messages,
         Label,
         Pos_Right
      );
      Result.MQTT_Max_Messages.Set_Tooltip_Text
      (  "The size of the queue of accumulated messages kept "
      &  "between sessions of a client. "
      &  "The messages are sent to the client upon connecting."
      );
      Result.MQTT_Max_Messages.Set_Width_Chars (Edit_Width);
      if (  Find_Property
            (  Result.MQTT_Max_Messages,
               "max-width-chars"
            )
         /= null
         )
      then
         Set_Property
         (  Result.MQTT_Max_Messages,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      ------------------------------------------------------------------
      Gtk_New (Label, "MQTT periodic publishing");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);
      Gtk_New (Result.MQTT_Push);
      Result.Grid.Attach_Next_To
      (  Result.MQTT_Push,
         Label,
         Pos_Right
      );
      Result.MQTT_Push.Set_Tooltip_Text
      (  "Normally the MQTT server publishes messages reflecting the "
      &  "status of a device only if it changes. Check this box to "
      &  "publish messages regardless the change. In effect time the "
      &  " cube is polled the status messages are re-published."
      );
      -- Row 17 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Label, "Enable LAN discovery");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 0, Row);
      Gtk_New (Result.Enable_Discovery);
      Result.Grid.Attach_Next_To
      (  Result.Enable_Discovery,
         Label,
         Pos_Right
      );
      Result.Enable_Discovery.Set_Tooltip_Text
      (  "A MAX! cube can be found if located in the same LAN segment. "
      &  "If enabled the application will try to find all such cubes. "
      &  "Changing this setting will have effect after next "
      &  "start."
      );
      Gtk_New (Label, "Host address");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);

      Gtk_New (Result.Host_Address);
      Result.Grid.Attach (Result.Host_Address, 4, Row);
      Result.Host_Address.Set_Tooltip_Text
      (  "When the computer has multiple network adapters, "
      &  "all of them are tried for cube discovery "
      &  "if this field is empty. "
      &  "You might wish to specify a concrete address or name of "
      &  "the adapter (and thus its LAN segment) to use. "
      &  "Changing the address will have effect by next start."
      );
      Result.Host_Address.Set_Text (To_String (Host));
      Result.Host_Address.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.Host_Address, "max-width-chars") /= null
      then
         Set_Property
         (  Result.Host_Address,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      -- Row 18 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Label, "Cube address");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);

      Gtk_New (Result.Cube_Address);
      Result.Grid.Attach (Result.Cube_Address, 4, Row);
      Result.Cube_Address.Set_Tooltip_Text
      (  "The cube address or name used to connect to when "
      &  "starting. "
      &  "You can leave this field is empty to connect to "
      &  "the cube manually after start. "
      &  "Changing the address will have effect by next start. "
      &  "You can specify several cubes here by separating "
      &  "their addresses by spaces, commas or semicolons. "
      &  CRLF & CRLF
      &  "To make this field sensitive uncheck "
      &  "'Enable LAN discovery' box."
      );
      Result.Cube_Address.Set_Text (To_String (Cube_Address));
      Result.Cube_Address.Set_Width_Chars (Edit_Width);
      if Find_Property (Result.Cube_Address, "max-width-chars") /= null
      then
         Set_Property
         (  Result.Cube_Address,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      ------------------------------------------------------------------
      Gtk_New (Result.Enable_Web_MQTT);
      Result.Grid.Attach_Next_To
      (  Result.Enable_Web_MQTT,
         Result.Enable_Discovery,
         Pos_Bottom
      );
      Result.Enable_Web_MQTT.Set_Tooltip_Text
      (  "Enable WebSockets MQTT server. If enabled the HTTP server "
      &  "will accept connections from WebSockets MQTT clients"
      );
      Gtk_New (Label, "Enable MQTT on WebSockets");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To
      (  Label,
         Result.Enable_Web_MQTT,
         Pos_Left
      );
      -- Row 19 --------------------------------------------------------
      Row := Row + 1;
      Gtk_New (Label, "Enable GNUTLS trace");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 0, Row);
      declare
         Box : Gtk_HBox;
      begin
         Gtk_New_HBox (Box, False, 3);
         Result.Grid.Attach (Box, 1, Row);
         Gtk_New (Result.Enable_GNUTLS_Trace);
         Box.Pack_Start (Result.Enable_GNUTLS_Trace, False, False);
         Result.Enable_GNUTLS_Trace.Set_Tooltip_Text
         (  "Enable GNUTLS internal tracing. "
         &  "If enabled the SSL/TLS exchange is traced into "
         &  "the specified file."
         );
         Gtk_New (Label, "Level");
         Box.Pack_Start (Label, False, False);
         Gtk_New (Result.GNUTLS_Trace_Level);
         Box.Pack_Start (Result.GNUTLS_Trace_Level, False, False);
         Result.GNUTLS_Trace_Level.Set_Tooltip_Text ("Debug level");
         Result.GNUTLS_Trace_Level.Set_Width_Chars (2);
      end;
      Gtk_New (Label, "GNUTLS trace file");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach (Label, 3, Row);

      Gtk_New (Result.GNUTLS_Trace_File);
      Result.Grid.Attach (Result.GNUTLS_Trace_File, 4, Row);
      Result.GNUTLS_Trace_File.Set_Tooltip_Text
      (  "The file GNUTLS will use for tracing."
      );
      Result.GNUTLS_Trace_File.Set_Text (To_String (Host));
      Result.GNUTLS_Trace_File.Set_Width_Chars (Edit_Width);
      if (  Find_Property (Result.GNUTLS_Trace_File, "max-width-chars")
         /= null
         )
      then
         Set_Property
         (  Result.GNUTLS_Trace_File,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      Trace_File_Buttons.Gtk_New (Result.GNUTLS_Browse_File);
      Result.Grid.Attach (Result.GNUTLS_Browse_File, 5, Row);
      ------------------------------------------------------------------
      Gtk_New (Label, "Home page: " & Home);
--        Label.Set_Markup
--        (  "<a href=""" & Home & """ title=""home page"">"
--        &  "MAX! Home Automation website</a>"
--        );
      Label.Set_Vexpand (False);
      Label.Set_Selectable (True);
      Result.Pack_Start (Label);
      declare
         Box : Gtk_HBox;
      begin
         Gtk_New_HBox (Box, False, 3);
         Result.Pack_End (Box, False, False);

         Gtk_New (Label, "Export CSS");
         Label.Set_Halign (Align_End);
         Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
         Box.Pack_Start (Label);

         Properties_Buttons.Gtk_New (Result.Export_CSS);
         Box.Pack_Start (Result.Export_CSS, False, False);

         Gtk_New (Label, "Export settings");
         Label.Set_Halign (Align_End);
         Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
         Box.Pack_Start (Label, False, False);

         Export_Buttons.Gtk_New (Result.Export_Settings);
         Box.Pack_Start (Result.Export_Settings, False, False);
      end;

      Result.MQTT_Max_Connections.Set_Text
      (  Restore ("mqtt-max-connections", "100")
      );
      Result.MQTT_Max_Messages.Set_Text
      (  Restore ("mqtt-max-messages", "20000")
      );
      On_MQTT_Max_Connnections_Changed (Result, Result);
      Result.MQTT_Max_Subscriptions.Set_Text
      (  Restore ("mqtt-max-subscriptions", "20")
      );
      On_MQTT_Max_Subscriptions_Changed (Result, Result);

      if Restore ("mqtt-enable-publishing", "off") = "on" then
         -- Enabling the MQTT publishing
         Result.MQTT_Enable_Publishing.Set_Active (True);
         Result.MQTT_Topics_List.Set_Sensitive    (False);
      end if;
      Result.MQTT_Topics_List.Set_Text
      (  Restore
         (  "mqtt-topics-list",
            ""
      )  );
      if Restore ("graph-add-offset", "on") = "on" then
         Result.Graph_Add_Offset.Set_Active (True);
         Graph_Add_Offset := True;
      else
         Graph_Add_Offset := False;
      end if;
      Result.Graph_Low_Temperature.Set_Text
      (  Restore ("graph-low-temperature-scale", "")
      );
      Result.Graph_High_Temperature.Set_Text
      (  Restore ("graph-high-temperature-scale", "")
      );
      begin
         Temperature_Low :=
            Centigrade
            (  Float'Value
               (  Result.Graph_Low_Temperature.Get_Text
            )  );
         Temperature_High :=
            Centigrade
            (  Float'Value
               (  Result.Graph_High_Temperature.Get_Text
               )  );
         if not (  Temperature_Low  in Temperature_Scale
                and then
                   Temperature_High in Temperature_Scale
                )
         then
            Temperature_Low  := Temperature_Scale'First;
            Temperature_High := Temperature_Scale'First;
         end if;
      exception
         when others =>
            Temperature_Low  := Temperature_Scale'First;
            Temperature_High := Temperature_Scale'First;
      end;
      if (  Restore ("graph-fixed-temperature-scale", "off") = "on"
         and then
            Temperature_Low < Temperature_High
         )  then
         Result.Graph_Fixed_Scale.Set_Active (True);
         Result.Graph_Low_Temperature.Set_Sensitive (False);
         Result.Graph_High_Temperature.Set_Sensitive (False);
         Graph_Fixed_Scale := True;
      else
         Result.Graph_Fixed_Scale.Set_Active (False);
         Result.Graph_Low_Temperature.Set_Sensitive (True);
         Result.Graph_High_Temperature.Set_Sensitive (True);
         Graph_Fixed_Scale := False;
      end if;
      if Restore ("db-trace", "off") = "on" then
         Result.DB_Trace.Set_Active (True);
         DB_Trace := True;
      end if;
      if Restore ("decoded-trace", "off") = "on" then
         Result.Decoded_Trace.Set_Active (True);
         Decoded_Trace := True;
      end if;
      if Restore ("mqtt-push", "off") = "on" then
         MQTT_Device_Policy := Retained;
         Result.MQTT_Push.Set_Active (True);
      else
         MQTT_Device_Policy := Updated;
         Result.MQTT_Push.Set_Active (False);
      end if;
      if Restore ("mqtt-webserver", "off") = "on" then
         -- Enabling the MQTT WebSockets server
         Result.Enable_Web_MQTT.Set_Active (True);
      end if;
      if Restore ("http-server", "off") = "on" then
         -- Starting the HTTP server if that was requested
         Result.Enable_Web_MQTT.Set_Sensitive    (False);
         Result.Enable_HTTP.Set_Active           (True);
         Result.HTTP_Port.Set_Sensitive          (False);
         Result.HTTP_Address.Set_Sensitive       (False);
         Result.HTTP_CORS_Origin.Set_Sensitive   (False);
         Result.HTTP_CORS_Methods.Set_Sensitive  (False);
         Result.HTTP_CORS_Headers.Set_Sensitive  (False);
         Result.HTTP_CORS_Max_Age.Set_Sensitive  (False);
         Result.HTTP_Default_Page.Set_Sensitive  (False);
         Result.HTTP_Browse_Button.Set_Sensitive (False);
         Start_HTTP (Result);
      end if;
      if Restore ("http-trace", "off") = "on" then
         Result.HTTP_Trace.Set_Active (True);
         Trace_On
         (  Factory  => HTTP_Factory,
            Received => GNAT.Sockets.Server.Trace_Decoded,
            Sent     => GNAT.Sockets.Server.Trace_Decoded
         );
      end if;
      if Restore ("mqtt-server", "off") = "on" then
         -- Starting the MQTT server if that was requested
         Result.Enable_MQTT.Set_Active               (True);
         Result.MQTT_Port.Set_Sensitive              (False);
         Result.MQTT_Address.Set_Sensitive           (False);
         Result.MQTT_Max_Messages.Set_Sensitive      (False);
         Start_MQTT
         (  Result,
            Result.MQTT_Enable_Publishing.Get_Active,
            Result.Get_Topics_List (True)
         );
      end if;
      if Restore ("mqtt-trace", "off") = "on" then
         Result.MQTT_Trace.Set_Active (True);
         Trace_On
         (  Factory  => MQTT_Factory,
            Received => GNAT.Sockets.Server.Trace_Decoded,
            Sent     => GNAT.Sockets.Server.Trace_Decoded
         );
         MQTT_State.Set_Tracing_Flags
         (  GNAT.Sockets.MQTT.Server.Trace_All
         );
      end if;
      Scan_On := Restore ("scanning", "off") = "on";
      Result.Scan.Set_Active (Scan_On);
      Result.Scan_Period.Set_Sensitive         (Scan_On);
      Result.Scan_Timeout.Set_Sensitive        (Scan_On);
      Result.Temperature_Timeout.Set_Sensitive (Scan_On);

      Result.Enable_Discovery.Set_Active (Discovery_On);
      Result.Host_Address.Set_Sensitive (Discovery_On);
      Result.Cube_Address.Set_Sensitive (not Discovery_On);

      if Restore ("gnutls-trace", "off") = "on" then
         Result.Enable_GNUTLS_Trace.Set_Active   (True);
         Result.GNUTLS_Trace_Level.Set_Sensitive (False);
         Result.GNUTLS_Trace_File.Set_Sensitive  (False);
         Result.GNUTLS_Browse_File.Set_Sensitive (False);
      else
         Result.Enable_GNUTLS_Trace.Set_Active (False);
      end if;
      Result.GNUTLS_Trace_File.Set_Text
      (  Restore ("gnutls-trace-file", "")
      );
      Result.GNUTLS_Trace_Level.Set_Text
      (  Restore ("gnutls-trace-level", "5")
      );

      Connect
      (  Result.Enable_GNUTLS_Trace,
         "clicked",
         On_GNUTLS_Trace_Enabled'Access,
         Result
      );
      Connect
      (  Result.ELV_Trace,
         "clicked",
         On_ELV_Trace_Enabled'Access,
         Result
      );
      Connect
      (  Result.Export_CSS,
         "clicked",
         On_Export_CSS'Access,
         Result
      );
      Connect
      (  Result.Export_Settings,
         "clicked",
         On_Export_Settings'Access,
         Result
      );
      Connect
      (  Result.HTTP_Browse_Button,
         "clicked",
         On_HTTP_Browse'Access,
         Result
      );
      Connect
      (  Result.GNUTLS_Browse_File,
         "clicked",
         On_GNUTLS_Browse'Access,
         Result
      );
      Connect
      (  Result.Graph_Add_Offset,
         "clicked",
         On_Graph_Add_Offset'Access,
         Result
      );
      Connect
      (  Result.Graph_Fixed_Scale,
         "clicked",
         On_Graph_Fixed_Scale'Access,
         Result
      );
      Connect
      (  Result.DB_Trace,
         "clicked",
         On_DB_Trace_Enabled'Access,
         Result
      );
      Connect
      (  Result.Decoded_Trace,
         "clicked",
         On_Decoded_Trace_Enabled'Access,
         Result
      );
      Connect
      (  Result.Enable_Discovery,
         "clicked",
         On_Discovery_Enabled'Access,
         Result
      );
      Connect
      (  Result.Enable_HTTP,
         "clicked",
         On_HTTP_Enabled'Access,
         Result
      );
      Connect
      (  Result.Enable_MQTT,
         "clicked",
         On_MQTT_Enabled'Access,
         Result
      );
      Connect
      (  Result.MQTT_Enable_Publishing,
         "clicked",
         On_MQTT_Publishing_Enabled'Access,
         Result
      );
      Connect
      (  Result.Enable_Web_MQTT,
         "clicked",
         On_Web_MQTT_Enabled'Access,
         Result
      );
      Connect
      (  Result.MQTT_Push,
         "clicked",
         On_MQTT_Push'Access,
         Result
      );
      Connect
      (  Result.HTTP_Trace,
         "clicked",
         On_HTTP_Trace'Access,
         Result
      );
      Connect
      (  Result.MQTT_Max_Subscriptions,
         "changed",
         On_MQTT_Max_Subscriptions_Changed'Access,
         Result
      );
      Connect
      (  Result.MQTT_Max_Connections,
         "changed",
         On_MQTT_Max_Connnections_Changed'Access,
         Result
      );
      Connect
      (  Result.MQTT_Trace,
         "clicked",
         On_MQTT_Trace'Access,
         Result
      );
      Connect
      (  Result.Poll,
         "changed",
         Poll_Changed'Access,
         Result
      );
      Connect
      (  Result.Graph_Width,
         "changed",
         Graph_Width_Changed'Access,
         Result
      );
      Connect
      (  Result.Scan,
         "clicked",
         On_Scan'Access,
         Result
      );
      Connect
      (  Result.Scan_Period,
         "changed",
         Scan_Period_Changed'Access,
         Result
      );
      Connect
      (  Result.Scan_Timeout,
         "changed",
         Scan_Timeout_Changed'Access,
         Result
      );
      Connect
      (  Result.Temperature_Timeout,
         "changed",
         Temperature_Timeout_Changed'Access,
         Result
      );
      Connect
      (  Result.Host_Address,
         "changed",
         On_Host_Changed'Access,
         Result
      );
      Connect
      (  Result.Cube_Address,
         "changed",
         On_Cube_Changed'Access,
         Result
      );
      Settings := Result;
      return Result;
   end Gtk_Settings_New;

   procedure Graph_Width_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      No_IO : IO_Blocker;
      Width : Float;
   begin
      if No_IO.Is_Free then
         Width := Value (Settings.Graph_Width.Get_Text);
         if Width < 0.01 then
            Width := 0.01;
            Settings.Graph_Width.Set_Text ("0.01");
         elsif Width > 100_000.0 then
            Width := 100_000.0;
            Settings.Graph_Width.Set_Text ("100000");
         end if;
         Store ("graph-width", Image (Width));
      end if;
   exception
      when others =>
         Settings.Graph_Width.Set_Text ("60");
   end Graph_Width_Changed;

   procedure Initialize (Client : in out MAX_HTTP_Client) is
   begin
      Initialize (HTTP_Client (Client));
   end Initialize;

   procedure On_Cube_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Cube_Address :=
         To_Unbounded_String (Settings.Cube_Address.Get_Text);
      Store ("cube", Settings.Cube_Address.Get_Text);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Cube_Changed")
         )  );
   end On_Cube_Changed;

   procedure On_DB_Trace_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      DB_Trace := Settings.DB_Trace.Get_Active;
      if DB_Trace then
         Store ("db-trace", "on");
      else
         Store ("db-trace", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_DB_Trace_Enabled")
         )  );
   end On_DB_Trace_Enabled;

   procedure On_Decoded_Trace_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Decoded_Trace := Settings.Decoded_Trace.Get_Active;
      if Decoded_Trace then
         Store ("decoded-trace", "on");
      else
         Store ("decoded-trace", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Decoded_Trace_Enabled")
         )  );
   end On_Decoded_Trace_Enabled;

   procedure On_Discovery_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Discovery_On := Settings.Enable_Discovery.Get_Active;
      if Discovery_On then
         Store ("discovery", "on");
      else
         Store ("discovery", "off");
      end if;
      Settings.Host_Address.Set_Sensitive (Discovery_On);
      Settings.Cube_Address.Set_Sensitive (not Discovery_On);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Discovery_Enabled")
         )  );
   end On_Discovery_Enabled;

   procedure On_ELV_Trace_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      ELV_Trace := Settings.ELV_Trace.Get_Active;
      if ELV_Trace then
         Store ("elv-trace", "on");
      else
         Store ("elv-trace", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_ELV_Trace_Enabled")
         )  );
   end On_ELV_Trace_Enabled;

   procedure On_Export_CSS_File_Select
             (  File_Name : String;
                Widget    : in out Gtk_Widget_Record'Class
             )  is
      use Ada.Directories;
      use Ada.Strings.Maps.Constants;
      use Ada.Text_IO;
      use Gtk.Recent_Manager_Alt;
   begin
      declare
         File : File_Type;
         List : Gtk_Recent_Info_Array renames Get_Items (Get_Default);
         Name : constant String := Get_Application_Name;
      begin
         Create (File, Out_File, File_Name);
         begin
            Put_Line
            (  File,
               (  "/*---------- MAX! Home Automation CSS "
               &  Image (Clock)
               &  " -----------*/"
            )  );
            Put_CSS_Styles (File, Pages);
            Close (File);
         exception
            when Error : others =>
               Say
               (  "Failed writing file "
               &  Quote (File_Name)
               &  ": "
               &  Exception_Message (Error)
               );
               Close (File);
               Delete_File (File_Name);
         end;
         Free (List);
      exception
         when others =>
            Free (List);
            raise;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Export_CSS_File_Select")
         )  );
   end On_Export_CSS_File_Select;

   procedure On_Export_CSS
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      Dialog :=
         Gtk_New
         (  "Select a file to export CSS",
            Action_Save,
            On_Export_CSS_File_Select'Access,
            Widget
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (CSS_Filter_Name);
      Filter.Add_Pattern ("*" & CSS_Extension);
      Dialog.Add_Filter (Filter);
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name ("Any file");
      Filter.Add_Pattern ("*");
      Dialog.Add_Filter (Filter);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Export_CSS")
         )  );
   end On_Export_CSS;

   procedure On_Export_File_Select
             (  File_Name : String;
                Widget    : in out Gtk_Widget_Record'Class
             )  is
      use Ada.Directories;
      use Ada.Strings.Maps.Constants;
      use Ada.Text_IO;
      use Gtk.Recent_Manager_Alt;
   begin
      declare
         File : File_Type;
         List : Gtk_Recent_Info_Array renames Get_Items (Get_Default);
         Name : constant String := Get_Application_Name;
      begin
         Create (File, Out_File, File_Name);
         begin
            Put_Line
            (  File,
               (  "----------------------------- Created "
               &  Image (Clock)
               &  " "
               &  " -----------------------------"
            )  );
            Put_Line
            (  File,
               (  "-- The file contains settings that are different "
               &  "from the defaults."
            )  );
            for Index in List'Range loop
               if Has_Application (List (Index), Name) then
                  declare
                     Key : constant String := Get_URI (List (Index));
                  begin
                     if not Is_Prefix ("file:", Key, Lower_Case_Map)
                     then
                        Put_Line
                        (  File,
                           (  Key
                           &  '='
                           &  Quote (Get_Display_Name (List (Index)))
                        )  );
                     end if;
                  end;
               end if;
            end loop;
            Close (File);
         exception
            when Error : others =>
               Say
               (  "Failed writing file "
               &  Quote (File_Name)
               &  ": "
               &  Exception_Message (Error)
               );
               Close (File);
               Delete_File (File_Name);
         end;
         Free (List);
      exception
         when others =>
            Free (List);
            raise;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Export_File_Select")
         )  );
   end On_Export_File_Select;

   procedure On_Export_Settings
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      Dialog :=
         Gtk_New
         (  "Select a file to export application settings",
            Action_Save,
            On_Export_File_Select'Access,
            Widget
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (Txt_Filter_Name);
      Filter.Add_Pattern ("*" & Txt_Extension);
      Dialog.Add_Filter (Filter);
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name ("Any file");
      Filter.Add_Pattern ("*");
      Dialog.Add_Filter (Filter);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Export_Settings")
         )  );
   end On_Export_Settings;

   procedure On_HTTP_Browse
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      Dialog :=
         Gtk_New
         (  "Select a HTTP page",
            Action_Open,
            Settings.HTTP_Default_Page
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (HTML_Filter_Name);
      Filter.Add_Pattern ("*.htm");
      Filter.Add_Pattern ("*" & HTML_Filter_Name);
      Dialog.Add_Filter (Filter);
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name ("Any file");
      Filter.Add_Pattern ("*");
      Dialog.Add_Filter (Filter);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_HTTP_Browse")
         )  );
   end On_HTTP_Browse;

   procedure On_GNUTLS_Browse
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      Dialog :=
         Gtk_New
         (  "Select a file for GNUTLS trace",
            Action_Save,
            Settings.GNUTLS_Trace_File
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (Log_Filter_Name);
      Filter.Add_Pattern ("*" & Log_Extension);
      Dialog.Add_Filter (Filter);
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name ("Any file");
      Filter.Add_Pattern ("*");
      Dialog.Add_Filter (Filter);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_GNUTLS_Browse")
         )  );
   end On_GNUTLS_Browse;

   procedure On_GNUTLS_Trace_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      use GNUTLS;
      Level : Integer := 5;
   begin
      if Settings.Enable_GNUTLS_Trace.Get_Active then
         Store ("gnutls-trace", "on");
         begin
            Level := Value (Settings.GNUTLS_Trace_Level.Get_Text);
            Level := Integer'Min (99, Integer'Max (0, Level));
         exception
            when others =>
               Level := 5;
         end;
         Set_TLS_Debug
         (  Interfaces.C.int (Level),
            Settings.GNUTLS_Trace_File.Get_Text
         );
         Store
         (  "gnutls-trace-file",
            Settings.GNUTLS_Trace_File.Get_Text
         );
         Store ("gnutls-trace-level", Image (Level));
         Settings.GNUTLS_Trace_File.Set_Sensitive  (False);
         Settings.GNUTLS_Trace_File.Set_Sensitive  (False);
         Settings.GNUTLS_Browse_File.Set_Sensitive (False);
      else
         Store ("gnutls-trace", "off");
         Set_TLS_Debug (0);
         Settings.GNUTLS_Trace_File.Set_Sensitive  (True);
         Settings.GNUTLS_Trace_File.Set_Sensitive  (True);
         Settings.GNUTLS_Browse_File.Set_Sensitive (True);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_GNUTLS_Trace_Enabled")
         )  );
   end On_GNUTLS_Trace_Enabled;

   procedure On_Graph_Add_Offset
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Graph_Add_Offset := Settings.Graph_Add_Offset.Get_Active;
      if Graph_Add_Offset then
         Store ("graph-add-offset", "on");
      else
         Store ("graph-add-offset", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Graph_Add_Offset")
         )  );
   end On_Graph_Add_Offset;

   procedure On_Graph_Fixed_Scale
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Graph_Fixed_Scale := Settings.Graph_Fixed_Scale.Get_Active;
      if Graph_Fixed_Scale then
         declare
            Value : Float;
         begin
            Value := Strings_Edit.Floats.Value
                     (  Settings.Graph_Low_Temperature.Get_Text
                     );
            if Value < Float (Temperature_Scale'First) then
               Graph_Fixed_Scale := False;
               Settings.Graph_Fixed_Scale.Set_Active (False);
               Say
               (  "The lower bound of the temperature scale "
               &  "must be greater than or equal to "
               &  Image (Temperature_Scale'First)
               );
               return;
            elsif Value > Float (Temperature_Scale'Last) then
               Graph_Fixed_Scale := False;
               Settings.Graph_Fixed_Scale.Set_Active (False);
               Say
               (  "The lower bound of the temperature scale "
               &  "must be less that or equal to "
               &  Image (Temperature_Scale'Last)
               );
               return;
            end if;
            Temperature_Low := Centigrade (Value);
         exception
            when others =>
               Graph_Fixed_Scale := False;
               Settings.Graph_Fixed_Scale.Set_Active (False);
               Say
               (  "The lower bound of the temperature scale "
               &  "must be a number in range "
               &  Image (Temperature_Scale'First)
               &  ".."
               &  Image (Temperature_Scale'Last)
               );
               return;
         end;
         declare
            Value : Float;
         begin
            Value := Strings_Edit.Floats.Value
                     (  Settings.Graph_High_Temperature.Get_Text
                     );
            if Value > Float (Temperature_Scale'Last) then
               Graph_Fixed_Scale := False;
               Settings.Graph_Fixed_Scale.Set_Active (False);
               Say
               (  "The upper bound of the temperature scale "
               &  "must be less that or equal to "
               &  Image (Temperature_Scale'Last)
               );
               return;
            elsif (  Value <= Float (Temperature_Low)
                  or else
                     Centigrade (Value) = Temperature_Low
                  )  then
               Graph_Fixed_Scale := False;
               Settings.Graph_Fixed_Scale.Set_Active (False);
               Say
               (  "The upper bound of the temperature scale "
               &  "must be greater than the lower bound by at least 0.5"
               );
               return;
            end if;
            Temperature_High := Centigrade (Value);
         exception
            when others =>
               Graph_Fixed_Scale := False;
               Settings.Graph_Fixed_Scale.Set_Active (False);
               Say
               (  "The upper bound of the temperature scale "
               &  "must be a number in range "
               &  Image (Temperature_Scale'First)
               &  ".."
               &  Image (Temperature_Scale'Last)
               );
               return;
         end;
         Store ("graph-fixed-temperature-scale", "on");
         Store
         (  "graph-low-temperature-scale",
            Image (Temperature_Low)
         );
         Store
         (  "graph-high-temperature-scale",
            Image (Temperature_High)
         );
         Settings.Graph_Low_Temperature.Set_Sensitive (False);
         Settings.Graph_High_Temperature.Set_Sensitive (False);
         MAX_IO.Set_Fixed_Scale (Temperature_Low, Temperature_High);
      else
         Store ("graph-fixed-temperature-scale", "off");
         Settings.Graph_Low_Temperature.Set_Sensitive (True);
         Settings.Graph_High_Temperature.Set_Sensitive (True);
         MAX_IO.Set_Auto_Scale;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Graph_Fixed_Scale")
         )  );
   end On_Graph_Fixed_Scale;

   procedure On_Host_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Host := To_Unbounded_String (Settings.Host_Address.Get_Text);
      Store ("host", Settings.Host_Address.Get_Text);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Host_Changed")
         )  );
   end On_Host_Changed;

   procedure On_HTTP_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      if Settings.Enable_HTTP.Get_Active then
         Store ("http-server", "on");
         Settings.Enable_Web_MQTT.Set_Sensitive    (False);
         Settings.HTTP_Port.Set_Sensitive          (False);
         Settings.HTTP_Address.Set_Sensitive       (False);
         Settings.HTTP_CORS_Origin.Set_Sensitive   (False);
         Settings.HTTP_CORS_Methods.Set_Sensitive  (False);
         Settings.HTTP_CORS_Headers.Set_Sensitive  (False);
         Settings.HTTP_CORS_Max_Age.Set_Sensitive  (False);
         Settings.HTTP_Default_Page.Set_Sensitive  (False);
         Settings.HTTP_Browse_Button.Set_Sensitive (False);
         Start_HTTP (Settings);
      else
         Store ("http-server", "off");
         Settings.Enable_Web_MQTT.Set_Sensitive    (True);
         Settings.HTTP_Port.Set_Sensitive          (True);
         Settings.HTTP_Address.Set_Sensitive       (True);
         Settings.HTTP_CORS_Origin.Set_Sensitive   (True);
         Settings.HTTP_CORS_Methods.Set_Sensitive  (True);
         Settings.HTTP_CORS_Headers.Set_Sensitive  (True);
         Settings.HTTP_CORS_Max_Age.Set_Sensitive  (True);
         Settings.HTTP_Default_Page.Set_Sensitive  (True);
         Settings.HTTP_Browse_Button.Set_Sensitive (True);
         Stop_HTTP;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_HTTP_Enabled")
         )  );
   end On_HTTP_Enabled;

   procedure On_HTTP_Trace
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      if Settings.HTTP_Trace.Get_Active then
         Trace_On
         (  Factory  => HTTP_Factory,
            Received => GNAT.Sockets.Server.Trace_Decoded,
            Sent     => GNAT.Sockets.Server.Trace_Decoded
         );
         Store ("http-trace", "on");
      else
         Trace_Off (Factory => HTTP_Factory);
         Store ("http-trace", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_HTTP_Trace")
         )  );
   end On_HTTP_Trace;

   procedure On_MQTT_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      if Settings.Enable_MQTT.Get_Active then
         begin
            declare
               Topics_List : constant Topic_Sets.Set :=
                                      Settings.Get_Topics_List (False);
            begin
               Store ("mqtt-server", "on");
               Settings.MQTT_Port.Set_Sensitive         (False);
               Settings.MQTT_Address.Set_Sensitive      (False);
               Settings.MQTT_Max_Messages.Set_Sensitive (False);
               Start_MQTT
               (  Settings,
                  Settings.MQTT_Enable_Publishing.Get_Active,
                  Topics_List
               );
            end;
         exception
            when Error : others =>
               Settings.Enable_MQTT.Set_Active (False);
               Say (Exception_Message (Error));
         end;
      else
         Store ("mqtt-server", "off");
         Settings.MQTT_Port.Set_Sensitive              (True);
         Settings.MQTT_Address.Set_Sensitive           (True);
         Settings.MQTT_Max_Messages.Set_Sensitive      (True);
         Settings.MQTT_Enable_Publishing.Set_Sensitive (True);
         Settings.MQTT_Topics_List.Set_Sensitive       (True);
         Stop_MQTT;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_MQTT_Enabled")
         )  );
   end On_MQTT_Enabled;

   procedure On_MQTT_Publishing_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      State : MAX_Server_State'Class renames
              MAX_Server_State'Class (MQTT_State.all);
   begin
      if Settings.MQTT_Enable_Publishing.Get_Active then
         begin
            declare
               Topics_List : constant Topic_Sets.Set :=
                                      Settings.Get_Topics_List (False);
            begin
               State.Enable_Publishing (Topics_List);
            end;
         exception
            when Error : others =>
               Say (Exception_Message (Error));
               Settings.MQTT_Enable_Publishing.Set_Active (False);
               Settings.MQTT_Topics_List.Set_Sensitive (True);
               return;
         end;
         Settings.MQTT_Topics_List.Set_Sensitive (False);
         Store ("mqtt-enable-publishing", "on");
         Store
         (  "mqtt-topics-list",
            Settings.MQTT_Topics_List.Get_Text
         );
      else
         State.Disable_Publishing;
         Settings.MQTT_Topics_List.Set_Sensitive (True);
         Store ("mqtt-enable-publishing", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_MQTT_Publishing_Enabled")
         )  );
   end On_MQTT_Publishing_Enabled;

   procedure On_MQTT_Push
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      if Settings.MQTT_Push.Get_Active then
         Store ("mqtt-push", "on");
         MQTT_Device_Policy := Retained;
      else
         Store ("mqtt-push", "off");
         MQTT_Device_Policy := Updated;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_MQTT_Push")
         )  );
   end On_MQTT_Push;

   procedure On_MQTT_Max_Connnections_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      Max : constant Integer :=
                     Get_Value
                     (  Edit    => Settings.MQTT_Max_Connections,
                        Min     => 1,
                        Max     => 100_000,
                        Default => 1,
                        Key     => "mqtt-max-connections"
                     );
   begin
      MQTT_Factory.Max_Connections := Max;
      MQTT_WebSockets_Factory.Max_Connections := Max;
   end On_MQTT_Max_Connnections_Changed;

   procedure On_MQTT_Max_Subscriptions_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      Max : constant Integer :=
                     Get_Value
                     (  Edit    => Settings.MQTT_Max_Subscriptions,
                        Min     => 1,
                        Max     => 1_000_000,
                        Default => 1,
                        Key     => "mqtt-max-subscriptions"
                     );
   begin
      MQTT_Factory.Max_Subscriptions := Max;
      MQTT_WebSockets_Factory.Max_Subscriptions := Max;
      Store ("mqtt-max-subscriptions", Image (Max));
   end On_MQTT_Max_Subscriptions_Changed;

   procedure On_MQTT_Trace
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      use GNAT.Sockets.MQTT.Server;
   begin
      if Settings.MQTT_Trace.Get_Active then
         Trace_On
         (  Factory  => MQTT_Factory,
            Received => GNAT.Sockets.Server.Trace_Decoded,
            Sent     => GNAT.Sockets.Server.Trace_Decoded
         );
         MQTT_State.Set_Tracing_Flags (Trace_All);
         Store ("mqtt-trace", "on");
      else
         Trace_Off (Factory => MQTT_Factory);
         MQTT_State.Set_Tracing_Flags (0);
         Store ("mqtt-trace", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_MQTT_Trace")
         )  );
   end On_MQTT_Trace;

   procedure On_Web_MQTT_Enabled
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      if Settings.Enable_MQTT.Get_Active then
         Store ("mqtt-webserver", "on");
      else
         Store ("mqtt-webserver", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Web_MQTT_Enabled")
         )  );
   end On_Web_MQTT_Enabled;

   procedure On_Scan
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      if Settings.Scan.Get_Active then
         Scan_On := True;
         Store ("scanning", "on");
      else
         Scan_On := False;
         Store ("scanning", "off");
      end if;
      Settings.Scan_Period.Set_Sensitive         (Scan_On);
      Settings.Scan_Timeout.Set_Sensitive        (Scan_On);
      Settings.Temperature_Timeout.Set_Sensitive (Scan_On);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Scan")
         )  );
   end On_Scan;

   procedure Poll_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
      No_IO : IO_Blocker;
   begin
      if No_IO.Is_Free then
         MAX_IO.Set_Poll_Time
         (  Duration
            (  Get_Value
               (  Edit    => Settings.Poll,
                  Key     => "poll",
                  Min     => 0.5,
                  Max     => 1_000.0,
                  Default => 1.0
         )  )  );
      end if;
   end Poll_Changed;

   procedure Scan_Period_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Scan_Period :=
         Duration
         (  Get_Value
            (  Edit    => Settings.Scan_Period,
               Key     => "scan-period",
               Min     => 0.5,
               Max     => 1_000.0,
               Default => 5.0
            )
         *  60.0
         );
   end Scan_Period_Changed;

   procedure Scan_Timeout_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Scan_Timeout :=
         Duration
         (  Get_Value
            (  Edit    => Settings.Scan_Timeout,
               Key     => "scan-timeout",
               Min     => 0.5,
               Max     => 1_000.0,
               Default => 5.0
            )
         *  60.0
         );
   end Scan_Timeout_Changed;

   procedure Start_HTTP
             (  Settings : not null access MAX_Settings_Record
             )  is
      Port : Integer;
   begin
      begin
         Port := Value
                 (  Source => Settings.HTTP_Port.Get_Text,
                    First  => Integer (GNAT.Sockets.Port_Type'First),
                    Last   => Integer (GNAT.Sockets.Port_Type'Last)
                 );
      exception
         when others =>
            Settings.HTTP_Port.Set_Text
            (  Image (Integer (HTTP_Parameters.Port))
            );
      end;
      declare
          Address : constant String := Settings.HTTP_Address.Get_Text;
      begin
         HTTP_Parameters.Set (Address, GNAT.Sockets.Port_Type (Port));
         if View /= null then
            View.Trace
            (  "Starting HTTP server listening ... ["
            &  Address
            &  ":"
            &  Image (Port)
            &  "]"
            );
         end if;
         Store ("http-address", Address);
         Store ("http-port",    Image (Port));
         Store ("cors-origin",  Settings.HTTP_CORS_Origin.Get_Text);
         Store ("cors-methods", Settings.HTTP_CORS_Methods.Get_Text);
         Store ("cors-headers", Settings.HTTP_CORS_Headers.Get_Text);
         Store ("cors-max-age", Settings.HTTP_CORS_Max_Age.Get_Text);
         Store
         (  "default-http-page",
            Settings.HTTP_Default_Page.Get_Text
         );
      end;
      HTTP_Server := new MAX_HTTP_Server (HTTP_Factory'Access, 0);
   end Start_HTTP;

   procedure Start_MQTT
             (  Settings    : not null access MAX_Settings_Record;
                Enable_List : Boolean;
                Topics_List : Topic_Sets.Set
             )  is
      State      : MAX_Server_State'Class renames
                   MAX_Server_State'Class (MQTT_State.all);
      Port       : Integer;
      Queue_Size : Integer;
   begin
      if Enable_List then
         State.Enable_Publishing (Topics_List);
      else
         State.Disable_Publishing;
      end if;
      begin
         Port := Value
                 (  Source => Settings.MQTT_Port.Get_Text,
                    First  => Integer (GNAT.Sockets.Port_Type'First),
                    Last   => Integer (GNAT.Sockets.Port_Type'Last)
                 );
      exception
         when others =>
            Port := Integer (MQTT_Parameters.Port);
            Settings.MQTT_Port.Set_Text (Image (Port));
      end;
      begin
         Queue_Size := Value
            (  Source => Settings.MQTT_Max_Messages.Get_Text,
               First  => Integer (100),
               Last   => Integer (1024*1024*10)
            );
      exception
         when others =>
            Queue_Size := 20000;
            Settings.MQTT_Max_Messages.Set_Text (Image (Queue_Size));
      end;
      declare
          Address : constant String := Settings.MQTT_Address.Get_Text;
      begin
         MQTT_Parameters.Set (Address, GNAT.Sockets.Port_Type (Port));
         if View /= null then
            View.Trace
            (  "Starting MQTT server listening ... ["
            &  Address
            &  ":"
            &  Image (Port)
            &  "]"
            );
         end if;
         Store ("mqtt-address", Address);
         Store ("mqtt-port", Image (Port));
         Store ("mqtt-max-messages", Image (Queue_Size));
      end;
      MQTT_State.Set_Queue_Size (Queue_Size);
      MQTT_Listener := new MAX_MQTT_Server (MQTT_Factory'Access, 0);
   end Start_MQTT;

   procedure Stop_HTTP is
   begin
      if HTTP_Server /= null then
         if View /= null then
            View.Trace ("Stopping HTTP server...");
         end if;
         Free (HTTP_Server);
      end if;
   end Stop_HTTP;

   procedure Stop_MQTT is
   begin
      if MQTT_Listener /= null then
         if View /= null then
            View.Trace ("Stopping MQTT server...");
         end if;
         Free (MQTT_Listener);
      end if;
   end Stop_MQTT;

   procedure Temperature_Timeout_Changed
             (  Widget   : access Gtk_Widget_Record'Class;
                Settings : MAX_Settings
             )  is
   begin
      Temperature_Timeout :=
         Duration
         (  Get_Value
            (  Edit    => Settings.Temperature_Timeout,
               Key     => "temperature-timeout",
               Min     => 0.5,
               Max     => 1_000.0,
               Default => 5.0
            )
         *  60.0
         );
   end Temperature_Timeout_Changed;

   protected body HTTP_Parameters is
      function Address return String is
      begin
         return To_String (HTTP_Address);
      end Address;

      function Port return GNAT.Sockets.Port_Type is
      begin
         return HTTP_Port;
      end Port;

      procedure Set (Address : String; Port : GNAT.Sockets.Port_Type) is
      begin
         HTTP_Address := To_Unbounded_String (Address);
         HTTP_Port    := Port;
      end Set;
   end HTTP_Parameters;

   protected body MQTT_Parameters is
      function Address return String is
      begin
         return To_String (MQTT_Address);
      end Address;

      function Port return GNAT.Sockets.Port_Type is
      begin
         return MQTT_Port;
      end Port;

      procedure Set (Address : String; Port : GNAT.Sockets.Port_Type) is
      begin
         MQTT_Address := To_Unbounded_String (Address);
         MQTT_Port    := Port;
      end Set;
   end MQTT_Parameters;

   function Replace_Settings (File_Name : String)
      return Messages_Queues.Doubly_Linked.List is
      use Ada.Text_IO;
      use Ada.Strings.Maps.Constants;
      use Gtk.Recent_Manager_Alt;
      use Messages_Queues.Doubly_Linked;

      Result : List;

      procedure Trace (Mode : Trace_Type; Message : String) is
      begin
         Append
         (  Result,
            new Trace_Request'(Mode, To_Unbounded_String (Message))
         );
      end Trace;

      procedure Import (Line : String) is
      begin
         for Index in Line'Range loop
            if Line (Index) = '=' then
               declare
                  Pointer : aliased Integer := Index + 1;
               begin
                  Store
                  (  Line (Line'First..Index - 1),
                     Get_Quoted (Line, Pointer'Access)
                  );
               exception
                  when Error : others =>
                     Trace
                     (  Error_Text,
                        (  "Failed to replace settings token "
                        &  Line (Line'First..Index - 1)
                        &  ": "
                        &  Exception_Message (Error)
                     )  );
               end;
            end if;
         end loop;
      end Import;

      use Strings_Edit;
      Line : String (1..1048 * 4);
      Last : Integer;
      File : Ada.Text_IO.File_Type;
   begin
      Trace
      (  Message_Text,
         "Replacing settings with ones from " & Quote (File_Name)
      );
      Open (File, In_File, File_Name);
      begin
         declare
            Items : constant Gtk_Recent_Info_Array :=
                             Get_Items (Get_Default);
            Name  : constant String := Get_Application_Name;
         begin
            for Index in Items'Range loop
               if Has_Application (Items (Index), Name) then
                  declare
                     Key   : constant String := Get_URI (Items (Index));
                     Error : GError;
                  begin
                     if not Is_Prefix ("file:", Key, Lower_Case_Map)
                     then
                        Remove_Item (Get_Default, Key, Error);
                        if Error /= null then
                           Error_Free (Error);
                        end if;
                     end if;
                  end;
               end if;
            end loop;
            Free (Items);
         exception
            when others =>
               Free (Items);
               raise;
         end;
         loop
            Get_Line (File, Line, Last);
            if Last < Line'Last then
               declare
                  Pointer : Integer := Line'First;
               begin
                  Get (Line, Pointer);
                  if not Is_Prefix ("--", Line (Pointer..Last)) then
                     Import (Line (Pointer..Last));
                  end if;
               end;
            end if;
         end loop;
      exception
         when Ada.Text_IO.End_Error =>
            Close (File);
         when others =>
            Close (File);
            raise;
      end;
      return Result;
   exception
      when Error : others =>
         Trace
         (  Error_Text,
            (  "Failed to replace settings with ones from "
            &  Quote (File_Name)
            &  ": "
            &  Exception_Information (Error)
         )  );
         return Result;
   end Replace_Settings;

   procedure Update_Host is
   begin
      if Settings /= null then
         Settings.Host_Address.Set_Text (To_String (Host));
      end if;
   end Update_Host;

   procedure Write
             (  Stream : not null access Ada.Streams.
                                         Root_Stream_Type'Class;
                File   : Page_File
             )  is
   begin
      null;
   end Write;

begin
   Actions_List.Add ("disconnect",          Disconnect_Action);
   Actions_List.Add ("get-battery",         Get_Battery_Action);
   Actions_List.Add ("get-connection",      Get_Connection_Action);
   Actions_List.Add ("get-cubes-list",      Get_Cubes_List_Action);
   Actions_List.Add ("get-cubes-json-list", Get_Cubes_List_JSON_Action);
   Actions_List.Add ("get-duty",            Get_Duty_Action);
   Actions_List.Add ("get-link",            Get_Link_Action);
   Actions_List.Add ("get-mode",            Get_Thermostat_Mode_Action);
   Actions_List.Add ("get-rooms-list",      Get_Rooms_List_Action);
   Actions_List.Add ("get-rooms-json-list", Get_Rooms_List_JSON_Action);
   Actions_List.Add ("get-set-temperature", Get_Set_Temperature_Action);
   Actions_List.Add ("get-status",          Get_Status_Action);
   Actions_List.Add ("get-status-csv",      Get_Status_CSV_Action);
   Actions_List.Add ("get-status-json",     Get_Status_JSON_Action);
   Actions_List.Add ("get-summer-time",     Get_Summer_Time_Action);
   Actions_List.Add ("get-temperature",     Get_Temperature_Action);
   Actions_List.Add ("get-valve",           Get_Valve_Action);
   Actions_List.Add ("get-valve-average",   Get_Valve_Average_Action);
   Actions_List.Add ("get-valve-max",       Get_Valve_Max_Action);
   Actions_List.Add ("get-valve-min",       Get_Valve_Min_Action);
   Actions_List.Add ("reboot",              Reboot_Action);
   Actions_List.Add ("reconnect",           Reconnect_Action);
   Actions_List.Add ("set-eco-temperature", Set_Eco_Temperature_Action);
   Actions_List.Add ("set-automatic",       Set_Automatic_Action);
   Actions_List.Add ("set-boost",           Set_Boost_Action);
   Actions_List.Add ("set-manual",          Set_Manual_Action);
   Actions_List.Add ("set-vacation",        Set_Vacation_Action);
   Actions_List.Add ("set-schedule",        Set_Schedule_Action);

   Parameter_List.Add ("airing",        Airing_Parameter);
   Parameter_List.Add ("callback=",     Callback_Parameter);
   Parameter_List.Add ("comfort",       Comfort_Parameter);
   Parameter_List.Add ("cube=",         Cube_Address_Parameter);
   Parameter_List.Add ("days=",         Days_Parameter);
   Parameter_List.Add ("device=",       Device_Address_Parameter);
   Parameter_List.Add ("eco",           Eco_Parameter);
   Parameter_List.Add ("hours=",        Hours_Parameter);
   Parameter_List.Add ("jsonp=",        Callback_Parameter);
   Parameter_List.Add ("minutes=",      Minutes_Parameter);
   Parameter_List.Add ("schedule=",     Schedule_Parameter);
   Parameter_List.Add ("serial=",       Serial_Parameter);
   Parameter_List.Add ("temperature=",  Temperature_Parameter);
   Parameter_List.Add ("temperature+=", Inc_Temperature_Parameter);
   Parameter_List.Add ("temperature-=", Dec_Temperature_Parameter);
   Parameter_List.Add ("weeks=",        Weeks_Parameter);

   MIME_Types.Add ("BMP",  "image/bmp");
   MIME_Types.Add ("CSS",  "text/css");
   MIME_Types.Add ("GIF",  "image/gif");
   MIME_Types.Add ("ICO",  "image/ico");
   MIME_Types.Add ("JPEG", "image/jpeg");
   MIME_Types.Add ("JPG",  "image/jpeg");
   MIME_Types.Add ("PNG",  "image/png");
   MIME_Types.Add ("SVG",  "image/svg");
   MIME_Types.Add ("TIF",  "image/tiff");
   MIME_Types.Add ("TIFF", "image/tiff");
   MIME_Types.Add ("WAV",  "audio/wave");
   MIME_Types.Add ("WEBP", "text/webp");
   MIME_Types.Add ("bmp",  "image/bmp");
   MIME_Types.Add ("sass", "text/x-sass");
   MIME_Types.Add ("scss", "text/x-scss");
   MIME_Types.Add ("css",  "text/css");
   MIME_Types.Add ("gif",  "image/gif");
   MIME_Types.Add ("htm",  "text/html");
   MIME_Types.Add ("html", "text/html");
   MIME_Types.Add ("ico",  "image/ico");
   MIME_Types.Add ("jpeg", "image/jpeg");
   MIME_Types.Add ("jpg",  "image/jpeg");
   MIME_Types.Add ("js",   "text/javascript");
   MIME_Types.Add ("png",  "image/png");
   MIME_Types.Add ("svg",  "image/svg");
   MIME_Types.Add ("tif",  "image/tiff");
   MIME_Types.Add ("tiff", "image/tiff");
   MIME_Types.Add ("ts",   "application/x-typescript");
   MIME_Types.Add ("wav",  "audio/wave");
   MIME_Types.Add ("webp", "text/webp");

end MAX_Settings_Page;
