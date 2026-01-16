--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_MQTT_Server                             Luebeck            --
--  Implementation                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  18:37 02 Jun 2023  --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with GNAT.Sockets.MQTT;      use GNAT.Sockets.MQTT;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

package body MAX_MQTT_Server is

   Eco_Messages       : MQTT_Messages_Array (1..6);
   Radiator_Messages  : MQTT_Messages_Array (1..10);
   Shutter_Messages   : MQTT_Messages_Array (1..7);
   Wall_Messages      : MQTT_Messages_Array (1..9);
   Statistic_Messages : MQTT_Messages_Array (1..4);

   function Image (Mode : Operating_Mode) return String is
   begin
      case Mode is
         when Automatic =>
            return "automatic";
         when Manual =>
            return "manual";
         when Boost =>
            return "boost";
         when Vacation =>
            return "vacation";
      end case;
   end Image;

   function Encode
            (  Data      : Device_Data;
               Serial_No : String;
               Name      : String;
               Offset    : Centigrade
            )  return String is
      function Image (Value : Boolean) return String is
      begin
         if Value then
            return "true";
         else
            return "false";
         end if;
      end Image;
      Text    : String (1..1024);
      Pointer : Integer := Text'First;
   begin
      Put (Text, Pointer, "{ ");
      Put (Text, Pointer, """address"":");
      Put (Text, Pointer, Integer (Data.Address));
      Put (Text, Pointer, ", ""error"":");
      Put (Text, Pointer, Image (Data.Error));
      Put (Text, Pointer, ", ""initialized"":");
      Put (Text, Pointer, Image (Data.Initialized));
      Put (Text, Pointer, ", ""battery_low"":");
      Put (Text, Pointer, Image (Data.Battery_Low));
      Put (Text, Pointer, ", ""link_error"":");
      Put (Text, Pointer, Image (Data.Link_Error));
      Put (Text, Pointer, ", ""panel_locked"":");
      Put (Text, Pointer, Image (Data.Panel_Locked));
      Put (Text, Pointer, ", ""gateway_known"":");
      Put (Text, Pointer, Image (Data.Gateway_Known));
      Put (Text, Pointer, ", ""summer_time"":");
      Put (Text, Pointer, Image (Data.DST));
      Put (Text, Pointer, ", ""mode"":");
      case Data.Mode is
         when Automatic => Put (Text, Pointer, """automatic""");
         when Manual    => Put (Text, Pointer, """manual""");
         when Boost     => Put (Text, Pointer, """boost""");
         when Vacation  => Put (Text, Pointer, """vacation""");
      end case;
      Put (Text, Pointer, ", ""type"":");
      case Data.Kind_Of is
         when Cube =>
            Put (Text, Pointer, """cube""");
         when Eco_Button =>
            Put (Text, Pointer, """eco button""");
         when Shutter_Contact =>
            Put (Text, Pointer, """shutter contact""");
         when Radiator_Thermostat =>
            Put (Text, Pointer, """radiator thermostat""");
         when Radiator_Thermostat_Plus =>
            Put (Text, Pointer, """radiator thermostat plus""");
         when Wall_Thermostat =>
            Put (Text, Pointer, """wall thermostat""");
         when Unknown =>
            Put (Text, Pointer, """unknown""");
      end case;
      case Data.Kind_Of is
         when Cube | Eco_Button | Unknown =>
            null;
         when Shutter_Contact =>
            Put (Text, Pointer, ", ""open"":");
            Put (Text, Pointer, Image (Data.Open));
         when Radiator_Thermostat..Wall_Thermostat =>
            Put (Text, Pointer, ", ""set_temperature"":");
            Put (Text, Pointer, Image (Data.Set_Temperature));
            Put (Text, Pointer, ", ""new_temperature"":");
            Put (Text, Pointer, Image (Data.New_Temperature));
            case Data.Kind_Of is
               when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                  Put (Text, Pointer, ", ""valve"":");
                  Put
                  (  Text,
                     Pointer,
                     Image
                     (  Integer (Float (Data.Valve_Position) * 100.0)
                  )  );
                  if Data.Temperature /= Centigrade'First then
                     Put (Text, Pointer, ", ""temperature"":");
                     Put (Text, Pointer, Image (Data.Temperature));
                  elsif Data.Latest_Temperature /= Centigrade'First then
                     Put (Text, Pointer, ", ""temperature"":");
                     Put
                     (  Text,
                        Pointer,
                        Image (Data.Latest_Temperature)
                     );
                  end if;
                  Put (Text, Pointer, ", ""offset"":");
                  Put (Text, Pointer, Image (Offset));
               when Wall_Thermostat =>
                  Put (Text, Pointer, ", ""temperature"":");
                  Put (Text, Pointer, Image (Data.Temperature));
                  Put (Text, Pointer, ", ""offset"":");
                  Put (Text, Pointer, Image (Offset));
               when others =>
                  null;
            end case;
      end case;
      Put (Text, Pointer, ", ""serial_no"":" & Quote (Serial_No));
      Put (Text, Pointer, ", ""name"":" & Quote (Name));
      Put (Text, Pointer, " }");
      return Text (Text'First..Pointer - 1);
   end Encode;

   procedure Publish
             (  Client    : in out ELV_MAX_Cube_Client'Class;
                Server    : in out MQTT_Server'Class;
                Cube      : RF_Address;
                Connected : Boolean;
                Policy    : Message_Type
             )  is
      Topic : constant String := Image (Cube) & "/connection";
   begin
      if Connected then
         Server.Publish (Topic, "on", Policy => Policy);
      else
         Server.Publish (Topic, "off", Policy => Policy);
      end if;
   end Publish;

   procedure Publish
             (  Client    : in out ELV_MAX_Cube_Client'Class;
                Server    : in out MQTT_Server'Class;
                Data      : Device_Data;
                Serial_No : String;
                Name      : String;
                Offset    : Centigrade;
                Policy    : Message_Type
             )  is
      procedure Do_Battery
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "battery");
         if Data.Battery_Low then
            Set_Message (Message, "low");
         else
            Set_Message (Message, "high");
         end if;
      end Do_Battery;

      procedure Do_Error
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "error");
         if Data.Error then
            Set_Message (Message, "error");
         else
            Set_Message (Message, "ok");
         end if;
      end Do_Error;

      procedure Do_Initialized
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "initialized");
         if Data.Initialized then
            Set_Message (Message, "ok");
         else
            Set_Message (Message, "uninitialized");
         end if;
      end Do_Initialized;

      procedure Do_Link
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "link");
         if Data.Link_Error then
            Set_Message (Message, "error");
         else
            Set_Message (Message, "ok");
         end if;
      end Do_Link;

      procedure Do_Mode
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "mode");
         Set_Message (Message, Image (Data.Mode));
      end Do_Mode;

      procedure Do_Panel_Locked
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "panel locked");
         if Data.Panel_Locked then
            Set_Message (Message, "locked");
         else
            Set_Message (Message, "unlocked");
         end if;
      end Do_Panel_Locked;

      procedure Do_Set_Temperature
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "set temperature");
         Set_Message (Message, Image (Data.Set_Temperature));
      end Do_Set_Temperature;

      procedure Do_Status_JSON
                (  Message   : in out MQTT_Message;
                   Serial_No : String;
                   Name      : String;
                   Offset    : Centigrade
                )  is
      begin
         Set_Topic
         (  Message,
            (  Image (Client.Get_RF_Address)
            &  "/status/"
            &  Image (Data.Address)
            &  "/json"
         )  );
         Set_Message
         (  Message,
            Encode (Data, Serial_No, Name, Offset)
         );
      end Do_Status_JSON;

      procedure Do_Temperature
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "temperature");
         Set_Message (Message, Image (Data.Temperature));
      end Do_Temperature;

      procedure Do_Valve
                (  Message : in out MQTT_Message;
                   Prefix  : String
                )  is
      begin
         Set_Topic (Message, Prefix & "valve");
         Set_Message
         (  Message,
            Image (Integer (Float (Data.Valve_Position) * 100.0))
         );
      end Do_Valve;

   begin
      case Data.Kind_Of is
         when Shutter_Contact =>
            declare
               Root : constant String :=
                               Image (Client.Get_RF_Address) &
                               "/shutter contact/"           &
                               Image (Data.Address)          &
                               '/';
            begin
               Do_Battery (Shutter_Messages (1), Root);
               Set_Topic (Shutter_Messages (2), Root & "status");
               if Data.Open then
                  Set_Message (Shutter_Messages (2), "open");
               else
                  Set_Message (Shutter_Messages (2), "close");
               end if;
               Do_Link         (Shutter_Messages (3), Root);
               Do_Error        (Shutter_Messages (4), Root);
               Do_Initialized  (Shutter_Messages (5), Root);
               Do_Panel_Locked (Shutter_Messages (6), Root);
               Do_Status_JSON
               (  Shutter_Messages (7),
                  Serial_No,
                  Name,
                  0.0
               );
               Server.Publish (Shutter_Messages, Policy => Policy);
            end;
         when Radiator_Thermostat | Radiator_Thermostat_Plus =>
            declare
               Root : constant String :=
                               Image (Client.Get_RF_Address) &
                               "/radiator thermostat/"       &
                               Image (Data.Address)          &
                               '/';
            begin
               Do_Battery         (Radiator_Messages (1), Root);
               Do_Mode            (Radiator_Messages (2), Root);
               Do_Set_Temperature (Radiator_Messages (3), Root);
               Do_Valve           (Radiator_Messages (4), Root);
               Do_Link            (Radiator_Messages (5), Root);
               Do_Error           (Radiator_Messages (6), Root);
               Do_Initialized     (Radiator_Messages (7), Root);
               Do_Panel_Locked    (Radiator_Messages (8), Root);
               Do_Status_JSON
               (  Radiator_Messages (9),
                  Serial_No,
                  Name,
                  Offset
               );
               if Data.Temperature = Centigrade'First then
                  Server.Publish
                  (  Messages => Radiator_Messages (1..9),
                     Policy   => Updated
                  );
               else
                  Do_Temperature (Radiator_Messages (10), Root);
                  Server.Publish (Radiator_Messages, Policy => Policy);
               end if;
            end;
         when Wall_Thermostat =>
            declare
               Root : constant String :=
                               Image (Client.Get_RF_Address) &
                               "/wall thermostat/"           &
                               Image (Data.Address)          &
                               '/';
            begin
               Do_Battery         (Wall_Messages (1), Root);
               Do_Mode            (Wall_Messages (2), Root);
               Do_Set_Temperature (Wall_Messages (3), Root);
               Do_Temperature     (Wall_Messages (4), Root);
               Do_Link            (Wall_Messages (5), Root);
               Do_Error           (Wall_Messages (6), Root);
               Do_Initialized     (Wall_Messages (7), Root);
               Do_Panel_Locked    (Wall_Messages (8), Root);
               Do_Status_JSON
               (  Wall_Messages (9),
                  Serial_No,
                  Name,
                  Offset
               );
               Server.Publish (Wall_Messages, Policy => Policy);
            end;
         when Eco_Button =>
            declare
               Root : constant String :=
                               Image (Client.Get_RF_Address) &
                               "/eco button/"                &
                               Image (Data.Address)          &
                               '/';
            begin
               Do_Battery      (Eco_Messages (1), Root);
               Do_Link         (Eco_Messages (2), Root);
               Do_Error        (Eco_Messages (3), Root);
               Do_Initialized  (Eco_Messages (4), Root);
               Do_Panel_Locked (Eco_Messages (5), Root);
               Do_Status_JSON
               (  Eco_Messages (6),
                  Serial_No,
                  Name,
                  0.0
               );
               Server.Publish (Eco_Messages, Policy => Policy);
            end;
         when others =>
            null;
      end case;
   exception
      when Status_Error =>
         null;
   end Publish;

   procedure Publish
             (  Client  : in out ELV_MAX_Cube_Client'Class;
                Server  : in out MQTT_Server'Class;
                Cube    : RF_Address;
                Average : Integer;
                Min     : Integer;
                Max     : Integer;
                Policy  : Message_Type
             )  is
      Root : constant String := Image (Client.Get_RF_Address) &
                                "/thermostat valves/";
   begin
      Set_Topic (Statistic_Messages (1), Root & "max");
      Set_Message (Statistic_Messages (1), Image (Max));
      Set_Topic (Statistic_Messages (2), Root & "min");
      Set_Message (Statistic_Messages (2), Image (Min));
      Set_Topic (Statistic_Messages (3), Root & "average");
      Set_Message (Statistic_Messages (3), Image (Average));
      Server.Publish (Statistic_Messages, Policy => Policy);
   end Publish;

   procedure Publish
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Server : in out MQTT_Server'Class;
                Cube   : RF_Address;
                Duty   : Ratio;
                Policy : Message_Type
             )  is
   begin
      Server.Publish
      (  Topic   => Image (Cube) & "/duty",
         Message => Image (Float (Duty), AbsSmall => -2),
         Policy  => Policy
      );
   end Publish;

end MAX_MQTT_Server;
