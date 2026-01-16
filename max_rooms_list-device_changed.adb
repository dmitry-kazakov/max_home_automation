--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Device_Changed               Luebeck            --
--  Separate body                                  Summer, 2015       --
--                                                                    --
--                                Last revision :  15:58 12 Jan 2021  --
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

with MAX_User;

separate (MAX_Rooms_List)
   procedure Device_Changed
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
   No_IO : IO_Blocker;
begin
   if List.View.Get_Selection.Count_Selected_Rows = 1 then
      declare
         Mode    : Operating_Mode := Automatic;
         Valid   : Boolean;
         Row     : Gtk_Tree_Iter;
         Kind_Of : Device_Type;
      begin
         Row     := List.Get_Selected;
         Kind_Of := Device_Type'(List.Get (Row));
         case Kind_Of is
            when Radiator_Thermostat..Wall_Thermostat =>
               List.Check_Move_Buttons (Row);
               List.Configure.Set_Sensitive (True);
               List.Add_Device.Hide;
               List.Delete_Device.Show;
               List.Disconnect.Hide;
               List.Export_To_MAX.Hide;
               List.Set_NTP.Hide;
               List.Reconnect.Hide;
               List.Rename.Show;
               List.Mode_Label.Show;
               List.Mode.Show;
               if List.Get_Rooms_Count (Row) > 1 then
                  List.Move.Show;
               else
                  List.Move.Hide;
               end if;
               if Kind_Of = Wall_Thermostat then
                  List.Display.Show;
               else
                  List.Display.Hide;
               end if;
               List.Store.Show;
               List.Save.Hide;
               List.Reset.Hide;
               List.Restore.Hide;
               List.Get (Row, Mode, Valid);
               if Valid then
                  case Mode is
                     when Automatic =>
                        List.Mode.Set_Active (0);
--                             List.Temperature.Hide;
--                             List.Degree_Label.Hide;
                        List.Temperature.Show;
                        List.Degree_Label.Show;
                        List.Temperature.Set_Text
                        (  Image (Centigrade'(List.Get_Set (Row)))
                        );
                        List.Calendar.Hide;
                        List.Date.Hide;
                     when Manual =>
                        List.Mode.Set_Active (1);
                        List.Temperature.Show;
                        List.Degree_Label.Show;
                        List.Temperature.Set_Text
                        (  Image (Centigrade'(List.Get_Set (Row)))
                        );
                        List.Calendar.Hide;
                        List.Date.Hide;
                     when Vacation =>
                        List.Mode.Set_Active (2);
                        List.Temperature.Show;
                        List.Degree_Label.Show;
                        List.Temperature.Set_Text
                        (  Image (Centigrade'(List.Get_Set (Row)))
                        );
                        List.Calendar.Show;
                        List.Date.Show;
                        List.Date.Set_Text
                        (  Image (Clock + 3600.0 * 24.0 * 14.0)
                        );
                     when Boost =>
                        List.Mode.Set_Active (3);
                        List.Temperature.Hide;
                        List.Degree_Label.Hide;
                        List.Calendar.Hide;
                        List.Date.Hide;
                  end case;
               end if;
            when Cube =>
               List.Check_Move_Buttons (Row);
               List.Configure.Set_Sensitive (False);
               List.Add_Device.Show;
               List.Delete_Device.Hide;
               declare
                  Cube : constant Cube_Client_Handle :=
                                  Get_Cube (List.Get_Cube (Row));
               begin
                  if Cube.Is_Valid and then Cube.Ptr.Is_Connected then
                     List.Disconnect.Show;
                     List.Reconnect.Hide;
                  else
                     List.Disconnect.Hide;
                     List.Reconnect.Show;
                  end if;
                  if Cube.Is_Valid then
                     List.Reboot.Show;
                  else
                     List.Reboot.Hide;
                  end if;
               end;
               if MAX_User.Is_Windows then
                  List.Export_To_MAX.Show;
               end if;
               List.Display.Hide;
               List.Mode_Label.Hide;
               List.Mode.Hide;
               List.Move.Hide;
               List.Rename.Hide;
               List.Store.Hide;
               List.Temperature.Hide;
               List.Degree_Label.Hide;
               List.Calendar.Hide;
               List.Date.Hide;
               List.Save.Show;
               List.Set_NTP.Show;
               List.Restore.Show;
               List.Reset.Show;
            when Shutter_Contact =>
               List.Check_Move_Buttons (Row);
               List.Configure.Set_Sensitive (False);
               List.Add_Device.Hide;
               List.Delete_Device.Show;
               List.Disconnect.Hide;
               List.Export_To_MAX.Hide;
               List.Reboot.Hide;
               List.Reconnect.Hide;
               List.Rename.Show;
               List.Mode_Label.Hide;
               List.Mode.Hide;
               if List.Get_Rooms_Count (Row) > 1 then
                  List.Move.Show;
               else
                  List.Move.Hide;
               end if;
               List.Display.Hide;
               List.Set_NTP.Hide;
               List.Store.Hide;
               List.Temperature.Hide;
               List.Degree_Label.Hide;
               List.Calendar.Hide;
               List.Date.Hide;
               List.Save.Hide;
               List.Reset.Hide;
               List.Restore.Hide;
            when Eco_Button =>
               List.Check_Move_Buttons (Row);
               List.Configure.Set_Sensitive (False);
               List.Add_Device.Show;
               List.Delete_Device.Show;
               List.Disconnect.Hide;
               List.Export_To_MAX.Hide;
               List.Reconnect.Hide;
               List.Rename.Show;
               List.Mode_Label.Hide;
               List.Mode.Hide;
               List.Move.Hide;
               List.Display.Hide;
               List.Set_NTP.Hide;
               List.Store.Hide;
               List.Temperature.Hide;
               List.Degree_Label.Hide;
               List.Calendar.Hide;
               List.Date.Hide;
               List.Save.Hide;
               List.Reset.Hide;
               List.Restore.Hide;
            when Unknown =>
               List.Check_Move_Buttons (Row);
               List.Configure.Set_Sensitive (False);
               List.Add_Device.Show;
               List.Delete_Device.Show;
               List.Disconnect.Hide;
               List.Export_To_MAX.Hide;
               List.Reconnect.Hide;
               List.Rename.Show;
               List.Mode_Label.Hide;
               List.Mode.Hide;
               List.Move.Hide;
               List.Display.Hide;
               List.Reboot.Hide;
               List.Set_NTP.Hide;
               List.Store.Hide;
               List.Temperature.Hide;
               List.Degree_Label.Hide;
               List.Calendar.Hide;
               List.Date.Hide;
               List.Save.Hide;
               List.Reset.Hide;
               List.Restore.Hide;
         end case;
      end;
      List.Wake_Up.Show;
   else
      List.Add_Device.Hide;
      List.Delete_Device.Hide;
      List.Disconnect.Hide;
      List.Export_To_MAX.Hide;
      List.Reboot.Hide;
      List.Reconnect.Hide;
      List.Move.Hide;
      List.Move_Down.Set_Sensitive (False);
      List.Move_Up.Set_Sensitive   (False);
      List.Rename.Hide;
      List.Reset.Hide;
      List.Set_NTP.Hide;
      declare
         type Found_Type is
              (  Wall_Thermostats,
                 Thermostats,
                 Diverse,
                 Unknown
              );
         Found   : Found_Type     := Unknown;
         Mode    : Operating_Mode := Automatic;
         Kind_Of : Device_Type;
         Valid   : Boolean;
         package Walker is
             new Selected_Foreach_User_Data (Boolean);
         procedure Enumerate
                   (  Model : Gtk_Tree_Model;
                      Path  : Gtk_Tree_Path;
                      Row   : Gtk_Tree_Iter;
                      Data  : Boolean
                   )  is
         begin
            if Found = Diverse then
               return;
            end if;
            Kind_Of := Device_Type'(List.Get (Row));
            case Kind_Of is
               when Radiator_Thermostat..Wall_Thermostat =>
                  List.Get (Row, Mode, Valid);
                  if Valid then
                     if Found = Unknown then
                        if Kind_Of = Wall_Thermostat then
                           Found := Wall_Thermostats;
                           List.Display.Show;
                        else
                           Found := Thermostats;
                           List.Display.Hide;
                        end if;
                        List.Configure.Set_Sensitive (False);
                        List.Disconnect.Hide;
                        List.Export_To_MAX.Hide;
                        List.Reconnect.Hide;
                        List.Mode_Label.Show;
                        List.Mode.Show;
                        List.Set_NTP.Hide;
                        List.Store.Show;
                        List.Save.Hide;
                        List.Reboot.Hide;
                        List.Restore.Hide;
                        case Mode is
                           when Automatic =>
                              List.Mode.Set_Active (0);
--                                   List.Temperature.Hide;
--                                   List.Degree_Label.Hide;
                              List.Temperature.Show;
                              List.Degree_Label.Show;
                              List.Temperature.Set_Text
                              (  Image
                                 (  Centigrade'(List.Get_Set (Row))
                              )  );
                              List.Calendar.Hide;
                              List.Date.Hide;
                           when Manual =>
                              List.Mode.Set_Active (1);
                              List.Temperature.Show;
                              List.Degree_Label.Show;
                              List.Temperature.Set_Text
                              (  Image
                                 (  Centigrade'(List.Get_Set (Row))
                              )  );
                              List.Calendar.Hide;
                              List.Date.Hide;
                           when Vacation =>
                              List.Mode.Set_Active (2);
                              List.Temperature.Show;
                              List.Degree_Label.Show;
                              List.Temperature.Set_Text
                              (  Image
                                 (  Centigrade'(List.Get_Set (Row))
                              )  );
                              List.Calendar.Show;
                              List.Date.Show;
                              List.Date.Set_Text
                              (  Image (Clock + 3600.0 * 24.0 * 14.0)
                              );
                           when Boost =>
                              List.Mode.Set_Active (3);
                              List.Temperature.Hide;
                              List.Degree_Label.Hide;
                              List.Calendar.Hide;
                              List.Date.Hide;
                        end case;
                     elsif (  Found = Wall_Thermostats
                           and then
                              Kind_Of /= Wall_Thermostat
                           )  then
                        Found := Thermostats;
                        List.Display.Hide;
                     end if;
                  end if;
               when others =>
                  Found := Diverse;
            end case;
         end Enumerate;
      begin
         Walker.Selected_Foreach
         (  List.View.Get_Selection,
            Enumerate'Access,
            True
         );
         case Found is
            when Wall_Thermostats | Thermostats =>
               null;
            when Diverse | Unknown =>
               List.Configure.Set_Sensitive (False);
               List.Disconnect.Hide;
               List.Export_To_MAX.Hide;
               List.Reboot.Hide;
               List.Reconnect.Hide;
               List.Mode_Label.Hide;
               List.Mode.Hide;
               List.Display.Hide;
               List.Set_NTP.Hide;
               List.Store.Hide;
               List.Temperature.Hide;
               List.Degree_Label.Hide;
               List.Calendar.Hide;
               List.Date.Hide;
               List.Save.Hide;
               List.Restore.Hide;
         end case;
      end;
      List.Wake_Up.Hide;
   end if;
exception
   when Error : others =>
      Log
      (  MAX_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("Device_Changed")
      )  );
end Device_Changed;
