--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Cube_Topology                           Luebeck            --
--  Implementation                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  13:38 14 Sep 2019  --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Gdk.Pixbuf;                  use Gdk.Pixbuf;
with GLib.Messages;               use GLib.Messages;
with GLib.Values;                 use GLib.Values;
with Gtk.Cell_Renderer_Pixbuf;    use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Progress;  use Gtk.Cell_Renderer_Progress;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;    use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                   use Gtk.Enums;
with MAX_IO.Moving;               use MAX_IO.Moving;
with MAX_IO.Pairing;              use MAX_IO.Pairing;
with MAX_IO.Renaming;             use MAX_IO.Renaming;
with Strings_Edit.Quoted;         use Strings_Edit.Quoted;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.Topology;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.Topology;

with Ada.Unchecked_Conversion;
with Image_Cube_XPM;
with Image_Eco_Button_XPM;
with Image_Radiator_XPM;
with Image_Room_XPM;
with Image_Thermometer_XPM;
with Image_Window_Closed_XPM;

package body MAX_Cube_Topology is

   Conflicting : constant String := "dialog-warning";
   Failure     : constant String := "dialog-error";
   Moving      : constant String := "go-previous";
   Pairing     : constant String := "gtk-connect";
   Pending     : constant String := "system-run";
   Removing    : constant String := "gtk-disconnect";
   Renaming    : constant String := "gtk-edit";

   Saved_Type_Column      : constant := 0;
   Saved_Address_Column   : constant := 1;
   Saved_Text_Column      : constant := 2;
   Saved_Serial_No_Column : constant := 3;
   Saved_Duty_Column      : constant := 4;
   Saved_Name_Column      : constant := 5;
   Saved_IP_Column        : constant := 6;
   Saved_Room_ID_Column   : constant := 7;
   Saved_Device_Column    : constant := 8;
   Saved_Status_Column    : constant := 9;
   Saved_Action_Column    : constant := 10;
   Saved_Apply_Column     : constant := 11;
   Saved_Op_Column        : constant := 12;
   Saved_Room_Name_Column : constant := 13;

   function Where (Name : String) return String is
   begin
      return " in MAX_Cube_Topology." & Name;
   end Where;

--     procedure Check_Buttons
--               (  Restore : not null access
--                            Restore_Topology_Record'Class
--               )  is
--     begin
--        if (  Restore.Selected > 0
--           and then
--              (  Restore.Restore_Schedule.Get_Active
--              or else
--                 Restore.Restore_Parameters.Get_Active
--              or else
--                 Restore.Restore_Valve.Get_Active
--           )  )
--        then
--           Restore.OK.Show;
--        else
--           Restore.OK.Hide;
--        end if;
--     end Check_Buttons;

   procedure Apply_Cell_Data
             (  Column  : not null access
                             Gtk_Tree_View_Column_Record'Class;
                Cell    : not null access
                             Gtk_Cell_Renderer_Record'Class;
                Model   : Gtk_Tree_Model;
                Row     : Gtk_Tree_Iter;
                Restore : Restore_Topology
             )  is
   begin
      Cell.Set_Visible
      (  Restore.List.Get_Int (Row, Saved_Apply_Column) in 0..1
      );
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Apply_Cell_Data")
         )  );
   end Apply_Cell_Data;

   procedure Canceled
             (  Restore : in out Restore_Topology_Record
             )  is
   begin
      if Restore.Current /= Null_Iter then
         Restore.Set (Restore.Current, Saved_Status_Column, Failure);
      end if;
      Restore.Update;
      Restore.Error.Show;
      Restore.OK.Show;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Canceled")
         )  );
   end Canceled;

   function Clean_Errors
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Topology
            )  return Boolean is
   begin
      if Gtk.Missed.Get (Data.List, Row, Saved_Status_Column) = Failure
      then
         Gtk.Missed.Set (Data.List, Row, Saved_Status_Column, Pending);
      end if;
      return False;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Clean_Errors")
         )  );
         return False;
   end Clean_Errors;

   procedure Clean_Errors (Restore : in out Restore_Topology_Record) is
   begin
      Walker.Foreach
      (  Restore.List,
         Clean_Errors'Access,
         Restore'Unchecked_Access
      );
   end Clean_Errors;

   procedure Completed
             (  Restore    : in out Restore_Topology_Record;
                Successful : Boolean
             )  is
   begin
      Restore.Update;
      if Successful then
         Restore.Update;
         Restore.Do_Restore (False);
      else
         Restore.OK.Show;
      end if;
   end Completed;

   procedure Cube_Cell_Data
             (  Column  : not null access
                             Gtk_Tree_View_Column_Record'Class;
                Cell    : not null access
                             Gtk_Cell_Renderer_Record'Class;
                Model   : Gtk_Tree_Model;
                Row     : Gtk_Tree_Iter;
                Restore : Restore_Topology
             )  is
   begin
      Cell.Set_Visible (Cube = Restore.Get (Row));
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Cube_Cell_Data")
         )  );
   end Cube_Cell_Data;

   procedure Do_Cancel (Restore : in out Restore_Topology_Record) is
      No : GInt;
   begin
      Restore.Rooms.Set_Pending_Restore (False);
      No := Restore.Find_Page;
      if No >= 0 then
         Restore.Parent.Remove_Page (No);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_Cancel")
         )  );
   end Do_Cancel;

   procedure Do_Restore
             (  Restore        : in out Restore_Topology_Record;
                After_Selected : Boolean
             )  is
   begin
      Restore.Active := True;
      Restore.Warning.Hide;
      Restore.Error.Hide;
      while Restore.Find (After_Selected) loop
         declare
            Row     : Gtk_Tree_Iter renames Restore.Current;
            Kind_Of : constant Device_Type := Restore.Get (Row);
         begin
            Restore.View.Get_Selection.Select_Iter (Row);
            case Kind_Of is
               when Cube =>
                  Restore.Set (Row, Ignore_Op);
               when Unknown =>
                  declare
                     Name : constant String :=
                               Restore.Get
                               (  Row,
                                  Saved_Name_Column
                               );
                  begin
                     Restore.Action.Set_Text
                     (  "Renaming "
                     &  Image (Kind_Of)
                     &  " to "
                     &  Quote (Name)
                     );
                     Restore.OK.Hide;
                     Rename
                     (  Box     => Restore.Cube,
                        Room    => Restore.Get (Row),
                        Name    => Name,
                        Silent  => True,
                        Handler => Restore'Unchecked_Access
                     );
                  end;
                  Restore.Active := False;
                  return;
               when Radiator_Thermostat..Shutter_Contact =>
                  case Op_Type'(Restore.Get (Row)) is
                     when Rename_Device_Op =>
                        declare
                           Name : constant String :=
                                     Restore.Get
                                     (  Row,
                                        Saved_Name_Column
                                     );
                        begin
                           Restore.Action.Set_Text
                           (  "Renaming "
                           &  Image (Kind_Of)
                           &  " to "
                           &  Quote (Name)
                           );
                           Restore.OK.Hide;
                           Rename
                           (  Box     => Restore.Cube,
                              Address => Restore.Get (Row),
                              Name    => Name,
                              Silent  => True,
                              Handler => Restore'Unchecked_Access
                           );
                        end;
                        Restore.Active := False;
                        return;
                     when Move_Device_Op =>
                        declare
                           Room : constant Room_ID := Restore.Get (Row);
                           Name : constant String :=
                                           Restore.Get
                                           (  Row,
                                              Saved_Room_Name_Column
                                           );
                        begin
                           Restore.Action.Set_Text
                           (  "Moving "
                           &  Image (Kind_Of)
                           &  " to "
                           &  Name
                           );
                           Restore.OK.Hide;
                           Move
                           (  Box     => Restore.Cube,
                              Address => Restore.Get (Row),
                              Kind_Of => Kind_Of,
                              Room    => Room,
                              Name    => Name,
                              Silent  => True,
                              Handler => Restore'Unchecked_Access
                           );
                        end;
                        Restore.Active := False;
                        return;
                     when Delete_Device_Op =>
                        Restore.Action.Set_Text
                        (  "Delete "
                        &  Image (Kind_Of)
                        );
                        Restore.OK.Hide;
                        Delete_Paired_Device
                        (  Box     => Restore.Cube,
                           Address => Restore.Get (Row),
                           Cancel  => False,
                           Silent  => True,
                           Handler => Restore'Unchecked_Access
                        );
                        Restore.Active := False;
                        return;
                     when Pair_Device_Op =>
                        Restore.Action.Set_Text
                        (  "Pairing "
                        &  Image (Kind_Of)
                        );
                        Restore.OK.Hide;
                        Restore.Rooms.Pair_Missing
                        (  Cube      => Restore.Cube,
                           Room      => Restore.Get
                                        (  Restore.Find_Room
                                           (  Restore.Get (Row)
                                           ),
                                           Saved_Name_Column
                                        ),
                           Device    => Restore.Get (Row),
                           Kind_Of   => Kind_Of,
                           Serial_No => Restore.Get
                                        (  Row,
                                           Saved_Serial_No_Column
                                        ),
                           Handler   => Restore'Unchecked_Access
                        );
                        Restore.Active := False;
                        return;
                     when others =>
                        Restore.Set (Row, Ignore_Op);
                  end case;
               when Eco_Button =>
                  case Op_Type'(Restore.Get (Row)) is
                     when Rename_Device_Op =>
                        declare
                           Name : constant String :=
                                     Restore.Get
                                     (  Row,
                                        Saved_Name_Column
                                     );
                        begin
                           Restore.Action.Set_Text
                           (  "Renaming "
                           &  Image (Kind_Of)
                           &  " to "
                           &  Quote (Name)
                           );
                           Restore.OK.Hide;
                           Rename
                           (  Box     => Restore.Cube,
                              Address => Restore.Get (Row),
                              Name    => Name,
                              Silent  => True,
                              Handler => Restore'Unchecked_Access
                           );
                        end;
                        Restore.Active := False;
                        return;
                     when Delete_Device_Op =>
                        Restore.Action.Set_Text
                        (  "Delete " & Image (Kind_Of)
                        );
                        Restore.OK.Hide;
                        Delete_Paired_Device
                        (  Box     => Restore.Cube,
                           Address => Restore.Get (Row),
                           Cancel  => False,
                           Silent  => True,
                           Handler => Restore'Unchecked_Access
                        );
                        Restore.Active := False;
                        return;
                     when Pair_Device_Op =>
                        Restore.Action.Set_Text
                        (  "Pairing " & Image (Kind_Of)
                        );
                        Restore.OK.Hide;
                        Restore.Rooms.Pair_Missing
                        (  Cube      => Restore.Cube,
                           Room      => "",
                           Device    => Restore.Get (Row),
                           Kind_Of   => Eco_Button,
                           Serial_No => Restore.Get
                                        (  Row,
                                           Saved_Serial_No_Column
                                        ),
                           Handler   => Restore'Unchecked_Access
                        );
                        Restore.Active := False;
                        return;
                     when others =>
                        Restore.Set (Row, Ignore_Op);
                  end case;
            end case;
         end;
         Restore.Update;
      end loop;
      Restore.Action.Set_Text ("Finished");
      Restore.OK.Show;
      Restore.Active := False;
   exception
      when Use_Error =>
         Restore.Action.Set_Text ("Operation in progress");
         Restore.OK.Show;
         Restore.Active := False;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_Restore")
         )  );
   end Do_Restore;

   function Find
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Topology
            )  return Boolean is
   begin
      if Data.Start = Row then
         Data.Start := Null_Iter;
         return False;
      elsif (  Data.Start = Null_Iter
            and then
               Data.List.Get_Int (Row, Saved_Apply_Column) = 1
            )  then
            Data.Current := Row;
         return True;
      else
         return False;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find")
         )  );
         return False;
   end Find;

   function Find
            (  Restore        : not null access Restore_Topology_Record;
               After_Selected : Boolean
            )  return Boolean is
      Model : Gtk_Tree_Model;
   begin
      if After_Selected then
         Restore.View.Get_Selection.Get_Selected (Model, Restore.Start);
      else
         Restore.Start := Null_Iter;
      end if;
      Restore.Current := Null_Iter;
      Walker.Foreach
      (  Restore.List,
         Find'Access,
         Restore.all'Unchecked_Access
      );
      return Restore.Current /= Null_Iter;
   end Find;

   function Find_Device
            (  Restore : Restore_Topology_Record;
               Address : RF_Address
            )  return Gtk_Tree_Iter is
      Row : Gtk_Tree_Iter := Null_Iter;

      type Local is not null access function
           (  Model : Gtk_Tree_Model;
              Path  : Gtk_Tree_Path;
              Iter  : Gtk_Tree_Iter
           )  return Boolean;
      function "+" is
         new Ada.Unchecked_Conversion
             (  Local,
                Gtk.Tree_Store.Gtk_Tree_Model_Foreach_Func
             );
      function Look_Up
               (  Model : Gtk_Tree_Model;
                  Path  : Gtk_Tree_Path;
                  Iter  : Gtk_Tree_Iter
               )  return Boolean is
      begin
         case Device_Type'(Restore.Get (Iter)) is
            when Cube | Unknown =>
               return False;
            when Radiator_Thermostat..Eco_Button =>
               if Restore.Get (Iter) = Address then
                  Row := Iter;
                  return True;
               else
                  return False;
               end if;
         end case;
      end Look_Up;
   begin
      Restore.List.Foreach (+Look_Up'Access);
      return Row;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find_Device")
         )  );
         return Null_Iter;
   end Find_Device;

   function Find_Page (Restore : Restore_Topology_Record)
      return GInt is
      type Gtk_Widget_Ref is
         access constant Gtk_Widget_Record'Class;
      This : constant Gtk_Widget_Ref :=
                      Restore.Immediate_Parent.all'Unchecked_Access;
   begin
      if Restore.Parent /= null then
         for Index in 0..Restore.Parent.Get_N_Pages - 1 loop
            if (  This
               =  Restore.Parent.Get_Nth_Page
                  (  Index
                  ) .all'Unchecked_Access
               )
            then
               return Index;
            end if;
         end loop;
      end if;
      return -1;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find_Page")
         )  );
         return -1;
   end Find_Page;

   function Find_Room
            (  Restore : Restore_Topology_Record;
               Room    : Room_ID
            )  return Gtk_Tree_Iter is
      Row : Gtk_Tree_Iter := Null_Iter;

      type Local is not null access function
           (  Model : Gtk_Tree_Model;
              Path  : Gtk_Tree_Path;
              Iter  : Gtk_Tree_Iter
           )  return Boolean;
      function "+" is
         new Ada.Unchecked_Conversion
             (  Local,
                Gtk.Tree_Store.Gtk_Tree_Model_Foreach_Func
             );
      function Look_Up
               (  Model : Gtk_Tree_Model;
                  Path  : Gtk_Tree_Path;
                  Iter  : Gtk_Tree_Iter
               )  return Boolean is
      begin
         if (  Restore.Get (Iter) = Unknown
            and then
               Restore.Get (Iter) = Room
            )  then
            Row := Iter;
            return True;
         else
            return False;
         end if;
      end Look_Up;
   begin
      Restore.List.Foreach (+Look_Up'Access);
      return Row;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find_Room")
         )  );
         return Null_Iter;
   end Find_Room;

   procedure Finished (Restore : in out Restore_Topology_Record) is
   begin
      Restore.Update;
      if not Restore.Active then
--           if Restore.Current /= Null_Iter then
--              Restore.Set (Restore.Current, No_Op);
--           end if;
         Restore.Action.Set_Text  ("");
         Restore.Do_Restore (False);
      end if;
   end Finished;

   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter
            )  return RF_Address is
   begin
      return Value (Get (Restore.List, Row, Saved_Address_Column));
   end Get;

   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter
            )  return Room_ID is
      Room : GInt;
   begin
      Room := Restore.List.Get_Int (Row, Saved_Room_ID_Column);
      return Room_ID (Room);
   exception
      when others =>
         return No_Room;
   end Get;

   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter
            )  return Device_Type is
      Result : GInt;
   begin
      Result := Restore.List.Get_Int (Row, Saved_Device_Column);
      return Device_Type'Val (Result);
   exception
      when others =>
         return Unknown;
   end Get;

   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter;
               Column  : GInt
            )  return String is
   begin
      return Gtk.Missed.Get (Restore.List, Row, Column);
   end Get;

   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter
            )  return Op_Type is
      Result : GInt;
   begin
      Result := Restore.List.Get_Int (Row, Saved_Op_Column);
      return Op_Type'Val (Result);
   exception
      when others =>
         return No_Op;
   end Get;

   function Get_Device_Parameters
            (  Kind_Of   : Device_Type;
               Room      : Room_ID;
               Address   : RF_Address;
               Serial_No : String;
               Name      : String
            )  return Device_Parameters is
      procedure Set (Result : in out Device_Parameters) is
      begin
         Result.Address   := Address;
         Result.Serial_No := Serial_No;
         Result.Name      := Name;
         Result.Room      := Room;
      end Set;
   begin
      case Kind_Of is
         when Cube =>
            return Result : Device_Parameters (Cube, Name'Length) do
               Set (Result);
            end return;
         when Shutter_Contact =>
            return Result : Device_Parameters
                            (  Shutter_Contact,
                               Name'Length
                            )  do
               Set (Result);
            end return;
         when Radiator_Thermostat =>
            return Result : Device_Parameters
                            (  Radiator_Thermostat,
                               Name'Length
                            )  do
               Set (Result);
            end return;
         when Radiator_Thermostat_Plus =>
            return Result : Device_Parameters
                            (  Radiator_Thermostat_Plus,
                               Name'Length
                            )  do
               Set (Result);
            end return;
         when Wall_Thermostat =>
            return Result : Device_Parameters
                            (  Wall_Thermostat,
                               Name'Length
                            )  do
               Set (Result);
            end return;
         when Eco_Button =>
            return Result : Device_Parameters
                            (  Eco_Button,
                               Name'Length
                            )  do
               Set (Result);
            end return;
         when Unknown =>
            return Result : Device_Parameters (Unknown, Name'Length) do
               Set (Result);
            end return;
      end case;
   end Get_Device_Parameters;

--     function Get_Rooms_List
--              (  Restore : Restore_Topology_Record
--              )  return Room_To_Device_List.Map is
--        Result : Room_To_Device_List.Map;
--
--        type Local is not null access function
--             (  Model : Gtk_Tree_Model;
--                Path  : Gtk_Tree_Path;
--                Iter  : Gtk_Tree_Iter
--             )  return Boolean;
--        function "+" is
--           new Ada.Unchecked_Conversion
--               (  Local,
--                  Gtk.Tree_Store.Gtk_Tree_Model_Foreach_Func
--               );
--        function Look_Up
--                 (  Model : Gtk_Tree_Model;
--                    Path  : Gtk_Tree_Path;
--                    Iter  : Gtk_Tree_Iter
--                 )  return Boolean is
--           Kind_Of : constant Device_Type := Restore.Get (Iter);
--        begin
--           case Kind_Of is
--              when Cube =>
--                 null;
--              when Radiator_Thermostat..Eco_Button =>
--                 declare
--                    Room  : constant Room_ID := Restore.Get (Iter);
--                    Index : constant Integer := Result.Find (Room);
--                 begin
--                    if Index > 0 then
--                       Result.Get (Index).Ptr.List.Add
--                       (  Restore.Get (Iter),
--                          Get_Device_Parameters
--                          (  Kind_Of   => Kind_Of,
--                             Room      => Room,
--                             Address   => Restore.Get (Iter),
--                             Serial_No => Gtk.Missed.Get
--                                          (  Restore.List,
--                                             Iter,
--                                             Saved_Serial_No_Column
--                                          ),
--                             Name      => Gtk.Missed.Get
--                                          (  Restore.List,
--                                             Iter,
--                                             Saved_Name_Column
--                       )  )               );
--                    end if;
--                 end;
--              when Unknown =>
--                 declare
--                    Room  : constant Room_ID := Restore.Get (Iter);
--                    Index : constant Integer := Result.Find (Room);
--                 begin
--                    if Index <= 0 then
--                       declare
--                          Name : constant String :=
--                                          Gtk.Missed.Get
--                                          (  Restore.List,
--                                             Iter,
--                                             Saved_Name_Column
--                                          );
--                          Item : Room_Devices_List_Handles.Handle;
--                       begin
--                          Item.Set (new Room_Devices_List (Name'Length));
--                          Item.Ptr.Room := Name;
--                          Result.Add (Room, Item);
--                       end;
--                    end if;
--                 end;
--           end case;
--           return False;
--        end Look_Up;
--     begin
--        Restore.List.Foreach (+Look_Up'Access);
--        return Result;
--     exception
--        when Error : others =>
--           Log
--           (  MAX_Domain,
--              Log_Level_Critical,
--              (  "Fault: "
--              &  Exception_Information (Error)
--              &  Where ("Get_Rooms_List")
--           )  );
--           return Result;
--     end Get_Rooms_List;

   function Get_Name
            (  Restore : Restore_Topology_Record;
               Device  : RF_Address
            )  return String is
      Row : constant Gtk_Tree_Iter := Restore.Find_Device (Device);
   begin
      if Row = Null_Iter then
         return "";
      else
         return Restore.Get (Row, Saved_Name_Column);
      end if;
   end Get_Name;

   function Get_Name
            (  Restore : Restore_Topology_Record;
               Room    : Room_ID
            )  return String is
   begin
      if Room = No_Room then
         return "";
      end if;
      declare
         Row : constant Gtk_Tree_Iter := Restore.Find_Room (Room);
      begin
         if Row = Null_Iter then
            return "";
         else
            return Restore.Get (Row, Saved_Name_Column);
         end if;
      end;
   end Get_Name;

   function Get_Object
            (  Restore : not null access Restore_Topology_Record
            )  return GObject is
   begin
      return Restore.all'Unchecked_Access;
   end Get_Object;

   function Get_Room
            (  Restore : Restore_Topology_Record;
               Device  : RF_Address
            )  return String is
      Row : constant Gtk_Tree_Iter := Restore.Find_Device (Device);
   begin
      if Row = Null_Iter then
         return "";
      end if;
      declare
         Room : constant Room_ID := Restore.Get (Row);
      begin
         return Restore.Get_Name (Room);
      end;
   end Get_Room;

   procedure On_Cancel
             (  Button  : access GObject_Record'Class;
                Params  : GValues;
                Restore : Restore_Topology
             )  is
   begin
      Restore.Do_Cancel;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Cancel")
         )  );
   end On_Cancel;

   procedure On_Restore
             (  Button  : access GObject_Record'Class;
                Params  : GValues;
                Restore : Restore_Topology
             )  is
   begin
      Restore.Clean_Errors;
      Restore.Do_Restore (False);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Restore")
         )  );
   end On_Restore;

   procedure On_Toggled
             (  Renderer : access GObject_Record'Class;
                Params   : GValues;
                Restore  : Restore_Topology
             )  is
      Row  : Gtk_Tree_Iter;
      Path : constant String := Get_String (Nth (Params, 1));
   begin
      Row := Get_Iter_From_String (Restore.List, Path);
      if Restore.List.Get_Int (Row, Saved_Apply_Column) = 0 then
         Restore.List.Set (Row, Saved_Apply_Column, 1);
      else
         Restore.List.Set (Row, Saved_Apply_Column, 0);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled")
         )  );
   end On_Toggled;

   function Restore_Topology_New
            (  Pages        : not null access Gtk_Notebook_Record'Class;
               Immediate    : not null access Gtk_Notebook_Record'Class;
               Rooms        : not null access Rooms_List_Record'Class;
               Cube_Address : RF_Address;
               Topology     : Topology_Handles.Handle
            )  return Restore_Topology is
      Result : constant Restore_Topology :=
                        new Restore_Topology_Record;
   begin
      Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
      Result.Parent           := Pages.all'Unchecked_Access;
      Result.Immediate_Parent := Immediate.all'Unchecked_Access;
      Result.Rooms            := Rooms.all'Unchecked_Access;
      Result.Cube             := Cube_Address;

      declare
         Buttons : Gtk_HBox;
      begin
         Gtk_New_Hbox (Buttons, Spacing => 3);
         Result.Pack_Start (Buttons, False, False);
         Cancel_Topology_Buttons.Gtk_New (Result.Cancel);
         Buttons.Pack_Start (Result.Cancel, False, False);
         Start_Topology_Buttons.Gtk_New (Result.OK);
         Buttons.Pack_Start (Result.OK, False, False);
      end;
      Result.List := Gtk_Tree_Store_Newv
                     (  (  Gdk.Pixbuf.Get_Type, -- Icon
                           GType_String,        -- Address
                           GType_String,        -- Text column
                           GType_String,        -- Serial No
                           GType_Int,           -- Duty %
                           GType_String,        -- Name
                           GType_String,        -- IP address
                           GType_Int,           -- Room ID
                           GType_Int,           -- Device type
                           GType_String,        -- Status
                           GType_String,        -- Action
                           GType_Int,           -- Apply
                           GType_Int,           -- Operation
                           GType_String         -- Room name
                     )  );
      declare
         List : Gtk_Tree_Store renames Result.List;
         Data : constant Topology_Data :=
                         Get_Topology (Topology.Ptr.Metadata);
         procedure Set (Row : Gtk_Tree_Iter; Kind_Of : Device_Type) is
            Image : Gdk_Pixbuf;
         begin
            case Kind_Of is
               when Cube =>
                  Image := Image_Cube_XPM.Get_Pixbuf;
               when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                  Image := Image_Radiator_XPM.Get_Pixbuf;
               when Wall_Thermostat =>
                  Image := Image_Thermometer_XPM.Get_Pixbuf;
               when Shutter_Contact =>
                  Image := Image_Window_Closed_XPM.Get_Pixbuf;
               when Eco_Button =>
                  Image := Image_Eco_Button_XPM.Get_Pixbuf;
               when others =>
                  Image := Image_Room_XPM.Get_Pixbuf;
            end case;
            List.Set
            (  Row,
               Saved_Type_Column,
               Image.all'Unchecked_Access
            );
            Image.Unref;
         end Set;

         procedure Add_Device
                   (  Parent    : Gtk_Tree_Iter;
                      Device    : Device_Topology_Data;
                      Room_Name : String
                   )  is
            Row : Gtk_Tree_Iter;
         begin
            List.Append (Row, Parent);
            Set (Row, Device.Kind_Of);
            Gtk.Missed.Set
            (  List,
               Row,
               Saved_Address_Column,
               Image (Device.Address)
            );
            Gtk.Missed.Set
            (  List,
               Row,
               Saved_Serial_No_Column,
               Device.Serial_No
            );
            Gtk.Missed.Set
            (  List,
               Row,
               Saved_Name_Column,
               Device.Name
            );
            Gtk.Missed.Set
            (  List,
               Row,
               Saved_Room_Name_Column,
               Room_Name
            );
            List.Set
            (  Row,
               Saved_Room_ID_Column,
               GInt (Device.Room)
            );
            List.Set
            (  Row,
               Saved_Duty_Column,
               0
            );
            List.Set
            (  Row,
               Saved_Device_Column,
               GInt (Device_Type'Pos (Device.Kind_Of))
            );
            List.Set (Row, Saved_Apply_Column, 1);
         end Add_Device;

         Parent : Gtk_Tree_Iter;
      begin
         List.Append (Parent, Null_Iter);
         Set (Parent, Cube);
         Gtk.Missed.Set
         (  List,
            Parent,
            Saved_Address_Column,
            Image (Cube_Address)
         );
         List.Set
         (  Parent,
            Saved_Device_Column,
            GInt (Device_Type'Pos (Cube))
         );
         Result.Set (Parent, Ignore_Op);
         declare
            Cube : constant Cube_Client_Handle :=
                      Get_Cube (Cube_Address);
            Addr : constant String :=
                      GNAT.Sockets.Image (Cube.Ptr.Get_Client_Address);
         begin
            if Cube.Is_Valid then
               Gtk.Missed.Set
               (  List,
                  Parent,
                  Saved_Serial_No_Column,
                  Cube.Ptr.Get_Serial_No
               );
               Gtk.Missed.Set
               (  List,
                  Parent,
                  Saved_IP_Column,
                  Addr
               );
               Gtk.Missed.Set
               (  List,
                  Parent,
                  Saved_Name_Column,
                  "at " & Addr
               );
               List.Set
               (  Parent,
                  Saved_Room_ID_Column,
                  GInt (No_Room)
               );
               Gtk.Missed.Set
               (  List,
                  Parent,
                  Saved_Room_Name_Column,
                  ""
               );
               List.Set
               (  Parent,
                  Saved_Duty_Column,
                  GInt (Float (Cube.Ptr.Get_Duty) * 100.0)
               );
            end if;
         end;
         for Index in Data.Rooms'Range loop
            declare
               Room : Room_Topology_Data renames Data.Rooms (Index).all;
               Row  : Gtk_Tree_Iter;
            begin
               List.Append (Row, Parent);
               Set (Row, Unknown);
               Gtk.Missed.Set
               (  List,
                  Row,
                  Saved_Name_Column,
                  Room.Name
               );
               List.Set
               (  Row,
                  Saved_Room_ID_Column,
                  GInt (Room.ID)
               );
               Gtk.Missed.Set
               (  List,
                  Row,
                  Saved_Room_Name_Column,
                  Room.Name
               );
               List.Set
               (  Row,
                  Saved_Duty_Column,
                  0
               );
               List.Set
               (  Row,
                  Saved_Device_Column,
                  GInt (Device_Type'Pos (Unknown))
               );
               Result.Set (Row, Ignore_Op);
               for Device in Room.Devices'Range loop
                  Add_Device
                  (  Row,
                     Room.Devices (Device).all,
                     Room.Name
                  );
               end loop;
            end;
         end loop;
         for Device in Data.Devices'Range loop
            Add_Device
            (  Parent,
               Data.Devices (Device).all,
               ""
            );
         end loop;
      end;
      Result.Update;

      Gtk_New (Result.Scroll);
      Result.Scroll.Set_Policy (Policy_Never, Policy_Automatic);
      Result.Pack_Start (Result.Scroll);
      Result.View :=
         Gtk_Tree_View_New_With_Model (To_Interface (Result.List));
      Result.List.Unref;
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Icon      : Gtk_Cell_Renderer_Pixbuf;
         Progress  : Gtk_Cell_Renderer_Progress;
         Toggle    : Gtk_Cell_Renderer_Toggle;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Column.Set_Title ("Name");
         Gtk_New (Icon);
         Column.Pack_Start (Icon, False);
         Column.Add_Attribute (Icon, "pixbuf", Saved_Type_Column);
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", Saved_Name_Column);
         Gtk_New (Progress);
         Column.Pack_Start (Progress, False);
         Column.Add_Attribute (Progress, "value", Saved_Duty_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (True);
         Set_Mode_Cell_Data.Set_Cell_Data_Func
         (  Column,
            Progress,
            Cube_Cell_Data'Access,
            Result
         );

         Gtk_New (Column);
         Column.Set_Title ("Address");
         Gtk_New (Text);
         Column.Pack_Start (Text, False);
         Column.Add_Attribute (Text, "text", Saved_Address_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (False);

         Gtk_New (Column);
         Column.Set_Title ("Serial No");
         Gtk_New (Text);
         Column.Pack_Start (Text, False);
         Column.Add_Attribute (Text, "text", Saved_Serial_No_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (False);

         Gtk_New (Column);
         Column.Set_Title ("Status");
         Gtk_New (Icon);
         Column.Pack_Start (Icon, False);
         Column.Add_Attribute (Icon, "icon-name", Saved_Status_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (False);

         Gtk_New (Column);
         Column.Set_Title ("Restoring action");
         Gtk_New (Toggle);
         Column.Pack_Start (Toggle, False);
         Column.Add_Attribute (Toggle, "active", Saved_Apply_Column);
         Set_Mode_Cell_Data.Set_Cell_Data_Func
         (  Column,
            Toggle,
            Apply_Cell_Data'Access,
            Result
         );
         Restore_Handlers.Connect
         (  Toggle,
            "toggled",
            On_Toggled'Access,
            Result
         );
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", Saved_Action_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (True);

      end;
      Result.Scroll.Add (Result.View);
      Result.View.Expand_All;
      Result.View.Columns_Autosize;  -- Size columns

      declare
         Bar : Gtk_HBox;
      begin
         Bar := Gtk_HBox_New;
         Result.Pack_Start (Bar, False, False);

         Gtk_New (Result.Action);
         Bar.Pack_Start (Result.Action);
         Result.Action.Set_Justify (Justify_Right);

         Gtk_New_From_Icon_Name
         (  Result.Warning,
            "dialog-warning",
            Icon_Size_Dialog
         );
         Result.Warning.Set_Tooltip_Text
         (  "The radio traffic is close to its limit. "
         &  "This may lead to a delay or failure for the operation "
         &  "to complete because the cube will wait for the radio band "
         &  "to become available again."
         );
         Bar.Pack_Start (Result.Warning, False, False);

         Gtk_New_From_Icon_Name
         (  Result.Error,
            Failure,
            Icon_Size_Dialog
         );
         Result.Error.Set_Tooltip_Text
         (  "The last upload operation failed. "
         &  "Usually it is because the radio band limit has been "
         &  "reached. "
         &  "You may try to upload remaining Topologys into "
         &  "thermostats later."
         );
         Bar.Pack_Start (Result.Error, False, False);
      end;
--        Check_Buttons (Result);
      declare
         Dummy  : GInt;
         Height : GInt;
         Width  : GInt;
      begin
         Result.View.Get_Preferred_Height (Dummy, Height);-- tree Result.View size
         Result.View.Get_Preferred_Width  (Dummy, Width);
         Result.Scroll.Set_Size_Request                   -- Set new size
         (  GInt'Min (Width,  500),
            GInt'Min (Height, 300)
         );
      end;
      Restore_Handlers.Connect
      (  Result.Cancel,
         "clicked",
         On_Cancel'Access,
         Result
      );
      Restore_Handlers.Connect
      (  Result.OK,
         "clicked",
         On_Restore'Access,
         Result
      );
      Rooms.Set_Pending_Restore (True);
      return Result;
   end Restore_Topology_New;

   procedure On_A_Response
             (  Restore  : in out Restore_Topology_Record;
                Address  : RF_Address;
                Devices  : Devices_Maps.Map;
                List     : Rooms_Maps.Map;
                Expected : in out Natural
             )  is
   begin
      if Expected > 0 then
         Expected := Expected - 1;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_A_Response")
         )  );
   end On_A_Response;

   procedure On_S_Response
             (  Restore  : in out Restore_Topology_Record;
                Cube     : access Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is
   begin
      begin
         Restore.Rooms.Get_Logger.Update_Status
         (  Cube.Get_RF_Address,
            Error,
            Duty,
            Slots
         );
      exception
         when Status_Error =>
            null;
      end;
      Restore.List.Set
      (  Restore.List.Children (Null_Iter),
         Saved_Duty_Column,
         GInt (Float (Duty) * 100.0)
      );
      if Duty >= Max_Duty then
         Restore.Warning.Show;
      else
         Restore.Warning.Hide;
      end if;
      if Error then
         Restore.Error.Show;
         Restore.OK.Show;
      end if;
      if Expected > 0 then
         Expected := Expected - 1;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_S_Response")
         )  );
   end On_S_Response;

   procedure Set
             (  Restore : not null access Restore_Topology_Record;
                Row     : Gtk_Tree_Iter;
                Column  : GInt;
                Text    : String
             )  is
   begin
      Gtk.Missed.Set (Restore.List, Row, Column, Text);
   end Set;

   procedure Set
             (  Restore : not null access Restore_Topology_Record;
                Row     : Gtk_Tree_Iter;
                Op      : Op_Type
             )  is
   begin
      case Op is
         when No_Op =>
            Restore.Set (Row, Saved_Status_Column, Success);
            Restore.Set (Row, Saved_Action_Column, "");
            Restore.List.Set (Row, Saved_Apply_Column, 2);
         when Conflicting_Op =>
            Restore.Set (Row, Saved_Status_Column, Conflicting);
            Restore.Set
            (  Row,
               Saved_Action_Column,
               "the device has wrong serial number and cannot be used"
            );
            Restore.List.Set (Row, Saved_Apply_Column, 2);
         when Ignore_Op =>
            Restore.Set (Row, Saved_Action_Column, "");
            Restore.List.Set (Row, Saved_Apply_Column, 2);
         when Rename_Device_Op =>
            Restore.Set (Row, Saved_Status_Column, Renaming);
            Restore.Set
            (  Row,
               Saved_Action_Column,
               "rename the device"
            );
--            Restore.List.Set (Row, Saved_Apply_Column, 1);
         when Move_Device_Op =>
            Restore.Set (Row, Saved_Status_Column, Moving);
            Restore.Set
            (  Row,
               Saved_Action_Column,
               "move the device from another room"
            );
--            Restore.List.Set (Row, Saved_Apply_Column, 1);
         when Delete_Device_Op =>
            Restore.Set (Row, Saved_Status_Column, Removing);
            Restore.Set
            (  Row,
               Saved_Action_Column,
               "remove the device from another cube"
            );
--            Restore.List.Set (Row, Saved_Apply_Column, 1);
         when Pair_Device_Op =>
            Restore.Set (Row, Saved_Status_Column, Pairing);
            Restore.Set
            (  Row,
               Saved_Action_Column,
               "pair the device with the cube"
            );
--            Restore.List.Set (Row, Saved_Apply_Column, 1);
      end case;
      Restore.List.Set (Row, Saved_Op_Column, GInt (Op_Type'Pos (Op)));
   end Set;

   function Update
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Topology
            )  return Boolean is
      Address : RF_Address;
      Kind_Of : Device_Type;
      Iter    : Gtk_Tree_Iter;
      Toggle  : GInt;
   begin
      Kind_Of := Data.Get (Row);
      case Kind_Of is
         when Cube =>
            Data.Set (Row, Ignore_Op);
         when Radiator_Thermostat..Eco_Button =>
            Toggle := Data.List.Get_Int (Row, Saved_Apply_Column);
            if Toggle in 0..1 then
               Address := Data.Get (Row);
               Iter    := Data.Rooms.Find (Address);
               if Iter = Null_Iter then
                  Data.Set (Row, Pair_Device_Op);
               elsif (  (  Data.Get (Row, Saved_Serial_No_Column)
                        =  Data.Rooms.Get (Iter, Serial_No_Column)
                        )
                     and then
                        Kind_Of = Data.Rooms.Get (Iter)
                     )  then
                  if Data.Rooms.Get_Cube (Iter) = Data.Cube then
                     if (  Data.Rooms.Get_Room_Name (Iter)
                        =  Data.Get (Row, Saved_Room_Name_Column)
                        )  then
                        if (  Data.Get (Row, Saved_Name_Column)
                           =  Data.Rooms.Get (Iter, Name_Column)
                           )  then
                           Data.Set (Row, No_Op);
                        else
                           Data.Set (Row, Rename_Device_Op);
                        end if;
                     else
                        Data.List.Set -- Set room ID from name
                        (  Row,
                           Saved_Room_ID_Column,
                           GInt
                           (  Data.Rooms.Get
                              (  Data.Cube,
                                 Data.Get (Row, Saved_Room_Name_Column)
                        )  )  );
                        Data.Set (Row, Move_Device_Op);
                     end if;
                  else
                     Data.Set (Row, Delete_Device_Op);
                  end if;
               else
                  Data.Set (Row, Conflicting_Op);
               end if;
            else
               Data.Set (Row, No_Op);
            end if;
         when Unknown =>
            Data.Set (Row, Ignore_Op);
      end case;
      return False;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update")
         )  );
         return False;
   end Update;

   procedure Update (Restore : in out Restore_Topology_Record) is
   begin
      Walker.Foreach
      (  Restore.List,
         Update'Access,
         Restore'Unchecked_Access
      );
   end Update;

end MAX_Cube_Topology;
