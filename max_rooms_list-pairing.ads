--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Pairing                      Luebeck            --
--  Interface                                      Spring, 2019       --
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

with Gtk.Button;           use Gtk.Button;
with Gtk.Grid;             use Gtk.Grid;
with Gtk.Progress_Bar;     use Gtk.Progress_Bar;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;

package MAX_Rooms_List.Pairing is

   procedure Pair_Device
             (  Rooms     : not null Rooms_List;
                Head      : String;
                Cube      : RF_Address;
                Room      : Room_ID            := No_Room;
                Kind_Of   : Device_Type        := Unknown;
                Device    : RF_Address         := 0;
                Serial_No : String             := "?         ";
                Interval  : Duration           := Manual_Pairing_Time;
                Handler   : Pairing_Issuer_Ptr := null
             );
private
   type Pairing_Action is (Add_Paired_Device, Delete_Paired_Device);
   type Pairing_Dialog_Record (Head_Length : Natural) is
      new Gtk_Dialog_Record
      and Pairing_Interface
      and Settings_Handler_Interface with
   record
      List        : Rooms_List;
      Scroll      : Gtk_Scrolled_Window;
      Title       : Gtk_Label;
      Grid        : Gtk_Grid;
      Default     : Gtk_Button;
      Cancel      : Gtk_Button;
      Ignore      : Gtk_Button;
      Stop        : Gtk_Button;
      Retry       : Gtk_Button;
      Progress    : Gtk_Progress_Bar;
      Cube        : RF_Address;
      Action      : Pairing_Action    := Add_Paired_Device;
      Grid_Height : Natural           := 0;
      Pending     : Natural           := 0;
      Room        : Room_ID           := No_Room;
      Successful  : Boolean           := False;
      To_Report   : Boolean           := True;
      Response    : Gtk_Response_Type := Gtk_Response_Delete_Event;
      Combo       : Gtk_Combo_Box_Text;
      Started     : Time;
      No          : Settings_Sequence_No;
      Interval    : Duration;
      Handler     : Pairing_Issuer_Ptr;
      Head        : String (1..Head_Length);
   end record;
   type Pairing_Dialog is access all Pairing_Dialog_Record'Class;
   procedure On_Response
             (  Pairing   : in out Pairing_Dialog_Record;
                Completed : Natural
             );
   function Get_Address
            (  Pairing : Pairing_Dialog_Record;
               Device  : Positive
            )  return RF_Address;
   function Get_Kind_Of
            (  Pairing : Pairing_Dialog_Record;
               Device  : Positive
            )  return Device_Type;
   function Get_Name
            (  Pairing : Pairing_Dialog_Record;
               Device  : Positive
            )  return String;
   function Get_Room (Pairing : Pairing_Dialog_Record) return String;
   function Get_Room (Pairing : Pairing_Dialog_Record) return Room_ID;
   function Get_Serial_No
            (  Pairing : Pairing_Dialog_Record;
               Device  : Positive
            )  return String;
   overriding
      procedure Canceled (Pairing : in out Pairing_Dialog_Record);
   overriding
      procedure Finished (Pairing : in out Pairing_Dialog_Record);
   overriding
      function Get_Object
               (  Pairing : not null access Pairing_Dialog_Record
               )  return GObject;
   overriding
      procedure On_A_Response
                (  Pairing  : in out Pairing_Dialog_Record;
                   Address  : RF_Address;
                   Devices  : Devices_Maps.Map;
                   List     : Rooms_Maps.Map;
                   Expected : in out Natural
                );
   overriding
      procedure On_S_Response
                (  Pairing  : in out Pairing_Dialog_Record;
                   Cube     : access Cube_Client'Class;
                   Error    : Boolean;
                   Duty     : Ratio;
                   Slots    : Natural;
                   Expected : in out Natural
                );
   overriding
      procedure Paired_Device
                (  Pairing   : in out Pairing_Dialog_Record;
                   Box       : RF_Address;
                   Device    : Device_Type;
                   Address   : RF_Address;
                   Serial_No : String
                );
   overriding
      procedure Pairing_Ended (Pairing : in out Pairing_Dialog_Record);
   overriding
      procedure Ping_Progress (Pairing : in out Pairing_Dialog_Record);
   overriding
      procedure Registered
                (  Handler : in out Pairing_Dialog_Record;
                   No      : Settings_Sequence_No
                );

end MAX_Rooms_List.Pairing;
