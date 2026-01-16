--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Deleting                     Luebeck            --
--  Interface                                      Spring, 2019       --
--                                                                    --
--                                Last revision :  20:42 01 Dec 2020  --
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

with Gtk.Button;  use Gtk.Button;

package MAX_Rooms_List.Deleting is
--
-- Delete_Faulty -- Create delete faulty devices dialog
--
--    Rooms -- The rooms list
--
-- Returns :
--
--    The dialog object
--
   function Delete_Faulty
            (  Rooms : not null Rooms_List
            )  return Detached_Interface_Ptr;
--
-- Delete_Selected -- Create dialog
--
--    Rooms -- The rooms list
--
   procedure Delete_Selected
             (  Rooms    : not null Rooms_List;
                Selected : Gtk_Tree_Iter := Null_Iter
             );

private
   type Delete_Mode is (Faulty_Mode, Device_Mode);
   type Delete_Dialog_Record (Mode : Delete_Mode) is
      new Gtk_Dialog_Record
      and Detached_Interface
      and Settings_Handler_Interface with
   record
      List : Rooms_List;
      case Mode is
         when Faulty_Mode => -- Faulty data
            Deleted_Store : Gtk_List_Store;
            Faulty_Store  : Gtk_List_Store;
            Faulty_View   : Gtk_Tree_View;
            Reattach      : Gtk_Button;
            Have_Deleted  : Natural := 0;
            Have_Faulty   : Natural := 0;
         when Device_Mode => -- Delete data
            Cube          : RF_Address  := 0;
            Kind_Of       : Device_Type := Unknown;
            Room          : Room_ID     := No_Room;
            Address       : RF_Address  := 0;
            Devices       : RF_Address_Maps.Map;
      end case;
   end record;
   subtype Device_Dialog_Record is Delete_Dialog_Record (Device_Mode);
   subtype Faulty_Dialog_Record is Delete_Dialog_Record (Faulty_Mode);

   type Device_Dialog is access all Device_Dialog_Record;
   type Faulty_Dialog is access all Faulty_Dialog_Record;

   overriding
      procedure Canceled (Dialog : in out Delete_Dialog_Record);
   overriding
      procedure Detached_Device
                (  Dialog    : in out Delete_Dialog_Record;
                   Cube      : RF_Address;
                   Kind_Of   : Device_Type;
                   Device    : RF_Address;
                   Serial_No : String
                );
   overriding
      procedure Finished (Dialog : in out Delete_Dialog_Record);
   overriding
      function Get_Object
               (  Dialog : not null access Delete_Dialog_Record
               )  return GObject;
   overriding
      procedure On_A_Response
                (  Dialog   : in out Delete_Dialog_Record;
                   Address  : RF_Address;
                   Devices  : Devices_Maps.Map;
                   List     : Rooms_Maps.Map;
                   Expected : in out Natural
                );
   overriding
      procedure On_S_Response
                (  Dialog   : in out Delete_Dialog_Record;
                   Cube     : access Cube_Client'Class;
                   Error    : Boolean;
                   Duty     : Ratio;
                   Slots    : Natural;
                   Expected : in out Natural
                );
   function Find_Faulty
            (  Dialog  : not null access Delete_Dialog_Record;
               Address : RF_Address
            )  return Gtk_Tree_Iter;
   procedure Start_Deleting
             (  Dialog    : not null access Delete_Dialog_Record;
                Cube      : RF_Address;
                Addresses : RF_Address_Array
             );
   package Delete_Handlers is new Gtk.Handlers.User_Callback
           (  GObject_Record,
              Faulty_Dialog
           );
   procedure Device_Changed
             (  Object : access GObject_Record'Class;
                Dialog : Faulty_Dialog
             );

   procedure On_Faulty_Device_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );
   procedure On_Delete_Device_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );

end MAX_Rooms_List.Deleting;
