--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Graphs                                  Luebeck            --
--  Interface                                      Autumn, 2015       --
--                                                                    --
--                                Last revision :  18:44 15 Jan 2021  --
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

with Ada.Calendar;      use Ada.Calendar;
with GLib;              use GLib;
with Gtk.Box;           use Gtk.Box;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Grid;          use Gtk.Grid;
with Gtk.Label;         use Gtk.Label;
with Gtk.Notebook;      use Gtk.Notebook;
with Gtk.Spin_Button;   use Gtk.Spin_Button;
with Gtk.Oscilloscope;  use Gtk.Oscilloscope;
with Gtk.Widget;        use Gtk.Widget;
with MAX_IO;            use MAX_IO;

with Generic_Map;
with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
with Gtk.Handlers;
with Gtk.Layered.Refresh_Engine;
with Gtk.Oscilloscope.Sweeper_Panel;
with Object.Handle;

package MAX_Graphs is
   use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
--
-- Graphs_Record -- Graphs per room
--
   type Graphs_Record is new Gtk_Widget_Record with private;
   type Graphs is access all Graphs_Record'Class;

   procedure Add
             (  History : not null access Graphs_Record;
                Cube    : RF_Address;
                Room    : Room_ID;
                Name    : String
             );
   procedure Delete
             (  History : not null access Graphs_Record;
                Cube    : RF_Address;
                Room    : Room_ID
             );
   procedure Feed
             (  History     : not null access Graphs_Record;
                Address     : RF_Address;
                Temperature : Centigrade;
                Stamp       : Time
             );
   procedure Feed
             (  History     : not null access Graphs_Record;
                Address     : RF_Address;
                Valve       : Ratio;
                Stamp       : Time
             );
   procedure Feed
             (  History     : not null access Graphs_Record;
                Address     : RF_Address;
                Temperature : Centigrade;
                Offset      : Centigrade;
                Stamp       : Time
             );
   function Is_In
            (  History : not null access Graphs_Record;
               Cube    : RF_Address;
               Room    : Room_ID
            )  return Boolean;
   procedure Rename
             (  History : not null access Graphs_Record;
                Cube    : RF_Address;
                Room    : Room_ID;
                Name    : String
             );
   procedure Rename
             (  History : not null access Graphs_Record;
                Cube    : RF_Address;
                Address : RF_Address;
                Name    : String
             );
   procedure Reorder
             (  History  : not null access Graphs_Record;
                Cube     : RF_Address;
                Room     : Room_ID;
                Position : GLib.GInt
             );
   procedure Set_Auto_Scale
             (  History : not null access Graphs_Record
             );
   procedure Set_Fixed_Scale
             (  History     : not null access Graphs_Record;
                Low         : Centigrade;
                High        : Centigrade
             );
--
-- Graphs_Overview_Record -- Graphs overview
--
   type Graphs_Overview_Record is new Gtk_Widget_Record with private;
   type Graphs_Overview is access all Graphs_Overview_Record'Class;
   function Is_In
            (  Overview : not null access Graphs_Overview_Record;
               Cube     : RF_Address;
               Room     : Room_ID
            )  return Boolean;
   procedure Gtk_Graphs_New
             (  History  : out Graphs;
                Overview : out Graphs_Overview
             );
private
   use Gtk.Layered.Refresh_Engine;
   use Gtk.Oscilloscope.Sweeper_Panel;

   type Oscilloscopes_Array is
      array (Positive range <>) of Gtk_Oscilloscope;

   type Graph_Data is record
      Oscilloscope    : Gtk_Oscilloscope;
      Channel         : Channel_Number;
      Is_Temperature  : Gtk_Label;
      Set_Temperature : Gtk_Label;
   end record;
   package Graph_Maps is new Generic_Map (RF_Address, Graph_Data);
------------------------------------------------------------------------
-- Overview graphs
--
   type Room_Key is record
      Cube : RF_Address;
      Room : Room_ID;
   end record;
   function "<" (Left, Right : Room_Key) return Boolean;

   type Room_Data is new Object.Entity with record
      Oscilloscope : Gtk_Oscilloscope;
      Title        : Gtk_Label;
      Average      : GDouble := 0.0;
   end record;
   type Room_Data_Ptr is access Room_Data'Class;
   package Room_Data_Handles is
      new Object.Handle (Room_Data, Room_Data_Ptr);

   package Graph_Overview_Maps is
      new Generic_Map (Room_Key, Room_Data_Handles.Handle);

   type Valve_Data is record
      Channel : Channel_Number;
      Room    : Room_Data_Ptr;
   end record;
   package Valve_Maps is new Generic_Map (RF_Address, Valve_Data);

   type Graphs_Overview_Record is new Gtk_Grid_Record with record
      Slots        : Graph_Overview_Maps.Map;
      Temperatures : Graph_Maps.Map;
      Valves       : Valve_Maps.Map;
      Schedules    : Graph_Maps.Map;
      Scaler       : Gtk_Oscilloscope;
      Sweeper      : Gtk_Oscilloscope_Sweeper_Panel;
      Check        : Gtk_Check_Button;
      Period       : Gtk_Spin_Button;
      History      : Graphs;
   end record;
------------------------------------------------------------------------
-- Individual room graphs
--
   type Thermostat_Data is record
      Kind_Of : Device_Type;
      Name    : Gtk_Label;
   end record;
   package Thermostat_Data_Maps is
      new Generic_Map (RF_Address, Thermostat_Data);

   type Room_Graphs_Record (Size : Natural) is
      new Gtk_VBox_Record with
   record
      Cube          : RF_Address;
      Room          : Room_ID;
      Name          : Gtk_Label;
      Period        : Gtk_Spin_Button;
      Check         : Gtk_Check_Button;
      Thermostats   : Thermostat_Data_Maps.Map;
      Oscilloscopes : Oscilloscopes_Array (1..Size);
   end record;
   type Room_Graphs is access all Room_Graphs_Record'Class;

   function Gtk_Room_Graphs_New
            (  History : not null access Graphs_Record;
               Cube    : RF_Address;
               Room    : Room_ID
            )  return Room_Graphs;

   type Graphs_Record is new Gtk_VBox_Record with record
      Pages         : Gtk_Notebook;
      Temperatures  : Graph_Maps.Map;
      Valves        : Graph_Maps.Map;
      Schedules     : Graph_Maps.Map;
      Overview      : Graphs_Overview;
      In_On_Refresh : Boolean := False;
   end record;
------------------------------------------------------------------------
   package Graphs_Handlers is
      new Gtk.Handlers.User_Callback (Gtk_Widget_Record, Graphs);
   use Graphs_Handlers;

   procedure On_Toggled_Grid
             (  Widget  : access Gtk_Widget_Record'Class;
                History : Graphs
             );

   procedure On_Refresh_Changed
             (  Widget  : access Gtk_Widget_Record'Class;
                History : Graphs
             );

   type MAX_Refresh_Engine is
      new Layered_Refresh_Engine with null record;
   overriding procedure Refresh (Engine : in out MAX_Refresh_Engine);

end MAX_Graphs;
