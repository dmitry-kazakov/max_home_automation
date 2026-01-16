--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Renaming                     Luebeck            --
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

package MAX_Rooms_List.Renaming is

   procedure Rename_Selected
             (  Rooms    : Rooms_List;
                Selected : Gtk_Tree_Iter := Null_Iter
             );
private
   type Rename_Dialog_Record is new Gtk_Dialog_Record with record
      List    : Rooms_List;
      Edit    : Gtk_GEntry;
      Cube    : RF_Address;
      Address : RF_Address    := 0;
      Room    : Room_ID       := No_Room;
      Row     : Gtk_Tree_Iter := Null_Iter;
   end record;
   type Rename_Dialog is access all Rename_Dialog_Record'Class;
end MAX_Rooms_List.Renaming;
