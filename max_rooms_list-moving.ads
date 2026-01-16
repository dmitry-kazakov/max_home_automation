--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Moving                       Luebeck            --
--  Separate body                                  Spring, 2019       --
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

package MAX_Rooms_List.Moving is

   procedure Move_Selected
             (  Rooms    : Rooms_List;
                Selected : Gtk_Tree_Iter := Null_Iter
             );
private
   type Move_Dialog_Record is new Gtk_Dialog_Record with record
      List    : Rooms_List;
      Cube    : RF_Address;
      Kind_Of : Device_Type;
      Address : RF_Address;
      Combo   : Gtk_Combo_Box_Text;
      Targets : Room_To_Device_List.Map;
   end record;
   type Move_Dialog is access all Move_Dialog_Record'Class;

   procedure On_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );
end MAX_Rooms_List.Moving;
