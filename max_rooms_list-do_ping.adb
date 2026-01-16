--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Do_Ping                      Luebeck            --
--  Separate body                                  Summer, 2015       --
--                                                                    --
--                                Last revision :  14:21 11 May 2019  --
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

separate (MAX_Rooms_List)
   function Do_Ping (Rooms : Rooms_List) return Boolean is
begin
   if Rooms.Ping_State < 5 then
      if Pages.Get_Current_Page = 0 then
         Rooms.Ping_Icons (Rooms.Ping_State).Hide;
         Rooms.Ping_State := Rooms.Ping_State + 1;
         Rooms.Ping_Icons (Rooms.Ping_State).Show;
      else
         Rooms.Ping_State := Rooms.Ping_State + 1;
      end if;
   end if;
   declare
      Now : constant Time := Clock;
   begin
      if Rooms.Pairing = null and then Poll_No > Rooms.Poll_No then
         Rooms.Poll_No := Poll_No;
         declare
            Empty : Boolean := True;
         begin
            for Cube in 1..Rooms.Faulty_List.Get_Size loop
               declare
                  Map : Faulty_Device_Maps.Map renames
                        Rooms.Faulty_List.Get (Cube).Ptr.Map;
               begin
                  for Device in reverse 1..Map.Get_Size loop
                     declare
                        This : Faulty_Device_Data'Class renames
                               Map.Get (Device).Ptr.all;
                     begin
                        if Rooms.Poll_No - 1 > This.Poll_No then
                           Map.Remove (Device);
                        else
                           Empty := False;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
            if Empty then
               Rooms.Faulty.Hide;
            else
               Rooms.Faulty.Show;
            end if;
         end;
      end if;
      if Now - Last_To_Go_Update > 0.5 then
         Last_To_Go_Update := Now;
         declare
            To_Go : constant Float := Float (Next_Poll - Now);
         begin
            if To_Go <= 0.0 then
               Rooms.Next_Poll.Set_Text ("now");
            elsif To_Go < 60.0 then
               Rooms.Next_Poll.Set_Text
               (  Image (Integer (Float'Ceiling (To_Go))) & "s"
               );
            elsif To_Go < 3600.0 then
               Rooms.Next_Poll.Set_Text
               (  Image (Integer (Float'Ceiling (To_Go / 60.0)))
               &  "min"
               );
            elsif To_Go < 24.0 * 3600.0 then
               Rooms.Next_Poll.Set_Text
               (  Image (Integer (Float'Ceiling (To_Go / 3600.0)))
               &  "h"
               );
            else
               Rooms.Next_Poll.Set_Text
               (  Image
                  (  Integer
                     (  Float'Ceiling (To_Go / (24.0 * 3600.0))
                  )  )
               &  "d"
               );
            end if;
         end;
         if Rooms.Pairing /= null then
            Rooms.Pairing.Ping_Progress;
         end if;
      end if;
   end;
   if not Rooms.Mails.Is_Enabled then
      return True;
   end if;
   declare
      Report : Boolean := False; -- Not reported device changes
   begin
      for Index in 1..Reports.Get_Size loop
         if not Reports.Get (Index).Reported then
            Report := True;
            exit;
         end if;
      end loop;
      if not Report then -- No changes
         return True;
      end if;
   end;
   declare -- Store changes and see if to send mails
      Send  : Boolean         := False; -- Send mail
      Text  : Root_Stream_Ptr := new Storage_Stream (1024);
      Stamp : constant String := Image (Clock);
   begin
      String'Write
      (  Text,
         "The following devices are reporting low battery:"  & CRLF &
                                                               CRLF &
         "Device Latest report    Description"               & CRLF &
         "------ ---------------- -------------------------" & CRLF
      );
      for Index in 1..Reports.Get_Size loop
         declare
            This    : constant Device_Status_Report :=
                               Reports.Get (Index);
            Address : constant RF_Address := Reports.Get_Key (Index);
            Row     : Gtk_Tree_Iter;
         begin
            if This.Battery_Low then -- Add to the devices list
               Row := Rooms.Find (Address);
               if Row /= Null_Iter then
                  if This.Reported then
                     String'Write
                     (  Text,
                        Image (Address) & ' ' & This.Stamp & ' '
                     );
                  else
                     String'Write
                     (  Text,
                        Image (Address) & "        -         "
                     );
                  end if;
                  String'Write
                  (  Text,
                     Image (Device_Type'(Rooms.Get (Row))) & ": "
                  );
                  String'Write
                  (  Text,
                     ''' & Rooms.Get (Row, Name_Column) & '''
                  );
                  Row := Parent (Rooms.List, Row);
                  if (  Row /= Null_Iter
                     and then
                        Parent (Rooms.List, Row) /= Null_Iter
                     )
                  then
                     String'Write
                     (  Text,
                        (  " installed in '"
                        &  Rooms.Get (Row, Name_Column)
                        &  "'"
                     )  );
                  end if;
                  String'Write (Text, CRLF);
               end if;
            end if;
            if not This.Reported then -- Store changes
               Send := Send or This.Battery_Low;
               Reports.Replace
               (  Index,
                  (  Reported    => True,
                     Battery_Low => This.Battery_Low,
                     Stamp       => Stamp
               )  );
               Store (Address, This.Battery_Low, Stamp);
            end if;
         end;
      end loop;
      if Send then
         Rooms.Mails.Send (False, Text);
      else
         Free (Text); -- Free the stream
      end if;
   exception
      when others =>
         Free (Text);
         raise;
   end;
   return True;
exception
   when Error : others =>
      Log
      (  MAX_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("Do_Ping")
      )  );
      return False;
end Do_Ping;
