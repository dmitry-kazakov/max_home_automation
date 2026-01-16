--                                                                    --
--  package Job_Servers             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2025       --
--                                                                    --
--                                Last revision :  17:53 15 Jan 2025  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with Ada.Unchecked_Deallocation;

package body Job_Servers is

   procedure Cancel (Job : in out Abstract_Job'Class) is
   begin
      if Job.Server /= null then
         Job.Server.Queue.Remove (Job);
         Job.State.Cancel;
      end if;
   end Cancel;

   procedure Enqueue
             (  Server : in out Job_Server;
                Job    : in out Abstract_Job'Class
             )  is
      Old_Status : Job_Status;
      Queued     : Boolean;
   begin
      if Server.Queue.Is_Closed then
         raise Use_Error;
      end if;
      Job.State.Enqueue (Job'Unchecked_Access, Server);
      Server.Queue.Put (Job'Unchecked_Access, Queued);
      if not Queued then -- All workers are busy, execute it here
         begin
            Job.State.Set_Status (In_Progress, Old_Status);
            Execute (Job, Server);
            Job.State.Set_Status (Completed, Old_Status);
         exception
            when Error : others =>
               Save_Occurrence (Job.Error, Error);
               Job.State.Set_Status (Failed, Old_Status);
         end;
      end if;
   end Enqueue;

   procedure Finalize (Server : in out Job_Server) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Worker, Worker_Ptr);
      Job : Abstract_Job_Ptr;
   begin
      Server.Queue.Close;
      for Index in Server.Workers'Range loop
         Server.Workers (Index).Stop;
      end loop;
      for Index in Server.Workers'Range loop
         while not Server.Workers (Index)'Terminated loop
            delay 0.001;
         end loop;
         Free (Server.Workers (Index));
      end loop;
      loop
         Job := Server.Queue.Peek;
         exit when Job = null;
         Job.State.Cancel;
      end loop;
   end Finalize;

   function Get_Jobs_Completed
            (  Server : Job_Server;
               No     : Positive
            )  return Natural is
   begin
      if No > Server.Workers_Count then
         raise Constraint_Error;
      else
         return Server.Info (No).Jobs_Completed;
      end if;
   end Get_Jobs_Completed;

   function Get_Status
            (  Server : Job_Server;
               No     : Positive
            )  return Worker_Status is
   begin
      if No > Server.Workers_Count then
         raise Constraint_Error;
      else
         return Server.Info (No).Status;
      end if;
   end Get_Status;

   function Get_Status (Job : Abstract_Job) return Job_Status is
   begin
      return Job.State.Get_Status;
   end Get_Status;

   function Idle_Workers_Count (Server : Job_Server) return Natural is
   begin
      return Server.Queue.Get_Count;
   end Idle_Workers_Count;

   procedure Initialize (Server : in out Job_Server) is
   begin
      for Index in Server.Workers'Range loop
         Server.Workers (Index) :=
            new Worker (Server'Unchecked_Access, Index);
      end loop;
   end Initialize;

   procedure Started (Server : in out Job_Server; No : Positive) is
   begin
      null;
   end Started;

   procedure Wait
             (  Job     : in out Abstract_Job;
                Timeout : Duration := Duration'Last
             )  is
      Status : Job_Status;
   begin
      select
         Job.State.Wait (Status);
      or delay Timeout;
         Cancel (Job);
         Job.State.Wait (Status);
      end select;
      case Status is
         when Cancelled =>
            raise Cancelled_Error;
         when Completed =>
            null;
         when Pending | In_Progress | Aborting =>
            raise Program_Error;
         when Failed =>
            Reraise_Occurrence (Job.Error);
      end case;
   end Wait;

   protected body Job_State is

      procedure Cancel is
      begin
         case Status is
            when Pending =>
               Status := Cancelled;
            when In_Progress =>
               Status := Aborting;
            when Cancelled | Completed | Aborting | Failed =>
               null;
         end case;
      end Cancel;

      procedure Enqueue
                (  Job    : Abstract_Job_Ptr;
                   Server : in out Job_Server'Class
                )  is
      begin
         if Status in Pending..Aborting then
            raise Use_Error;
         else
            Status := Pending;
            Job.Server := Server'Unchecked_Access;
         end if;
      end Enqueue;

      function Get_Status return Job_Status is
      begin
         return Status;
      end Get_Status;

      procedure Set_Status
                (  Status     : Job_Status;
                   Old_Status : out Job_Status
                )  is
      begin
         Old_Status := Job_State.Status;
         Job_State.Status := Status;
      end Set_Status;

      entry Wait (Status : out Job_Status) when
         Status not in Pending..Aborting is
      begin
         Status := Job_State.Status;
      end Wait;

   end Job_State;

   protected body Job_Queue is

      procedure Close is
      begin
         Closed := True;
      end Close;

      entry Get (Job : out Abstract_Job_Ptr)
         when First /= null or Closed is
      begin
         if Closed then
            Job := null;
         else
            Job := First;
            Remove (Job.all);
         end if;
      end Get;

      function Get_Count return Natural is
      begin
         return Get'Count;
      end Get_Count;

      function Is_Closed return Boolean is
      begin
         return Closed;
      end Is_Closed;

      function Peek return Abstract_Job_Ptr is
      begin
         return First;
      end Peek;

      procedure Put
                (  Job    : Abstract_Job_Ptr;
                   Queued : out Boolean
                )  is
      begin
         if Is_Closed then
            raise Use_Error;
         elsif Get'Count = 0 then -- All workers are busy
            Queued := False;
         else
            if First = null then
               First := Job;
            else
               Job.Next := First;
               Job.Previous := First.Previous;
               Job.Next.Previous := Job;
               Job.Previous.Next := Job;
            end if;
            Job.Server := Server.all'Unchecked_Access;
            Queued := True;
         end if;
      end Put;

      procedure Remove (Job : in out Abstract_Job'Class) is
      begin
         if Job'Unchecked_Access = First then
            if Job.Next = First then
               First := null;
            else
               First := First.Next;
            end if;
         end if;
         Job.Previous.Next := Job.Next;
         Job.Next.Previous := Job.Previous;
         Job.Previous := Job'Unchecked_Access;
         Job.Next     := Job'Unchecked_Access;
      end Remove;

   end Job_Queue;

   task body Worker is
      Job  : Abstract_Job_Ptr;
      Old  : Job_Status;
      Info : Worker_Info renames Server.Info (Worker.No);
   begin
      Started (Worker.Server.all, Worker.No);
      loop
         Info.Status := Idle;
         Server.Queue.Get (Job);
         if Job = null then -- The queue is closed
            accept Stop;
            Info.Status := Exiting;
            exit;
         end if;
         Job.State.Set_Status (In_Progress, Old);
         Info.Status := Busy;
         begin
            Execute (Job.all, Worker.Server.all);
            Job.State.Set_Status (Completed, Old);
         exception
            when Error : others =>
               Save_Occurrence (Job.Error, Error);
               Job.State.Set_Status (Failed, Old);
         end;
         Info.Jobs_Completed := Info.Jobs_Completed + 1;
      end loop;
   end Worker;

end Job_Servers;
