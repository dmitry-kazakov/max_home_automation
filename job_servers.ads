--                                                                    --
--  package Job_Servers             Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
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

with Ada.Exceptions;
with Ada.Finalization;

package Job_Servers is

   Cancelled_Error : exception;
--
-- Job_Server -- A set of worker task to execute jobs
--
   type Job_Server (Workers_Count : Positive) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Job_Status -- The job status
--
   type Job_Status is
        (  Cancelled,
           Completed,
           Pending,
           In_Progress,
           Aborting,
           Failed
        );
--
-- Worker_Status -- The worker task status
--
   type Worker_Status is (Starting, Idle, Busy, Exiting);
--
-- Abstract_Job -- A job
--
   type Abstract_Job is abstract
      new Ada.Finalization.Limited_Controlled with private;
--
-- Cancel -- A job
--
--    Job - The job to cancel
--
-- This procedure cancels job awaiting execution.
--
   procedure Cancel (Job : in out Abstract_Job'Class);
--
-- Execute -- The job execution
--
--    Job    - The job to execute
--    Server - The job server
--
-- This procedure is called by a worker task to execute the job.
--
-- Exceptions :
--
--    The exception to be propagated by Wait
--
   procedure Execute
             (  Job    : in out Abstract_Job;
                Server : in out Job_Server'Class
             )  is abstract;
--
-- Get_Status -- The job status
--
--    Job - The job to execute
--
-- Returns :
--
--    The current job status
--
   function Get_Status (Job : Abstract_Job) return Job_Status;
--
-- Wait -- For job completion
--
--    Job     - The job to wait for
--    Timeout - Before the job gets canceled
--
-- This procedure waits for completion of a job.
--
-- Exceptions :
--
--    Canceled_Error - The job was canceled
--    Propagated by Job's Execute
--
   procedure Wait
             (  Job     : in out Abstract_Job;
                Timeout : Duration := Duration'Last
             );
------------------------------------------------------------------------
--
-- Enqueue -- Enqueue a job to the server
--
--    Server - The server
--    Job    - The job to perform
--
-- Exceptions :
--
--    Canceled_Error - The server is being finalized
--    Use_Error      - The job is pending or in progress
--
   procedure Enqueue
             (  Server : in out Job_Server;
                Job    : in out Abstract_Job'Class
             );
--
-- Get_Jobs_Completed -- The number of jobs completed by a worker task
--
--    Server - The server
--    No     - The task number 1..Workers_Count
--
-- Returns :
--
--    The number of jobs completed by the worker
--
-- Exceptions :
--
--    Canceled_Error - No is out of range 1..Server.Workers_Count
--
   function Get_Jobs_Completed
            (  Server : Job_Server;
               No     : Positive
            )  return Natural;
--
-- Get_Status -- The the worker status
--
--    Server - The server
--    No     - The task number 1..Workers_Count
--
-- Returns :
--
--    The status
--
-- Exceptions :
--
--    Canceled_Error - No is out of range 1..Server.Workers_Count
--
   function Get_Status
            (  Server : Job_Server;
               No     : Positive
            )  return Worker_Status;
--
-- Idle_Workers_Count -- The number of workkers awaiting a job
--
--    Server - The server
--
-- Returns :
--
--    The number of idle workers
--
   function Idle_Workers_Count (Server : Job_Server) return Natural;
--
-- Started -- A working task startup
--
--    Server - The server
--    No     - The task number 1..Workers_Count
--
-- This procedure  is called  by a working task upon start.  The default
-- implementation does nothing.
--
   procedure Started (Server : in out Job_Server; No : Positive);

private
   use Ada.Exceptions;

   type Abstract_Job_Ptr is access all Abstract_Job'Class;
   type Job_Server_Ptr   is access all Job_Server'Class;
--
-- Event -- A plain event
--
   protected type Job_State is
   --
   -- Cancel -- Change job status on canceling if pending
   --
      procedure Cancel;
   --
   -- Enqueue -- Change job status when Job is to be queued
   --
   --    Job    - The job
   --    Server - The server
   --
   -- Exceptions :
   --
   --    Use_Error - The job is pending or in progress
   --
      procedure Enqueue
                (  Job    : Abstract_Job_Ptr;
                   Server : in out Job_Server'Class
                );
   --
   -- Get_Status -- The current job status
   --
   -- Returns :
   --
   --    The status
   --
      function Get_Status return Job_Status;
   --
   -- Set_Status -- Change the job status
   --
   --    Status     - The status to set
   --    Old_Status - The previous status
   --
      procedure Set_Status
                (  Status     : Job_Status;
                   Old_Status : out Job_Status
                );
   --
   -- Wait -- For the job to complete
   --
   --    Status - The job status
   --
      entry Wait (Status : out Job_Status);

   private
      Status : Job_Status := Completed;
   end Job_State;

   type Abstract_Job is abstract
      new Ada.Finalization.Limited_Controlled with
   record
      Previous : Abstract_Job_Ptr := Abstract_Job'Unchecked_Access;
      Next     : Abstract_Job_Ptr := Abstract_Job'Unchecked_Access;
      Server   : Job_Server_Ptr;
      State    : Job_State;
      Error    : Exception_Occurrence;
   end record;

   protected type Job_Queue (Server : access Job_Server'Class) is
      procedure Close;
      entry Get (Job : out Abstract_Job_Ptr);
      function Get_Count return Natural;
      function Is_Closed return Boolean;
      function Peek return Abstract_Job_Ptr;
      procedure Put (Job : Abstract_Job_Ptr; Queued : out Boolean);
      procedure Remove (Job : in out Abstract_Job'Class);
   private
      Closed : Boolean := False;
      First  : Abstract_Job_Ptr;
   end Job_Queue;

   task type Worker (Server : access Job_Server'Class; No : Positive) is
      entry Stop;
   end Worker;
   type Worker_Ptr is access Worker;
   type Worker_Array is array (Positive range <>) of Worker_Ptr;

   type Worker_Info is record
      Jobs_Completed : Natural := 0;
      Status         : Worker_Status := Starting;
      pragma Atomic (Status);
      pragma Atomic (Jobs_Completed);
   end record;
   type Worker_Info_Array is array (Positive range <>) of Worker_Info;

   type Job_Server (Workers_Count : Positive) is
      new Ada.Finalization.Limited_Controlled with
   record
      Queue   : Job_Queue (Job_Server'Unchecked_Access);
      Workers : Worker_Array      (1.. Workers_Count);
      Info    : Worker_Info_Array (1.. Workers_Count);
   end record;

   procedure Initialize (Server : in out Job_Server);
   procedure Finalize (Server : in out Job_Server);

end Job_Servers;
