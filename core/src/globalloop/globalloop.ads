with Ada.Finalization;

package GlobalLoop is

   ProcessAllreadyEnabled  : Exception;
   ProcessAllreadyDisabled : Exception;

   type Process_Type is new Ada.Finalization.Limited_Controlled with private;

   not overriding
   procedure Process
     (P : in out Process_Type) is null;

   not overriding
   procedure Enable
     (P : in out Process_Type);

   not overriding
   procedure Disable
     (P : in out Process_Type);

   overriding
   procedure Finalize
     (P: in out Process_Type);
   ---------------------------------------------------------------------------

   procedure Process;

private

   type Process_ClassAccess is access all Process_Type'Class;
   type Process_Type is new Ada.Finalization.Limited_Controlled with
      record
         Enabled     : Boolean := False;
         NextProcess : Process_ClassAccess := null;
         LastProcess : Process_ClassAccess := null;
      end record;

end GlobalLoop;
