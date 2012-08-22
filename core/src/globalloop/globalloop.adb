package body GlobalLoop is

   Processes : Process_ClassAccess:=null;

   pragma Thread_Local_Storage(Processes);

   procedure Finalize
     (P: in out Process_Type) is
   begin
      if P.Enabled then
         P.Disable;
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Enable
     (P : in out Process_Type) is
   begin
      if P.Enabled then
         raise ProcessAllreadyEnabled;
      end if;

      pragma Assert(P.LastProcess=null);
      pragma Assert(P.NextProcess=null);

      -- Regarding Unrestricted_Access:
      -- It may be possible someone creates this process by extension aggregate,
      -- but in this case the Finalize will remove the entry before we
      -- leave the procedure.
      P.NextProcess:=Processes;
      if Processes/=null then
         Processes.LastProcess:=P'Unrestricted_Access;
      end if;
      Processes:=P'Unrestricted_Access;

      P.Enabled:=True;

   end Enable;
   ---------------------------------------------------------------------------

   procedure Disable
     (P : in out Process_Type) is
   begin

      if not P.Enabled then
         raise ProcessAllreadyDisabled;
      end if;
      if P.LastProcess/=null then
         P.LastProcess.NextProcess:=P.NextProcess;
      else
         Processes:=P.NextProcess;
      end if;
      if P.NextProcess/=null then
         P.NextProcess.LastProcess:=P.LastProcess;
      end if;

      P.LastProcess:=null;
      P.NextProcess:=null;

      P.Enabled:=False;

   end Disable;
   ---------------------------------------------------------------------------

   procedure Process is

      Current : Process_ClassAccess:=Processes;

   begin

      while Current/=null loop
         Current.Process;
         Current:=Current.NextProcess;
      end loop;

   end Process;
   ---------------------------------------------------------------------------

end GlobalLoop;
