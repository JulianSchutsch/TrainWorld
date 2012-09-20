package body ProtectedBasics.Test is

   procedure TestPollingBarrier is
   begin

      -- Checking if Barrier enters are properly counted
      declare
         Barrier : PollingBarrier_Type;
      begin

         Barrier.SetMemberCount(1);

         begin
            Barrier.EnterBarrier;
         exception
            when TooManyBarrierEnters =>
               ReportIssue("Unxpected TooManyBarrierEnters for first Enter with one member");
         end;

         begin
            Barrier.EnterBarrier;
            ReportIssue("Missing TooManyBarrierEnters for second Enter with one member");
         exception
            when TooManyBarrierEnters =>
               null;
         end;

      end;

      declare
         Barrier : PollingBarrier_Type;
         Success : Boolean;
      begin

         Barrier.SetMemberCount(2);
         Barrier.EnterBarrier;
         begin
            Barrier.EnterBarrier;
         exception
            when TooManyBarrierEnters =>
               ReportIssue("Unexpected TooManyBarrierEnters for second Enter with two members");
         end;

         Barrier.CheckBarrier(Success);
         if not Success then
            ReportIssue("Expected Successfull Check of Barrier after second Enter with two members");
         end if;

         begin
            Barrier.EnterBarrier;
            ReportIssue("Missing TooManyBarrierEnters for third Enter with two members after one check");
         exception
            when TooManyBarrierEnters =>
               null;
         end;

      end;

      declare
         Barrier : PollingBarrier_Type;
         Success : Boolean;
      begin

         Barrier.SetMemberCount(1);
         Barrier.EnterBarrier;
         Barrier.CheckBarrier(Success);
         if not Success then
            ReportIssue("Expected successfull check for barrier rewind check");
         end if;
         begin
            Barrier.CheckBarrier(Success);
            ReportIssue("Missing TooManyBarrierChecks for barrier rewind check");
         exception
            when TooManyBarrierChecks =>
               null;
         end;
      end;

      declare
         Barrier : PollingBarrier_Type;
         Success : Boolean;
      begin

         Barrier.SetMemberCount(1);
         Barrier.EnterBarrier;
         Barrier.CheckBarrier(Success);
         Barrier.EnterBarrier;
         begin
            Barrier.EnterBarrier;
            ReportIssue("Missing TooManyBarrierEnters in second enter");
         exception
            when TooManyBarrierEnters =>
               null;
         end;
      end;

      declare
         Barrier : PollingBarrier_Type;
         Success : Boolean;
      begin

         Barrier.SetMemberCount(1);
         Barrier.EnterBarrier;
         Barrier.CheckBarrier(Success);
         Barrier.EnterBarrier;
         Barrier.CheckBarrier(Success);
         if not Success then
            ReportIssue("Expected successfull in second check");
         end if;

      end;

   end TestPollingBarrier;
   ---------------------------------------------------------------------------

end ProtectedBasics.Test;
