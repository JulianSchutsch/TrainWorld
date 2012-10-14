with TestFrameWork; use TestFrameWork;

package ClientServerStream.Test is

   procedure ConnectionMonteCarloSMPipe;
   procedure TransferMonteCarloSMPipe;

   StrCSNConnectionMonteCarloSMPipe : aliased constant String:="ClientServerNet.ConnectionMonteCarloSMPipe";
   StrCSNTransferMonteCarloSMPipe   : aliased constant String:="ClientServerNet.TransferMonteCarloSMPipe";

   Tests : Test_Array:=
     ((Name => RefConstStr(StrCSNConnectionMonteCarloSMPipe'Access),
       Test => ConnectionMonteCarloSMPipe'Access),
      (Name => RefConstStr(StrCSNTransferMonteCarloSMPipe'Access),
       Test => TransferMonteCarloSMPipe'Access));

   -- TODO: Corner cases:
   --     Same name used twice for server.
end ClientServerStream.Test;
