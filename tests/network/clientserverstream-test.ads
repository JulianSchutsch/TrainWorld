with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package ClientServerStream.Test is

   procedure ConnectionMonteCarloSMPipe;
   procedure TransferMonteCarloSMPipe;

   Tests : Test_Array:=
     ((Name => U("ClientServerNet.ConnectionMonteCarloSMPipe"),
       Test => ConnectionMonteCarloSMPipe'Access),
      (Name => U("ClientServerNet.TransferMonteCarloSMPipe"),
       Test => TransferMonteCarloSMPipe'Access));

   -- TODO: Corner cases:
   --     Same name used twice for server.
end ClientServerStream.Test;
