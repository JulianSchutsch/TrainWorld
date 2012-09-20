with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package ClientServerNet.Test is

   procedure ConnectionMonteCarloSMPipe;
   procedure TransferMonteCarloSMPipe;

   Tests : Test_Array:=
     ((Name => U("ClientServerNet.ConnectionMonteCarloSMPipe"),
       Test => ConnectionMonteCarloSMPipe'Access),
      (Name => U("ClientServerNet.TransferMonteCarloSMPipe"),
       Test => TransferMonteCarloSMPipe'Access));
end ClientServerNet.Test;
