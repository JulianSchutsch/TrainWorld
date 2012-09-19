with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package ClientServerNet.Test is

   procedure ConnectionMonteCarloSMPipe;

   Tests : Test_Array:=
     (0=>(Name => U("ClientServerNet.ConnectionMonteCarloSMPipe"),
       Test => ConnectionMonteCarloSMPipe'Access));
end ClientServerNet.Test;
