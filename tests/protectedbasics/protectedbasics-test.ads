pragma Ada_2012;

with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package ProtectedBasics.Test is

   procedure TestPollingBarrier;

   Tests : constant Test_Array:=
     (0=>(Name => U("ProtectedBasics.PollingBarrier"),
          Test => TestPollingBarrier'Access));

end ProtectedBasics.Test;
