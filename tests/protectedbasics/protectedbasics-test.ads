pragma Ada_2012;

with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package ProtectedBasics.Test is

   procedure TestPollingBarrier;

   StrProtectedBasicsPollingBarrier : aliased constant String:="ProtectedBasics.PollingBarrier";

   Tests : constant Test_Array:=
     (0=>(Name => RefConstStr(StrProtectedBasicsPollingBarrier'Access),
          Test => TestPollingBarrier'Access));

end ProtectedBasics.Test;
