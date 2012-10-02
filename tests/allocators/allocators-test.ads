with TestFrameWork; use TestFrameWork;

package Allocators.Test is

   procedure TestFirstFitLinear;

   Tests : Test_Array:=
     (0=>(Name => U("FirstFitLinear"),
          Test => TestFirstFitLinear'Access));

end Allocators.Test;
