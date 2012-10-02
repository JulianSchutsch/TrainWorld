with TestFrameWork; use TestFrameWork;

package Allocators.Test is

   procedure TestFirstFitLinearInvalidSizeRequest;
   procedure TestFirstFitLinearValidSizeRequest;
   procedure TestFirstFitLinearMonteCarloAndTaggedMemory;

   Tests : Test_Array:=
     ((Name => U("FirstFitLinear.InvalidSizeRequest"),
       Test => TestFirstFitLinearInvalidSizeRequest'Access),
      (Name => U("FirstFitLinear.ValidSizeRequest"),
       Test => TestFirstFitLinearValidSizeRequest'Access),
      (Name => U("FirstFitLinear.MonteCarloAndTaggedMemory"),
       Test => TestFirstFitLinearMonteCarloAndTaggedMemory'Access));

end Allocators.Test;
