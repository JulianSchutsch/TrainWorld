with TestFrameWork; use TestFrameWork;

package Allocators.Test is

   procedure TestFirstFitLinearInvalidSizeRequest;
   procedure TestFirstFitLinearValidSizeRequest;
   procedure TestFirstFitLinearMonteCarloAndTaggedMemory;

   StrFFLInvalidSizeRequest : aliased constant String:="FirstFitLinear.InvalidSizeRequest";
   StrFFLValidSizeRequest   : aliased constant String:="FirstFitLinear.ValidSizeRequest";
   StrFFLMonteCarloAndTaggedMemory : aliased constant String:="FirstFitLinear.MonteCarloAndTaggedMemory";

   Tests : Test_Array:=
     ((Name => RefConstStr(StrFFLInvalidSizeRequest'Access),
       Test => TestFirstFitLinearInvalidSizeRequest'Access),
      (Name => RefConstStr(StrFFLValidSizeRequest'Access),
       Test => TestFirstFitLinearValidSizeRequest'Access),
      (Name => RefConstStr(StrFFLMonteCarloAndTaggedMemory'Access),
       Test => TestFirstFitLinearMonteCarloAndTaggedMemory'Access));

end Allocators.Test;
