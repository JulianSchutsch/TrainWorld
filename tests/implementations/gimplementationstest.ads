with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package GImplementationsTest is

   ---------------------------------------------------------------------------
   -- 1. Verify if the correct config is passed to implementations
   procedure CreateTest;

   StrImplementationsCreate : aliased constant String:="Implementations.Create";

   Tests : constant Test_Array:=
     (0=>(Name => RefConstStr(StrImplementationsCreate'Access),
       Test => CreateTest'Access));

end GImplementationsTest;
