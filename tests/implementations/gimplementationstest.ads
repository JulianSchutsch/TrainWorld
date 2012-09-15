with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package GImplementationsTest is

   ---------------------------------------------------------------------------
   -- 1. Verify if the correct config is passed to implementations
   procedure CreateTest;

   Tests : constant Test_Array:=
     (0=>(Name => U("Implementations.Create"),
       Test => CreateTest'Access));

end GImplementationsTest;
