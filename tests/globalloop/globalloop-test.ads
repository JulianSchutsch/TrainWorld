pragma Ada_2012;

with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package GlobalLoop.Test is

   procedure Test;

   Tests : constant Test_Array:=
     (0=>(Name => U("GlobalLoop"),
          Test => Test'Access));

end GlobalLoop.Test;
