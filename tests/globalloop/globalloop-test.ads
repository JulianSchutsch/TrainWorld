pragma Ada_2012;

with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package GlobalLoop.Test is

   procedure Test;

   StrGlobalLoop : aliased constant String:="GlobalLoop";

   Tests : constant Test_Array:=
     (0=>(Name => RefConstStr(StrGlobalLoop'Access),
          Test => Test'Access));

end GlobalLoop.Test;
