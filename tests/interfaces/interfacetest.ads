with TestFramework; use Testframework;
with Basics; use Basics;

package InterfaceTest is

   procedure Test;

   StrInterfaceTest : aliased constant String:="Interface test";

   Tests : constant Test_Array:=
     (0=>(Name => RefConstStr(StrInterfaceTest'Access),
          Test => Test'Access));

end InterfaceTest;
