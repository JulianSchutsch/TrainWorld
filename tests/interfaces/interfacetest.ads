with TestFramework; use Testframework;
with Basics; use Basics;

package InterfaceTest is

   procedure Test;

   Tests : constant Test_Array:=
     (0=>(Name => U("Interface test"),
          Test => Test'Access));

end InterfaceTest;
