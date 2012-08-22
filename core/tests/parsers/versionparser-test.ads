with TestFramework; use Testframework;
with Basics; use Basics;

package VersionParser.Test is

   procedure VersionTest;

   Tests : constant Test_Array:=
     (0=>(Name => U("VersionParser"),
          Test => VersionTest'Access));

end VersionParser.Test;
