with TestFramework; use Testframework;
with Basics; use Basics;

package VersionParser.Test is

   procedure VersionTest;

   StrVersionParser : aliased constant String:="VersionParser";

   Tests : constant Test_Array:=
     (0=>(Name => RefConstStr(StrVersionParser'Access),
          Test => VersionTest'Access));

end VersionParser.Test;
