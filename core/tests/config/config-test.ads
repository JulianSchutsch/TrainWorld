with TestFrameWork; use TestFrameWork;

package Config.Test is

   procedure PointerFinalizeTest;
   procedure PathCreationTest;
   procedure PointersLeftTest;

   Tests : constant Test_Array:=
     ((Name => U("Config.PointerFinalize"),
       Test => PointerFinalizeTest'Access),
      (Name => U("Config.PathCreationTest"),
       Test => PathCreationTest'Access),
      (Name => U("Config.PointersLeftTest"),
       Test => PointersLeftTest'Access));

end Config.Test;
