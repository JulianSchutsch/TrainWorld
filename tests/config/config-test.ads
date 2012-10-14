with TestFrameWork; use TestFrameWork;

package Config.Test is

   ---------------------------------------------------------------------------
   -- 1. Single path creation (3 levels)
   -- 2. Second path creation (2 levels)
   -- 3. Empty path creation
   -- 4. Path extension (for both paths)
   procedure PathTest;

   ---------------------------------------------------------------------------
   -- 1. Setting and reading Config entries
   -- 2. Setting and reading ImplConfig entries
   procedure ConfigTest;

   ---------------------------------------------------------------------------
   -- 1. Pass Config to separate implementations
   procedure PassConfigTest;

   StrConfigPath   : aliased constant String:="Config.Path";
   StrConfigConfig : aliased constant String:="Config.Config";
   StrPassConfig   : aliased constant String:="PassConfig";

   Tests : constant Test_Array:=
     ((Name => RefConstStr(StrConfigPath'Access),
       Test => PathTest'Access),
      (Name => RefConstStr(StrConfigConfig'Access),
       Test => ConfigTest'Access),
      (Name => RefConstStr(StrPassConfig'Access),
       Test => PassConfigTest'Access));

end Config.Test;
