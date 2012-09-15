with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

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

   Tests : constant Test_Array:=
     ((Name => U("Config.Path"),
       Test => PathTest'Access),
      (Name => U("Config.Config"),
       Test => ConfigTest'Access),
      (Name => U("PassConfig"),
       Test => PassConfigTest'Access));

end Config.Test;
