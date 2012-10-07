package body GUI is

   procedure SetContext
     (GUI     : in out GUI_Type;
      Context : Graphics.Context_Ref) is
      pragma Unreferenced(Context);
   begin
      -- 1. Unlink all objects from ObjectImplementations
      -- 2. Free ObjectImplementations
      GUI.ObjectImplementations.SetNull;
      -- 3. Set reference to other ref to null
      GUI.Context.SetNull;
      -- 4. Search for ObjectImplementations again
      -- 5. Set new ObjectImplementations
      -- 6. Distribute ObjectImplementations to all objects
      null;
   end SetContext;
   ---------------------------------------------------------------------------

   procedure SetObjectImplementations
     (GUI           : in out GUI_Type;
      Configuration : Config.ConfigNode_Type) is
      pragma Unreferenced(Configuration);
   begin
      -- 1. Unlink all objects from ObjectImplementations
      -- 2. Free ObjectImplementations
      GUI.ObjectImplementations.SetNull;
      -- 3. Set new ObjectImplementations
      -- 4. Distribute ObjectImplementations to all objects
   end SetObjectImplementations;
   ---------------------------------------------------------------------------

end GUI;
