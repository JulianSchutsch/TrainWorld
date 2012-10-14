package body GUI.Window is

   procedure SetObjectImplementation
     (Window               : in out Window_Type;
      ObjectImplementation : GUIObjectImplementation_Ref) is
   begin
      Window.Impl:=ObjectImplementation.I.CreateWindowImpl;
   end SetObjectImplementation;
   ---------------------------------------------------------------------------

   procedure ResetObjectImplementation
     (Window               : in out Window_Type;
      ObjectImplementation : GUIObjectImplementation_Ref) is
   begin
      ObjectImplementation.I.DestroyWindowImpl(Window.Impl);
   end ResetObjectImplementation;
   ---------------------------------------------------------------------------

end GUI.Window;
