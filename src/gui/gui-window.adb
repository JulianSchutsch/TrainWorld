package body GUI.Window is

   procedure Paint
     (Window : in out Window_Type) is
   begin
      Window.Impl.Paint;
   end Paint;
   ---------------------------------------------------------------------------

   procedure SetBounds
     (Window : in out Window_Type;
      Bounds : Bounds_Type) is
   begin

      Window_Inherited(Window).SetBounds(Bounds);
      if Window.Impl/=null then
         Window.Impl.SetBounds(Bounds);
      end if;

   end SetBounds;

   procedure SetObjectImplementation
     (Window               : in out Window_Type;
      ObjectImplementation : GUIObjectImplementation_Ref) is
   begin
      Window.Impl:=ObjectImplementation.I.CreateWindowImpl;
      Window.Impl.SetBounds(Window.Bounds);
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
