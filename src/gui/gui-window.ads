pragma Ada_2012;

with GUIImplInterface_Window; use GUIImplInterface_Window;

package GUI.Window is

   subtype Window_Inherited is GUIObject_Type;
   type Window_Type is new Window_Inherited with private;

   overriding
   procedure SetObjectImplementation
     (Window               : in out Window_Type;
      ObjectImplementation : GUIObjectImplementation_Ref);

   overriding
   procedure ResetObjectImplementation
     (Window               : in out Window_Type;
      ObjectImplementation : GUIObjectImplementation_Ref);

   overriding
   procedure Paint
     (Window : in out Window_Type);

   overriding
   procedure SetBounds
     (Window : in out Window_Type;
      Bounds : Bounds_Type);


private

   type Window_Type is new GUI.GUIObject_Type with
      record
         Impl : WindowImpl_ClassAccess:=null;
      end record;

end GUI.Window;
