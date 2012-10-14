pragma Ada_2012;

with GUIImplInterface_Window;

package GUI.Window is

   type Window_Type is new GUIObject_Type with private;

   overriding
   procedure SetObjectImplementation
     (Window               : in out Window_Type;
      ObjectImplementation : GUIObjectImplementation_Ref);

   overriding
   procedure ResetObjectImplementation
     (Window               : in out Window_Type;
      ObjectImplementation : GUIObjectImplementation_Ref);

private

   type Window_Type is new GUI.GUIObject_Type with
      record
         Impl : GUIImplInterface_Window.WindowImpl_ClassAccess:=null;
      end record;

end GUI.Window;
