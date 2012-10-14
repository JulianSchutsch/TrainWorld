pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GUIBounds; use GUIBounds;

package GUIImplInterface_Window is

   type BorderStyle_Enum is
     (BorderStyleNone,
      BorderStyleResizeable,
      BorderStyleFixed);

   type Button_Enum is
     (ButtonClose,
      ButtonMinimize,
      ButtonMaximize);
   type Button_Set is array(Button_Enum) of Boolean;

   type WindowImpl_Interface is interface;
   type WindowImpl_ClassAccess is access all WindowImpl_Interface'Class;

   procedure Draw
     (Window : in out WindowImpl_Interface) is abstract;

   procedure SetBounds
     (Window : in out WindowImpl_Interface;
      Bounds : GUIBounds.Bounds_Type) is abstract;

   procedure SetTitle
     (Window : in out WindowImpl_Interface;
      Title  : Unbounded_String) is abstract;

   procedure SetBorderStyle
     (Window      : in out WindowImpl_Interface;
      BorderStyle : BorderStyle_Enum) is abstract;

   procedure SetSelected
     (Window   : in out WindowImpl_Interface;
      Selected : Boolean) is abstract;

   procedure SetButtons
     (Window  : in out WindowImpl_Interface;
      Buttons : Button_Set) is abstract;

end GUIImplInterface_Window;
