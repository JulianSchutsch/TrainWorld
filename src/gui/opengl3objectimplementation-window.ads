pragma Ada_2012;

with GUIImplInterface_Window; use GUIImplInterface_Window;
with GUIBounds; use GUIBounds;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;

private package OpenGL3ObjectImplementation.Window is

   type WindowImpl_Data is new Ada.Finalization.Limited_Controlled with private;

   not overriding
   procedure Setup
     (Data          : in out WindowImpl_Data;
      Impl          : access OGL3Impl_Type'Class;
      Configuration : OGL3ImplConfig_Type);

   overriding
   procedure Finalize
     (Data : in out WindowImpl_Data);
   ---------------------------------------------------------------------------

   type WindowImpl_Type is new GUIImplInterface_Window.WindowImpl_Interface with private;
   type WindowImpl_DataAccess is access all WindowImpl_Data;

   overriding
   procedure Paint
     (Window : in out WindowImpl_Type);

   overriding
   procedure SetBounds
     (Window : in out WindowImpl_Type;
      Bounds : GUIBounds.Bounds_Type);

   overriding
   procedure SetTitle
     (Window : in out WindowImpl_Type;
      Title  : Unbounded_String);

   overriding
   procedure SetBorderStyle
     (Window      : in out WindowImpl_Type;
      BorderStyle : BorderStyle_Enum);

   overriding
   procedure SetSelected
     (Window   : in out WindowImpl_Type;
      Selected : Boolean);

   overriding
   procedure SetButtons
     (Window  : in out WindowImpl_Type;
      Buttons : Button_Set);
   ---------------------------------------------------------------------------

   function Create
     (Impl : OGL3Impl_ClassAccess;
      Data : WindowImpl_DataAccess)
      return GUIImplInterface_Window.WindowImpl_ClassAccess;

   procedure Destroy
     (Impl : WindowImpl_ClassAccess);

private

   type WindowImpl_Data is new Ada.Finalization.Limited_Controlled with
      record
         TopLeftTex : aliased OpenGL.LinearBuffer.LinearRange_Type;
      end record;

   type WindowImpl_Type is new GUIImplInterface_Window.WindowImpl_Interface with
      record
         Impl   : OGL3Impl_ClassAccess  := null;
         Data   : WindowImpl_DataAccess := null;
         Bounds : Bounds_Type;
      end record;

end OpenGL3ObjectImplementation.Window;
