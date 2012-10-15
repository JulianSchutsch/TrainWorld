pragma Ada_2012;

with Textures;
with System;
with Ada.Text_IO; use Ada.Text_IO;
with Basics; use Basics;

package body OpenGL3ObjectImplementation.Window is

   procedure Setup
     (Data          : in out WindowImpl_Data;
      Impl          : access OGL3Impl_Type'Class;
      Configuration : OGL3ImplConfig_Type) is
      pragma Unreferenced(Configuration);

      Tex  : Textures.RGBATexture_Type;
      Addr : System.Address;

   begin

      Put_Line("Setup called");
      -- DANGER : Texture size must be correct, or otherwise it is possible to have a bufferoverflow in copytoraw

      Impl.TexBuffers.AllocateConst
        (Amount      => 20*20,
         BufferRange => Data.TopLeftTex'Access);

      Tex.Create
        (Height => 20,
         Width  => 20);
      Tex.Clear((Red => 0,Green=>0,Blue=>255,Alpha=>255));
      Addr:=Data.TopLeftTex.Map;
      Tex.CopyToRawData(Addr);
      Data.TopLeftTex.Unmap;
      Data.TopLeftTex.AssocHeight := 20;
      Data.TopLeftTex.AssocWidth  := 20;

   end Setup;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Data : in out WindowImpl_Data) is
   begin
      OpenGL.LinearBuffer.LinearRange_Interface'Class(Data.TopLeftTex).Finalize; -- TODO: Check if necessary
   end Finalize;
   ---------------------------------------------------------------------------

   type WindowImpl_Access is access all WindowImpl_Type;

   function Create
     (Impl : OGL3Impl_ClassAccess;
      Data : WindowImpl_DataAccess)
      return GUIImplInterface_Window.WindowImpl_ClassAccess is

      WindowImpl : constant WindowImpl_Access:=new WindowImpl_Type;

   begin

      WindowImpl.Impl := Impl;
      WindowImpl.Data := Data;
      return GUIImplInterface_Window.WindowImpl_ClassAccess(WindowImpl);

   end Create;
   ---------------------------------------------------------------------------

   procedure Destroy
     (Impl : WindowImpl_ClassAccess) is
   begin
      null; -- TODO: Free record
   end Destroy;
   ---------------------------------------------------------------------------

   procedure Paint
     (Window : in out WindowImpl_Type) is
   begin
      Window.Impl.PushRect
        (BufferRange => Window.Data.TopLeftTex'Access,
         Vertx       => Window.Bounds.Left,
         Verty       => Window.Bounds.Top,
         VertHeight  => Window.Bounds.Height,
         VertWidth   => Window.Bounds.Width,
         Texx        => 0,
         Texy        => 0,
         TexHeight   => 20,
         TexWidth    => 20);
   end Paint;
   ---------------------------------------------------------------------------

   procedure SetBounds
     (Window : in out WindowImpl_Type;
      Bounds : GUIBounds.Bounds_Type) is
   begin
      Window.Bounds:=Bounds;
   end SetBounds;
   ---------------------------------------------------------------------------

   procedure SetTitle
     (Window : in out WindowImpl_Type;
      Title  : Unbounded_String) is
   begin
      null;
   end SetTitle;
   ---------------------------------------------------------------------------

   procedure SetBorderStyle
     (Window      : in out WindowImpl_Type;
      BorderStyle : BorderStyle_Enum) is
   begin
      null;
   end SetBorderStyle;
   ---------------------------------------------------------------------------

   procedure SetSelected
     (Window   : in out WindowImpl_Type;
      Selected : Boolean) is
   begin
      null;
   end SetSelected;
   ---------------------------------------------------------------------------

   procedure SetButtons
     (Window  : in out WindowImpl_Type;
      Buttons : Button_Set) is
   begin
      null;
   end SetButtons;
   ---------------------------------------------------------------------------

end OpenGL3ObjectImplementation.Window;
