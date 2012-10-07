pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body GUI is

   procedure SetParent
     (Object : in out GUIObject_Type;
      Parent : access GUIObject_Type'Class) is
   begin
      null;
   end SetParent;
   ---------------------------------------------------------------------------

   procedure SetObjectImplementation
     (Object               : in out GUIObject_Type;
      ObjectImplementation : GUIObjectImplementation_Ref) is
   begin
      null;
   end SetObjectImplementation;
   ---------------------------------------------------------------------------

   procedure ResetObjectImplementation
     (Object : in out GUIObject_Type) is
   begin
      null;
   end ResetObjectImplementation;
   ---------------------------------------------------------------------------

   function GetBounds
     (Object : GUIObject_Type)
      return Bounds_Type is
   begin
      return Object.Bounds;
   end GetBounds;
   ---------------------------------------------------------------------------

   procedure SetBounds
     (Object : in out GUIObject_Type;
      Bounds : Bounds_Type) is
   begin
      Object.Bounds:=Bounds;
      -- TODO: Update, call resize etc
   end SetBounds;
   ---------------------------------------------------------------------------

   procedure ContextCreate
     (T : in out GUIGraphicsCallBack_Type) is

      GUI : GUI_Type renames T.GUI.all;

   begin
      GUI.ObjectImplementations := ObjectImplementations.Utilize(GUI.ThemeConfig,GUI.Context.I.GetInfo);
      -- TODO: Catch exception and report the failure to bind a theme to the callback
      -- TODO: Run ObjectsImpl initialize
   end ContextCreate;
   ---------------------------------------------------------------------------

   procedure ContextClose
     (T : in out GUIGraphicsCallBack_Type) is
   begin
      Put_Line("ContextClose");
      -- TODO: Cleanup current Objects Implementation
      if T.GUI.CallBack/=null then
         T.GUI.CallBack.GUIClose;
      end if;
   end ContextClose;
   ---------------------------------------------------------------------------

   procedure ContextPaint
     (T : in out GUIGraphicsCallBack_Type) is
   begin
      null;
   end ContextPaint;
   ---------------------------------------------------------------------------

   procedure ContextResize
     (T      : in out GUIGraphicsCallBack_Type;
      Height : Natural;
      Width  : Natural) is
   begin
      null;
   end ContextResize;
   ---------------------------------------------------------------------------

   procedure Setup
     (GUI     : in out GUI_Type;
      Context : Graphics.Context_Ref;
      Theme   : Config.ConfigNode_Type) is
   begin
      -- TODO:Cleanup current Objects Implementation
      GUI.ObjectImplementations.SetNull;
      -- Set new context
      GUI.ObjectImplementations.SetNull;
      GUI.Context:=Context;
      GUI.GraphicsCallBack.GUI:=GUI'Unrestricted_Access;
      -- TODO:Reinitialize current Objects Implementation
      GUI.ThemeConfig:=Theme;
      if Context.I.IsInitialized then
         GUI.GraphicsCallBack.ContextCreate;
      end if;

      Context.I.CallBack:=GUI.GraphicsCallBack'Unrestricted_Access;

   end Setup;
   ---------------------------------------------------------------------------

   procedure Finalize
     (GUI : in out GUI_Type) is
   begin
      -- Ensure correct order of finalization
      GUI.ObjectImplementations.SetNull;
      GUI.Context.SetNull;
   end Finalize;
   ---------------------------------------------------------------------------

end GUI;
