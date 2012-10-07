pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body GUI is

   procedure ContextCreate
     (T : in out GUIGraphicsCallBack_Type) is
   begin
      null;
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
      GUI.Context:=Context;
      GUI.GraphicsCallBack.GUI:=GUI'Unrestricted_Access;
      -- TODO:Reinitialize current Objects Implementation
      GUI.ThemeConfig:=Theme;
      if Context.I.IsInitialized then
         Put_Line("ObjectIMpl");
         GUI.ObjectImplementations := ObjectImplementations.Utilize(Theme,Context.I.GetInfo);
         Put_Line("ObjectImpl/");
         -- TODO: Catch exception
      end if;

      Context.I.CallBack:=GUI.GraphicsCallBack'Unrestricted_Access;

   end Setup;
   ---------------------------------------------------------------------------

end GUI;
