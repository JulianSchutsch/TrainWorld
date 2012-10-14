pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body GUI is

   procedure TreeSetObjectImplementation
     (Object               : in out GUIObject_Type;
      ObjectImplementation : GUIObjectImplementation_Ref) is

      p : GUIObject_ClassAccess:=Object'Unrestricted_Access;

   begin

      while p/=null loop

         p.SetObjectImplementation(ObjectImplementation);
         if p.FirstChild/=null then
            p:=p.FirstChild;
         else
            while (p/=null) and then (p.Next=null) loop
               p:=p.Parent;
            end loop;
            if p/=null then
               p:=p.Next;
            end if;
         end if;

      end loop;

   end TreeSetObjectImplementation;
   ---------------------------------------------------------------------------

   procedure TreeResetObjectImplementation
     (Object               : in out GUIObject_Type;
      ObjectImplementation : GUIObjectImplementation_Ref) is

      p : GUIObject_ClassAccess:=Object'Unrestricted_Access;

   begin

      while p/=null loop

         p.ResetObjectImplementation(ObjectImplementation);
         if p.FirstChild/=null then
            p:=p.FirstChild;
         else
            while (p/=null) and then (p.Next=null) loop
               p:=p.Parent;
            end loop;
            if p/=null then
               p:=p.Next;
            end if;
         end if;

      end loop;

   end TreeResetObjectImplementation;
   ---------------------------------------------------------------------------

   procedure TreeSetGUI
     (Object : in out GUIObject_Type;
      GUI    : GUI_Ref) is

      p : GUIObject_ClassAccess:=Object'Unrestricted_Access;

   begin

      while p/=null loop

         p.GUI:=GUI;
         if p.FirstChild/=null then
            p:=p.FirstChild;
         else
            while (p/=null) and then (p.Next=null) loop
               p:=p.Parent;
            end loop;
            if p/=null then
               p:=p.Next;
            end if;
         end if;

      end loop;

   end TreeSetGUI;
   ---------------------------------------------------------------------------

   procedure SetParent
     (Object : in out GUIObject_Type;
      Parent : access GUIObject_Type'Class) is

      OldGUI : GUI_Ref;
      NewGUI : GUI_Ref;

   begin

      if Object.Parent=Parent then
         return;
      end if;

      if Object.Parent/=null then
         OldGUI:=Object.GUI;
      end if;
      if Parent/=null then
         NewGUI:=Parent.GUI;
      end if;

      -- Disconnect from current parent
      if Object.Previous/=null then
         Object.Previous.Next:=Object.Next;
      else
         if Object.Parent/=null then
            Object.Parent.FirstChild:=Object.Next;
         end if;
      end if;
      if Object.Next/=null then
         Object.Next.Previous:=Object.Previous;
      else
         if Object.Parent/=null then
            Object.Parent.LastChild:=Object.Previous;
         end if;
      end if;

      -- Connect to new parent
      Object.Parent := GUIObject_ClassAccess(Parent);
      if Parent/=null then
         Object.Next:=Parent.FirstChild;
         if Object.Next/=null then
            Object.Next.Previous:=Object'Unrestricted_Access;
         else
            Parent.LastChild:=Object'Unrestricted_Access;
         end if;
      else
         Object.Next     := null;
         Object.Previous := null;
      end if;

      if OldGUI.I/=NewGUI.I then
         -- The ObjectImplementations stay valid as long as OldGUI and NewGUI are in scope
         TreeResetObjectImplementation(Object,GUI_Access(OldGUI.I).ObjectImplementations);
         TreeSetGUI(Object,NewGUI);
         TreeSetObjectImplementation(Object,GUI_Access(NewGUI.I).ObjectImplementations);
      end if;

   end SetParent;
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

   procedure CheckCreatePending
     (T : in out GUI_Type) is
   begin
      if T.CreatePending and T.CallBack/=null then
         T.CallBack.GUICreate;
         T.CreatePending:=False;
      end if;
   end CheckCreatePending;
   ---------------------------------------------------------------------------

   procedure ContextCreate
     (T : in out GUIGraphicsCallBack_Type) is

      GUI : GUI_Type renames T.GUI.all;

   begin
      CheckCreatePending(GUI);
      GUI.ObjectImplementations := ObjectImplementations.Utilize(GUI.ThemeConfig,GUI.Context.I.GetInfo);
      -- TODO: Catch exception and report the failure to bind a theme to the callback
      -- TODO: Run ObjectsImpl initialize
   end ContextCreate;
   ---------------------------------------------------------------------------

   procedure ContextClose
     (T : in out GUIGraphicsCallBack_Type) is

      GUI : GUI_Type renames T.GUI.all;

   begin
      CheckCreatePending(GUI);
      Put_Line("ContextClose");
      -- TODO: Cleanup current Objects Implementation
      if GUI.CallBack/=null then
         GUI.CallBack.GUIClose;
      end if;
   end ContextClose;
   ---------------------------------------------------------------------------

   procedure ContextPaint
     (T : in out GUIGraphicsCallBack_Type) is

      GUI : GUI_Type renames T.GUI.all;

   begin
      GUI.ObjectImplementations.I.StartPainting;
      GUI.ObjectImplementations.I.StopPainting;
   end ContextPaint;
   ---------------------------------------------------------------------------

   procedure ContextResize
     (T      : in out GUIGraphicsCallBack_Type;
      Height : Natural;
      Width  : Natural) is

      pragma Unreferenced(Height,Width);

      GUI : GUI_Type renames T.GUI.all;
   begin
      CheckCreatePending(GUI);
   end ContextResize;
   ---------------------------------------------------------------------------

   function GetBaseLayer
     (GUI : in out GUI_Type)
      return GUIObject_ClassAccess is
   begin
      return GUI.BaseLayer'Unrestricted_Access;
   end GetBaseLayer;
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
