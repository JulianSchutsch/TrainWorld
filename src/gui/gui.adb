pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body GUI is

   procedure TreePrint
     (Object : in GUIObject_Type'Class;
      Left   : String) is

      p : GUIObject_ClassAccess:=Object.FirstChild;

   begin

      while p/=null loop

         Put_Line(Left&"Object");
         if p.Next/=null then
            TreePrint(p.all,Left&" | ");
         else
            TreePrint(p.all,Left&"   ");
         end if;
         p:=p.Next;

      end loop;

   end TreePrint;
   ---------------------------------------------------------------------------

   procedure PrintTree
     (GUI : GUI_Type) is
   begin
      Put_Line("GUI Tree:");
      TreePrint(GUI.BaseLayer,"");
      TreePrint(GUI.FrontLayer,"");
      TreePrint(GUI.ModalLayer,"");
      TreePrint(GUI.ContextLayer,"");
   end PrintTree;
   ---------------------------------------------------------------------------

   procedure TreePaint
     (Object : in out GUIObject_Type) is
      p : GUIObject_ClassAccess:=Object'Unrestricted_Access;

   begin

      while p/=null loop

         p.Paint;
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

   end TreePaint;
   ---------------------------------------------------------------------------

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
         Parent.FirstChild:=Object'Unrestricted_Access;
      else
         Object.Next     := null;
         Object.Previous := null;
      end if;

      if OldGUI.I/=NewGUI.I then
         -- The ObjectImplementations stay valid as long as OldGUI and NewGUI are in scope
         -- Cast is always correct since there is no other GUI type
         TreeResetObjectImplementation(Object,GUI_Access(OldGUI.I).ObjectImplementations);
         TreeSetGUI(Object,NewGUI);
         TreeSetObjectImplementation(Object,GUI_Access(NewGUI.I).ObjectImplementations);
      end if;

   end SetParent;
   ---------------------------------------------------------------------------

   -- This method is used to reset the current ObjectImplementations
   procedure ResetObjectImplementations
     (GUI : in out GUI_Type) is
   begin

      if GUI.ObjectImplementations.I/=null then
         TreeResetObjectImplementation(GUI.BaseLayer,GUI.ObjectImplementations);
         TreeResetObjectImplementation(GUI.FrontLayer,GUI.ObjectImplementations);
         TreeResetObjectImplementation(GUI.ModalLayer,GUI.ObjectImplementations);
         TreeResetObjectImplementation(GUI.ContextLayer,GUI.ObjectImplementations);
         Put_Line("Set Null ObjectImplementations");
         GUI.ObjectImplementations.SetNull;
         Put_Line("///////");
      end if;

   end ResetObjectImplementations;
   ---------------------------------------------------------------------------

   -- This method is used to apply the current (new) ObjectImplementation to
   -- all objects associated with this GUI.
   procedure SetObjectImplementations
     (GUI : in out GUI_Type) is
   begin

      TreeSetObjectImplementation(GUI.BaseLayer,GUI.ObjectImplementations);
      TreeSetObjectImplementation(GUI.FrontLayer,GUI.ObjectImplementations);
      TreeSetObjectImplementation(GUI.ModalLayer,GUI.ObjectImplementations);
      TreeSetObjectImplementation(GUI.ContextLayer,GUI.ObjectImplementations);

   end SetObjectImplementations;
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
      Put_Line("ContextCreate");
      CheckCreatePending(GUI);
      GUI.ObjectImplementations := ObjectImplementations.Utilize(GUI.ThemeConfig,GUI.Context.I.GetInfo);
      -- TODO: Catch exception and report the failure to bind a theme to the callback
      Put_Line("ContextCreate.2");
      SetObjectImplementations(GUI);
      -- TODO: Call resize
   end ContextCreate;
   ---------------------------------------------------------------------------

   procedure ContextClose
     (T : in out GUIGraphicsCallBack_Type) is

      GUI : GUI_Type renames T.GUI.all;

   begin
      Put_Line("ContextClose.1");
      CheckCreatePending(GUI);
      Put_Line("ContextClose");
      ResetObjectImplementations(GUI);
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

      if GUI.ObjectImplementations.I/=null then
         GUI.ObjectImplementations.I.StartPainting;
         TreePaint(GUI.BaseLayer);
         TreePaint(GUI.FrontLayer);
         TreePaint(GUI.ModalLayer);
         TreePaint(GUI.ContextLayer);
         GUI.ObjectImplementations.I.StopPainting;
      end if;

   end ContextPaint;
   ---------------------------------------------------------------------------

   procedure ContextResize
     (T      : in out GUIGraphicsCallBack_Type;
      Height : Natural;
      Width  : Natural) is

      GUI : GUI_Type renames T.GUI.all;

   begin
      if GUI.ObjectImplementations.I/=null then
         GUI.ObjectImplementations.I.Resize
           (Height => Height,
            Width  => Width);
      end if;
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

      Put_Line("Setup.1");
      ResetObjectImplementations(GUI);
      -- Set new context
      Put_Line("Setup.2");
      GUI.Context:=Context;
      Put_Line("Setup.3");
      Context.I.CallBack:=GUI.GraphicsCallBack'Unrestricted_Access;
      Put_Line("Setup.4");
      GUI.GraphicsCallBack.GUI:=GUI'Unrestricted_Access;
      -- TODO:Reinitialize current Objects Implementation
      GUI.ThemeConfig:=Theme;
      if Context.I.IsInitialized then
         -- TODO:Cleanup current Objects Implementation
         Put_Line("::::");
         GUI.GraphicsCallBack.ContextCreate;
         Put_Line("....//");
      end if;
      Put_Line("//Setup");

   end Setup;
   ---------------------------------------------------------------------------

   procedure Finalize
     (GUI : in out GUI_Type) is
   begin
      Put_Line("Finalize GUI");
      -- Ensure correct order of finalization
      ResetObjectImplementations(GUI);
      Put_Line("Set Null Context");
      GUI.Context.SetNull;
   end Finalize;
   ---------------------------------------------------------------------------

end GUI;
