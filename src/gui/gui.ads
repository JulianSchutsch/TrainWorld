pragma Ada_2005;

--with GUIBounds;
with Graphics;
with RefCount;
with Config;
with Implementations;

package GUI is

   type GUIObjectImplementation_Interface is abstract new RefCount.Ref_Interface with null record;
   type GUIObjectImplementation_ClassAccess is access all GUIObjectImplementation_Interface'Class;

   package GUIObjectImplementationRef is new RefCount.Ref(GUIObjectImplementation_Interface,GUIObjectImplementation_ClassAccess);

   subtype GUIObjectImplementation_Ref is GUIObjectImplementationRef.Ref_Type;

   type GUICallBack_Interface is interface;
   type GUICallBack_ClassAccess is access all GUICallBack_Interface'Class;

   procedure GUIClose
     (T : in out GUICallBack_Interface) is null;

   type GUI_Interface is abstract new RefCount.Ref_Interface with
      record
         CallBack : GUICallBack_ClassAccess:=null;
      end record;
   type GUI_ClassAccess is access all GUI_Interface'Class;

   not overriding
   procedure Setup
     (GUI     : in out GUI_Interface;
      Context : Graphics.Context_Ref;
      Theme   : Config.ConfigNode_Type) is abstract;
   ---------------------------------------------------------------------------

   package ObjectImplementations is new Implementations(GUIObjectImplementation_Ref,Graphics.Context_Info);

   ---------------------------------------------------------------------------

   package GUIRef is new RefCount.Ref(GUI_Interface,GUI_ClassAccess);

   subtype GUI_Ref is GUIRef.Ref_Type;

   type GUI_Type is new GUI_Interface with private;

   overriding
   procedure Setup
     (GUI     : in out GUI_Type;
      Context : Graphics.Context_Ref;
      Theme   : Config.ConfigNode_Type);
   ---------------------------------------------------------------------------

private

   type GUI_Access is access all GUI_Type;

   type GUIGraphicsCallBack_Type is new Graphics.ContextCallBack_Interface with
      record
         GUI : GUI_Access:=null;
      end record;

   overriding
   procedure ContextCreate
     (T : in out GUIGraphicsCallBack_Type);

   overriding
   procedure ContextClose
     (T : in out GUIGraphicsCallBack_Type);

   overriding
   procedure ContextPaint
     (T : in out GUIGraphicsCallBack_Type);

   overriding
   procedure ContextResize
     (T      : in out GUIGraphicsCallBack_Type;
      Height : Natural;
      Width  : Natural);
   ---------------------------------------------------------------------------

   type GUI_Type is new GUI_Interface with
      record
         Context               : Graphics.Context_Ref;
         ObjectImplementations : GUIObjectImplementation_Ref;
         GraphicsCallBack      : aliased GUIGraphicsCallBack_Type;
         ThemeConfig           : Config.ConfigNode_Type;
      end record;

end GUI;
