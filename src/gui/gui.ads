pragma Ada_2012;

with GUIBounds; use GUIBounds;
with Graphics;
with RefCount;
with Config;
with Implementations;
with GUIImplInterface_Window;

package GUI is

   type GUIObjectImplementation_Interface is abstract new RefCount.Ref_Interface with null record;
   type GUIObjectImplementation_ClassAccess is access all GUIObjectImplementation_Interface'Class;

   not overriding
   function CreateWindowImpl
     (ObjectImplementation : in out GUIObjectImplementation_Interface)
      return GUIImplInterface_Window.WindowImpl_ClassAccess is abstract;

   not overriding
   procedure DestroyWindowImpl
     (ObjectImplementation : in out GUIObjectImplementation_Interface;
      WindowImpl           : in out GUIImplInterface_Window.WindowImpl_ClassAccess) is abstract;

   not overriding
   procedure StartPainting
     (ObjectImplementation : in out GUIObjectImplementation_Interface) is abstract;

   not overriding
   procedure StopPainting
     (ObjectImplementation : in out GUIObjectImplementation_Interface) is abstract;

   not overriding
   procedure Resize
     (ObjectImplementation : in out GUIObjectImplementation_Interface;
      Height               : Natural;
      Width                : Natural) is abstract;
   ---------------------------------------------------------------------------

   package GUIObjectImplementationRef is new RefCount.Ref(GUIObjectImplementation_Interface,GUIObjectImplementation_ClassAccess);

   subtype GUIObjectImplementation_Ref is GUIObjectImplementationRef.Ref_Type;

   type GUICallBack_Interface is limited interface;
   type GUICallBack_ClassAccess is access all GUICallBack_Interface'Class;

   procedure GUICreate
     (T : in out GUICallBack_Interface) is null;

   procedure GUIClose
     (T : in out GUICallBack_Interface) is null;
   ---------------------------------------------------------------------------

   type GUIObject_Type is new RefCount.Ref_Interface with private;
   type GUIObject_ClassAccess is access all GUIObject_Type'Class;

   not overriding
   function GetBounds
     (Object : GUIObject_Type)
      return Bounds_Type;

   not overriding
   procedure SetBounds
     (Object : in out GUIObject_Type;
      Bounds : Bounds_Type);

   not overriding
   procedure SetParent
     (Object : in out GUIObject_Type;
      Parent : access GUIObject_Type'Class);

   not overriding
   procedure Paint
     (Object : in out GUIObject_Type) is null;

   -- Only called by GUI
   not overriding
   procedure SetObjectImplementation
     (Object               : in out GUIObject_Type;
      ObjectImplementation : GUIObjectImplementation_Ref) is null;

   not overriding
   procedure ResetObjectImplementation
     (Object               : in out GUIObject_Type;
      ObjectImplementation : GUIObjectImplementation_Ref) is null;

   ---------------------------------------------------------------------------

   type GUI_Interface is abstract new RefCount.Ref_Interface with
      record
         CallBack     : GUICallBack_ClassAccess:=null;
      end record;
   type GUI_ClassAccess is access all GUI_Interface'Class;

   not overriding
   procedure Setup
     (GUI     : in out GUI_Interface;
      Context : Graphics.Context_Ref;
      Theme   : Config.ConfigNode_Type) is abstract;

   not overriding
   function GetBaseLayer
     (GUI : in out GUI_Interface)
      return GUIObject_ClassAccess is abstract;

   not overriding
   procedure PrintTree
     (GUI : GUI_Interface) is abstract;
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

   overriding
   function GetBaseLayer
     (GUI : in out GUI_Type)
      return GUIObject_ClassAccess;

   overriding
   procedure Finalize
     (GUI : in out GUI_Type);

   overriding
   procedure PrintTree
     (GUI : GUI_Type);
   ---------------------------------------------------------------------------

private

   type GUIObject_Type is new RefCount.Ref_Interface with
      record
         Bounds     : Bounds_Type;
         Previous   : GUIObject_ClassAccess:=null;
         GUI        : GUI_Ref;
         Next       : GUIObject_ClassAccess:=null;
         Parent     : GUIObject_ClassAccess:=null;
         FirstChild : GUIObject_ClassAccess:=null;
         LastChild  : GUIObject_ClassAccess:=null;
      end record;

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
         ContextLayer          : aliased GUIObject_Type;
         ModalLayer            : aliased GUIObject_Type;
         FrontLayer            : aliased GUIObject_Type;
         BaseLayer             : aliased GUIObject_Type;
         Context               : aliased Graphics.Context_Ref;
         ObjectImplementations : GUIObjectImplementation_Ref;
         GraphicsCallBack      : aliased GUIGraphicsCallBack_Type;
         ThemeConfig           : Config.ConfigNode_Type;
         CreatePending         : Boolean:=True;
      end record;

end GUI;
