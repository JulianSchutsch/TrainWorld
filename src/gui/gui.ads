pragma Ada_2005;

--with GUIBounds;
with Graphics;
with RefCount;
with Config;

package GUI is

   type GUIObjectImplementations_Interface is abstract new RefCount.Ref_Interface with null record;
   type GUIObjectImplementations_ClassAccess is access all GUIObjectImplementations_Interface'Class;

   package GUIObjectImplementationsRef is new RefCount.Ref(GUIObjectImplementations_Interface,GUIObjectImplementations_ClassAccess);

   subtype GUIObjectImplementations_Ref is GUIObjectImplementationsRef.Ref_Type;

   type GUI_Interface is abstract new RefCount.Ref_Interface with null record;
   type GUI_ClassAccess is access all GUI_Interface'Class;

   not overriding
   procedure SetContext
     (GUI     : in out GUI_Interface;
      Context : Graphics.Context_Ref) is abstract;

   not overriding
   procedure SetObjectImplementations
     (GUI           : in out GUI_Interface;
      Configuration : Config.ConfigNode_Type) is abstract;

   ---------------------------------------------------------------------------

   package GUIRef is new RefCount.Ref(GUI_Interface,GUI_ClassAccess);

   subtype GUI_Ref is GUIRef.Ref_Type;

   type GUI_Type is new GUI_Interface with private;

   overriding
   procedure SetContext
     (GUI     : in out GUI_Type;
      Context : Graphics.Context_Ref);

   overriding
   procedure SetObjectImplementations
     (GUI           : in out GUI_Type;
      Configuration : Config.ConfigNode_Type);

private

   type GUI_Type is new GUI_Interface with
      record
         Context               : Graphics.Context_Ref;
         ObjectImplementations : GUIObjectImplementations_Ref;
      end record;

end GUI;
