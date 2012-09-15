pragma Ada_2005;

with Ada.Finalization;

package GUI is

   type GUIObject_Type is new Ada.Finalization.Limited_Controlled with private;
   type GUIObject_ClassAccess is access GUIObject_Type'Class;

   type GUI_Public is new Ada.Finalization.Limited_Controlled with
      record
         BackgroundLayer : GUIObject_Type;
         WindowLayer     : GUIObject_Type;
         TopWindowLayer  : GUIObject_Type;
         ModalLayer      : GUIObject_Type;
         ContextLayer    : GUIObject_Type;
      end record;

   type GUI_Type is new GUI_Public with private;

   procedure Dummy;

private

   type GUIObject_Type is new Ada.Finalization.Limited_Controlled with
      record
         FirstChild  : GUIObject_ClassAccess := null;
         LastChild   : GUIObject_ClassAccess := null;
         Parent      : GUIObject_ClassAccess := null;
         NextSibling : GUIObject_ClassAccess := null;
         LastSibling : GUIObject_ClassAccess := null;
      end record;

   type GUI_Type is new GUI_Public with
      record
         Root : GUIObject_ClassAccess := null;
      end record;

end GUI;
