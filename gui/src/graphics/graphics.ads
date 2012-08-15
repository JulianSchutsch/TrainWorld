pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with RefCount;
with Implementations;

package Graphics is

   type WindowType_Enum is
     (WindowType_Dummy,
      WindowType_Resizeable,
      WindowType_Fullscreen);

   type Context_Interface is new RefCount.Ref_Interface with null record;

   type Context_Info is
      record
         InterfaceType : Unbounded_String;
         VersionMajor : Natural;
         VersionMinor : Natural;
         VersionPatch : Natural;
      end record;

   package Ref is new RefCount.Ref(Context_Interface);

   subtype Context_Ref is Ref.Ref_Type;

   type Context_Config is
      record
         Height                 : Natural:=768;
         Width                  : Natural:=1024;
         FullScreen             : Boolean:=False;
         ChangeScreenResolution : Boolean:=False;
      end record;

   package Implementations is new Standard.Implementations(Context_Ref);

   procedure dummy;

end Graphics;
