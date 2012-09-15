pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with RefCount;
with Implementations;
with Config;
with Basics; use Basics;

package Graphics is

   FailedContextCreation : Exception;
   InvalidContext        : Exception;

   type MouseButton_Enum is
     (MouseButtonLeft,
      MouseButtonRight);

   type MouseButton_Array is array(MouseButton_Enum) of Boolean;

   NoMouseButtons : constant Mousebutton_Array:=(others => False);

   type WindowType_Enum is
     (WindowTypeWindow,
      WindowTypeFullDesktopWindow,
      WindowTypeFullScreen);

   type Context_OnClose is
     access procedure
       (Data : C_ClassAccess);

   type Context_OnResize is
     access procedure
       (Data   : C_ClassAccess;
        Height : Integer;
        Width  : Integer);

   type Context_OnPaint is
     access procedure
       (Data : C_ClassAccess);

   type Context_Interface is new RefCount.Ref_Interface with
      record
         Data     : C_ClassAccess    := null;
         OnClose  : Context_OnClose  := null;
         OnResize : Context_OnResize := null;
         OnPaint  : Context_OnPaint  := null;
      end record;
   type Context_ClassAccess is access all Context_Interface'Class;

   type Context_Info is
      record
         InterfaceType : Unbounded_String;
         VersionMajor : Natural;
         VersionMinor : Natural;
         VersionPatch : Natural;
      end record;

   package Ref is new RefCount.Ref(Context_Interface,Context_ClassAccess);

   subtype Context_Ref is Ref.Ref_Type;

   type Context_Config is new Config.Config_Type with
      record
         WindowType             : WindowType_Enum  := WindowTypeWindow;
         Height                 : Natural:=1024;
         Width                  : Natural:=768;
         RedBits                : Natural:=8;
         GreenBits              : Natural:=8;
         BlueBits               : Natural:=8;
         AlphaBits              : Natural:=8;
         DepthBits              : Natural:=32;
         StencilBits            : Natural:=0;
         WindowTitle            : Unbounded_String:=U("Win");
         ApplicationTitle       : Unbounded_String:=U("App");
      end record;

   package Implementations is new Standard.Implementations(Context_Ref);

   procedure CreateConfig
     (Configuration    : in out Config.ConfigNode_Type;
      WindowType       : WindowType_Enum:=WindowTypeWindow;
      Height           : Natural:=1024;
      Width            : Natural:=768;
      RedBits          : Natural:=8;
      GreenBits        : Natural:=8;
      BlueBits         : Natural:=8;
      AlphaBits        : Natural:=8;
      DepthBits        : Natural:=32;
      StencilBits      : Natural:=0;
      WindowTitle      : Unbounded_String:=U("Win");
      ApplicationTitle : Unbounded_String:=U("App"));

end Graphics;
