pragma Ada_2012;

with Graphics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;
with GUIImplInterface_Window;
with OpenGL3ObjectImplementation.Window;

package body OpenGL3ObjectImplementation is

   type OpenGL3OI_Type is new OGL3Impl_Type with
      record
         WindowData : OpenGL3ObjectImplementation.Window.WindowImpl_Data;
      end record;
   type OpenGL3OI_Access is access all OpeNGL3OI_Type'Class;

   overriding
   function CreateWindowImpl
     (ObjectImplementation : in out OpenGL3OI_Type)
      return GUIImplInterface_Window.WindowImpl_ClassAccess;

   overriding
   procedure DestroyWindowImpl
     (ObjectImplementation : in out OpenGL3OI_Type;
      WindowImpl           : in out GUIImplInterface_Window.WindowImpl_ClassAccess);
   ---------------------------------------------------------------------------

   function CreateWindowImpl
     (ObjectImplementation : in out OpenGL3OI_Type)
      return GUIImplInterface_Window.WindowImpl_ClassAccess is
   begin
      return OpenGL3ObjectImplementation.Window.Create(ObjectImplementation.WindowData);
   end CreateWindowImpl;
   ---------------------------------------------------------------------------

   procedure DestroyWindowImpl
     (ObjectImplementation : in out OpenGL3OI_Type;
      WindowImpl           : in out GUIImplInterface_Window.WindowImpl_ClassAccess) is
   begin
      null;
   end DestroyWindowImpl;
   ---------------------------------------------------------------------------

   function Constructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess;
      Parameter  : Graphics.Context_Info)
      return GUI.GUIObjectImplementation_Ref is

      pragma Unreferenced(GenConfig,Parameter);

      use type Config.Config_ClassAccess;

      NewOpenGL3OI : constant OpenGL3OI_Access:=new OpenGL3OI_Type;
      Config       : aliased OGL3ImplConfig_Type;

   begin
      if ImplConfig/=null then
         Config:=OGL3ImplConfig_Type(ImplConfig.all);
      end if;
      return GUI.GUIObjectImplementationRef.MakeInitialRef(GUI.GUIObjectImplementation_ClassAccess(NewOpenGL3OI));
   end Constructor;
   ---------------------------------------------------------------------------

   function Compatible
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess;
      Parameter  : Graphics.Context_Info)
      return Boolean is
      pragma Unreferenced(GenConfig, ImplConfig);
   begin

      Put_Line("Compatible?");
      Put_Line(To_String(Parameter.InterfaceType));
      Put_Line(Natural'Image(Parameter.VersionMajor));
      return To_String(Parameter.InterfaceType)="OpenGL" and (Parameter.VersionMajor>=3);

   end Compatible;
   ---------------------------------------------------------------------------

   procedure Register is
   begin
      Put_Line("Register");
      GUI.ObjectImplementations.Register(U("OpenGL3 Object Implementation"),Compatible'Access,Constructor'access);
   end Register;
   ---------------------------------------------------------------------------

end OpenGL3ObjectImplementation;
