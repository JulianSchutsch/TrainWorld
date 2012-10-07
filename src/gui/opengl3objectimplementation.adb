pragma Ada_2012;

with GUI;
with Config;
with Graphics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;

package body OpenGL3ObjectImplementation is

   type OpenGL3OI_Type is new GUI.GUIObjectImplementation_Interface with
      record
         null;
      end record;
   type openGL3OI_Access is access all OpeNGL3OI_Type'Class;

   function Constructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess;
      Parameter  : Graphics.Context_Info)
      return GUI.GUIObjectImplementation_Ref is

      pragma Unreferenced(GenConfig,ImplConfig,Parameter);

      NewOpenGL3OI : constant OpenGL3OI_Access:=new OpeNGL3OI_Type;

   begin
      return GUI.GUIObjectImplementationRef.MakeInitialRef(GUI.GUIObjectImplementation_ClassAccess(NewOpeNGL3OI));
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
