pragma Ada_2012;

with RefCount; use RefCount;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with Ada.Finalization;

package OpenGL.Program is

   FailedToCreateShader : Exception;
   FailedToCreateProgram : Exception;

   type Shader_Enum is
     (ShaderVertex,
      ShaderFragment);

   type Shader_Interface is new Ref_Interface with null record;

   not overriding
   procedure Create
     (Shader     : in out Shader_Interface;
      ShaderType : Shader_Enum;
      Source     : String) is null;

   not overriding
   procedure Reset
     (Shader : in out Shader_Interface) is null;

   not overriding
   function GetCompileLog
     (Shader : Shader_Interface)
      return Unbounded_String is (U(""));
   ---------------------------------------------------------------------------

   package Ref is new RefCount.Ref(Shader_Interface);

--   subtype Shader_Ref is Ref.Ref_Type;

   type Shader_Set is array(Shader_Enum) of Ref.Ref_Type;

   type Shader_Type is new Shader_Interface with private;

   overriding
   procedure Create
     (Shader     : in out Shader_Type;
      ShaderType : Shader_Enum;
      Source     : String);

   overriding
   procedure Reset
     (Shader : in out Shader_Type);

   overriding
   function GetCompileLog
     (Shader : Shader_Type)
      return Unbounded_String;

   overriding
   procedure Finalize
     (Shader : in out Shader_Type);
   ---------------------------------------------------------------------------

   type Program_Type is new Ada.Finalization.Controlled with private;

   not overriding
   procedure Create
     (Program : in out Program_Type;
      Shaders : Shader_Set);

   not overriding
   procedure Reset
     (Program : in out Program_Type);

   not overriding
   procedure UseProgram
     (Program : in out Program_Type);

   not overriding
   function GetLinkLog
     (Program : Program_Type)
      return Unbounded_String;

   overriding
   procedure Finalize
     (Program : in out Program_Type);

private

   type Shader_Type is new Shader_Interface with
      record
         ID         : GLuint_Type:=0; -- 0 means no shader created
         ShaderType : Shader_Enum;
         CompileLog : Unbounded_String;
      end record;

   type Program_Type is new Ada.Finalization.Controlled with
      record
         ID      : GLuint_Type:=0;
         Shaders : Shader_Set;
         LinkLog : Unbounded_String;
      end record;

end OpenGL.Program;
