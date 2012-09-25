pragma Ada_2012;

with RefCount; use RefCount;
with Ada.Finalization;

package OpenGL.Program is

   FailedToCreateShader : Exception;
   FailedToCreateProgram : Exception;

   type Shader_Enum is
     (ShaderVertex,
      ShaderFragment);

   type Shader_Interface is abstract new Ref_Interface with null record;
   type Shader_ClassAccess is access all Shader_Interface'Class;

   not overriding
   procedure Create
     (Shader     : in out Shader_Interface;
      ShaderType : Shader_Enum;
      Source     : String) is abstract;

   not overriding
   procedure Reset
     (Shader : in out Shader_Interface) is abstract;

   not overriding
   function GetCompileLog
     (Shader : Shader_Interface)
      return Unbounded_String is abstract;
   ---------------------------------------------------------------------------

   package Ref is new RefCount.Ref(Shader_Interface,Shader_ClassAccess);

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

   not overriding
   procedure BindAttribLocation
     (Program : Program_Type;
      Index   : GLuint_Type;
      Name    : String);

   not overriding
   function GetUniformLocation
     (Program : Program_Type;
      Name    : String)
      return GLint_Type;

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
