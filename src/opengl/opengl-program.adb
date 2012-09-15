pragma Ada_2012;

with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Basics; use Basics;

package body OpenGL.Program is

   type Shader_Access is access all Shader_Type;

   ShaderTypes : constant array(Shader_Enum) of GLenum_Type:=
     (ShaderVertex   => GL_VERTEX_SHADER,
      ShaderFragment => GL_FRAGMENT_SHADER);

   procedure BindAttribLocation
     (Program : Program_Type;
      Index   : GLuint_Type;
      Name    : String) is

      CName : Char_Array:=To_C(Name);

   begin
      glBindAttribLocation
        (program => Program.ID,
         index   => Index,
         name    => CName(CName'First)'Access);
   end BindAttribLocation;
   ---------------------------------------------------------------------------

   procedure Create
     (Program : in out Program_Type;
      Shaders  : Shader_Set) is

   begin

      Program.Reset;
      Program.ID:=glCreateProgram.all;
      if Program.ID=0 then
         raise FailedToCreateProgram with "glCreateProgram returned 0";
      end if;
      Put_Line("New Program:"&GLuint_Type'Image(Program.ID));
      Program.Shaders:=Shaders;
      for Shader of Shaders loop
         if Shader.I/=null then
            Put_Line("Attach : "&GLuint_Type'Image(Shader_Access(Shader.I).ID));
            glAttachShader(Program.ID,Shader_Access(Shader.I).ID);
         end if;
      end loop;
      glLinkProgram(Program.ID);
      -- TODO: CHECK IF LINK has been successfull
      declare
         Log       : Char_Array(1..1024);
         LogLength : aliased GLsizei_Type;
      begin
         glGetProgramInfoLog
           (program   => Program.ID,
            maxLength => Log'Length,
            length    => LogLength'Access,
            infoLog   => Log(Log'First)'Access);
         Program.LinkLog:=U(To_Ada(Log));
      end;
   end Create;
   ---------------------------------------------------------------------------

   function GetLinkLog
     (Program : Program_Type)
      return Unbounded_String is
   begin
      return Program.LinkLog;
   end GetLinkLog;
   ---------------------------------------------------------------------------

   procedure Reset
     (Program : in out Program_Type) is
   begin

      if Program.ID/=0 then
         glDeleteProgram(Program.ID);
         Program.ID:=0;
         Program.LinkLog:=U("");
      end if;

   end Reset;
   ---------------------------------------------------------------------------

   procedure UseProgram
     (Program : in out Program_Type) is
   begin
      pragma Assert(Program.ID/=0);
      glUseProgram(Program.ID);
   end UseProgram;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Program : in out Program_Type) is
   begin
      Program.Reset;
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Create
     (Shader     : in out Shader_Type;
      ShaderType : Shader_Enum;
      Source     : String) is

   begin

      Shader.Reset;
      Shader.ID:=glCreateShader(ShaderTypes(ShaderType));
      if Shader.ID=0 then
         raise FailedToCreateShader with "glCreateShader returned 0";
      end if;
      Put_Line("New Shader:"&GLuint_Type'Image(Shader.ID));
      Put_Line("Pick ShaderType:"&Shader_Enum'Image(ShaderType));

      declare
         CSource    : Char_Array:=To_C(Source);
         SourceList : aliased CChar_Access;
      begin
         SourceList := CSource(CSource'First)'Unrestricted_Access;
         glShaderSource
           (shader  => Shader.ID,
            count   => 1,
            strings => SourceList'Access,
            lengths => null);
         glCompileShader(Shader.ID);
      end;

      declare
         Log       : Char_Array(1..1024);
         LogLength : aliased GLsizei_Type;
      begin
         glGetShaderInfoLog
           (shader    => Shader.ID,
            maxLength => Log'Length,
            length    => LogLength'Access,
            infolog   => Log(Log'First)'Access);
         Shader.CompileLog:=U(To_Ada(Log));
      end;
      -- TODO: Verify if compilation was successfull

   end Create;
   ---------------------------------------------------------------------------

   procedure Reset
     (Shader : in out Shader_Type) is
   begin

      if Shader.ID/=0 then
         glDeleteShader(Shader.ID);
         Shader.ID := 0;
         Shader.CompileLog := U("");
      end if;

   end Reset;
   ---------------------------------------------------------------------------

   function GetCompileLog
     (Shader : Shader_Type)
      return Unbounded_String is
   begin
      return Shader.CompileLog;
   end GetCompileLog;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Shader : in out Shader_Type) is
   begin
      Shader.Reset;
   end Finalize;
   ---------------------------------------------------------------------------

end OpenGL.Program;
