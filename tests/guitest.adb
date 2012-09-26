-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-- Revision History
--   2.Aug 2012 Julian Schutsch
--     - Original version

pragma Ada_2012;

with Graphics;
with Graphics.Impl;
with Ada.Text_IO; use Ada.Text_IO;
with Config;
with Basics; use Basics;
with OpenGL; use OpenGL;
with GlobalLoop;
with Interfaces.C;
with OpenGL.Program;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;
with OpenGL.Textures;
pragma Warnings(Off);

procedure GUITest is

   C : Graphics.Context_Ref;
   pragma Warnings(Off,C);
   Configuration : Config.ConfigNode_Type;
   Terminated : Boolean:=False;
   pragma Warnings(Off,Terminated);

   VertexShaderSource : constant String:=
     "#version 150"&Character'Val(10)&
     "in vec3 in_Position;"&
     "in vec2 in_TexCoord;"&
     "out vec2 ex_TexCoord;"&
     "void main(void)"&
     "{"&
     "  gl_Position = vec4(in_Position,1.0);"&
     "  ex_TexCoord = in_TexCoord;"&
     "}"&Character'Val(0);

   FragmentShaderSource : constant String:=
     "#version 150"&Character'Val(10)&
     "in vec2 ex_TexCoord;"&
     "out vec4 out_Color;"&
     "uniform sampler2D tex;"&
     "void main(void)"&
     "{"&
     "  out_Color=texture(tex,ex_TexCoord);"&--//texture(tex,in_TexCoord);"&
     "}"&Character'Val(0);

   FragmentShader : aliased OpenGL.Program.Shader_Type;
   VertexShader   : aliased OpenGL.Program.Shader_Type;
   Program        : OpenGL.Program.Program_Type;

   procedure OnContextClose
     (Data : C_ClassAccess) is
      pragma Unreferenced(Data);
   begin
      Terminated:=True;
   end OnContextClose;
   ---------------------------------------------------------------------------

   Vert : GLfloat_Array(0..8):=
     (0.0,0.8,-1.0,
      -0.8,-0.8,-1.0,
      0.8,-0.8,-1.0);
   Tex : GLfloat_Array(0..5):=
     (0.0,0.0,
      0.0,1.0,
      1.0,0.0);

   VertBuffer : aliased GLuint_Type;
   TexBuffer  : aliased GLuint_Type;
   AttArray  : aliased GLuint_Type;
   TexUniform : aliased GLint_Type;
   MyTexture : OpenGL.Textures.BGRATexture_Type;

   procedure OnContextPaint
     (Data : C_ClassAccess) is
      pragma Unreferenced(Data);
   begin
      glViewport(0,0,400,400);
      glClearColor(1.0,1.0,0.0,1.0);
      glClear(GL_COLOR_BUFFER_BIT);
      MyTexture.Bind;
      Program.UseProgram;
      glBindVertexArray(AttArray);
      glDrawArrays(GL_TRIANGLES,0,3);
      glBindVertexArray(0);
      AssertError;
   end OnContextPaint;
   ---------------------------------------------------------------------------

begin

   Graphics.Impl.Register;

   Configuration.SetImplementation( U("OpenGL"));
   Graphics.CreateConfig
     (Configuration => Configuration,
      WindowType    => Graphics.WindowTypeWindow,
      BufferKind    => Graphics.BufferKindDefault);

   Put_Line("Find");
   C:=Graphics.Implementations.Utilize(Configuration);

   C.I.OnClose:=OnContextClose'Unrestricted_Access;
   C.I.OnPaint:=OnContextPaint'Unrestricted_Access;

   if OpenGL.SupportProgram then
      Put_Line("GLSL supported");
   else
      Put_Line("GLSL not supported");
      return;
   end if;
   Put_Line("GLSL Version:"&GLint_Type'Image(GLSLVersion.Major)&"."&GLint_Type'Image(GLSLVersion.Minor));

   FragmentShader.Create
     (ShaderType => OpenGL.Program.ShaderFragment,
      Source     => FragmentShaderSource);
   Put_Line("Compile Fragment Shader:"&To_String(FragmentShader.GetCompileLog));

   VertexShader.Create
     (ShaderType => OpenGL.Program.ShaderVertex,
      Source     => VertexShaderSource);
   Put_Line("Compile Vertex Shader:"&To_String(VertexShader.GetCompileLog));

   Program.Create
     ((OpenGL.Program.ShaderVertex   => OpenGL.Program.Ref.MakeConstRef(VertexShader'Unrestricted_Access),
       OpenGL.Program.ShaderFragment => OpenGL.Program.Ref.MakeConstRef(FragmentShader'Unrestricted_Access)));
   Put_Line("Link:"&To_String(Program.GetLinkLog));
   Program.BindAttribLocation(0,"in_Position");
   Program.BindAttribLocation(1,"in_TexCoord");
   Program.UseProgram;

   glGenVertexArrays(1,AttArray'Access);
   glBindVertexArray(AttArray);

   glGenBuffers(1,VertBuffer'Access);
   glBindBuffer(GL_ARRAY_BUFFER,VertBuffer);
   glBufferData(GL_ARRAY_BUFFER,Vert'Size/8,Vert(0)'Address,GL_STATIC_DRAW);
   glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,0,System.Null_Address);
   glEnableVertexAttribArray(0);

   glGenBuffers(1,TexBuffer'Access);
   glBindBuffer(GL_ARRAY_BUFFER,TexBuffer);
   glBufferData(GL_ARRAY_BUFFER,Tex'Size/8,Tex(0)'Address,GL_STATIC_DRAW);
   glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,0,System.Null_Address);
   glEnableVertexAttribArray(1);

   glBindVertexArray(0);

   TexUniform:=Program.GetUniformLocation("tex");
   Program.UseProgram;
   glUniform1i(TexUniform,0);

   MyTexture.Create
     (Height => 10,
      Width => 10);

   MyTexture.Clear
     (Color => (Red=>255,Green=>0,Blue=>255,Alpha=>255));
   MyTexture.Pixels(4,4):=(Red=>0,Green=>0,Blue=>0,Alpha=>255);

   MyTexture.Upload;

   while not Terminated loop
      GlobalLoop.Process;
   end loop;

   FragmentShader.Reset;
   VertexShader.Reset;

end GUITest;
