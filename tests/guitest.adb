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

procedure GUITest is

   C : Graphics.Context_Ref;
   pragma Warnings(Off,C);
   Configuration : Config.ConfigNode_Type;
   Terminated : Boolean:=False;
   pragma Warnings(Off,Terminated);

   VertexShaderSource : constant String:=
     "#version 120"&Character'Val(10)&
     "attribute vec3 in_Position;"&
     "attribute vec3 in_Color;"&
     "varying vec3 ex_Color;"&
     "void main(void)"&
     "{"&
     "  gl_Position = vec4(in_Position,1.0);"&
     "  ex_Color = in_Color;"&
     "}"&Character'Val(0);

   FragmentShaderSource : constant String:=
     "#version 120"&Character'Val(10)&
     "varying vec3 ex_Color;"&
     "void main(void)"&
     "{"&
     "  gl_FragColor=vec4(ex_Color,1.0);"&
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
   Col : GLfloat_Array(0..8):=
     (1.0,0.0,0.0,
      0.0,1.0,0.0,
      0.0,0.0,1.0);

   VertBuffer : aliased GLuint_Type;
   ColBuffer  : aliased GLuint_Type;
   VertArray  : aliased GLuint_Type;


   procedure OnContextPaint
     (Data : C_ClassAccess) is
      pragma Unreferenced(Data);
   begin
      glViewport(0,0,400,400);
      glClearColor(1.0,1.0,0.0,1.0);
      glClear(GL_COLOR_BUFFER_BIT);
      Program.UseProgram;
      glBindVertexArray(VertArray);
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
      WindowType    => Graphics.WindowTypeWindow);

   Put_Line("Find");
   C:=Graphics.Implementations.Utilize(Configuration);

   C.I.OnClose:=OnContextClose'Unrestricted_Access;
   C.I.OnPaint:=OnContextPaint'Unrestricted_Access;

--   Put_Line("Extensions:");
--   for i in OpenGL.Extensions'Range loop
--      Put_Line(Integer'Image(i)&":"&To_String(OpenGL.Extensions(i))&":");
--   end loop;
   AssertError;

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
   Program.BindAttribLocation(1,"in_Color");
   Program.UseProgram;

   glGenVertexArrays(1,VertArray'Access);
   glBindVertexArray(VertArray);

   glGenBuffers(1,VertBuffer'Access);
   glBindBuffer(GL_ARRAY_BUFFER,VertBuffer);
   glBufferData(GL_ARRAY_BUFFER,Vert'Size/8,Vert(0)'Address,GL_STATIC_DRAW);
   glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,0,System.Null_Address);
   glEnableVertexAttribArray(0);

   glGenBuffers(1,ColBuffer'Access);
   glBindBuffer(GL_ARRAY_BUFFER,ColBuffer);
   glBufferData(GL_ARRAY_BUFFER,Col'Size/8,Col(0)'Address,GL_STATIC_DRAW);
   glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,0,System.Null_Address);
   glEnableVertexAttribArray(1);

   glBindVertexArray(0);


   while not Terminated loop
      GlobalLoop.Process;
   end loop;

   FragmentShader.Reset;
   VertexShader.Reset;

end GUITest;
