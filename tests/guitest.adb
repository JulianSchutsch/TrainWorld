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
with OpenGL.Program;
with OpenGL.Textures;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;
with Ada.Finalization;

procedure GUITest is

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

   Vert : GLfloat_Array(0..8):=
     (0.0,0.8,-1.0,
      -0.8,-0.8,-1.0,
      0.8,-0.8,-1.0);
   Tex : GLfloat_Array(0..5):=
     (0.0,0.0,
      0.0,1.0,
      1.0,0.0);

   type ContextCallBack_Type is new Ada.Finalization.Limited_Controlled and Graphics.ContextCallBack_Interface with
      record
         FragmentShader : aliased OpenGL.Program.Shader_Type;
         VertexShader   : aliased OpenGL.Program.Shader_Type;
         Program        : OpenGL.Program.Program_Type;
         VertBuffer     : aliased GLuint_Type;
         TexBuffer      : aliased GLuint_Type;
         AttArray       : aliased GLuint_Type;
         TexUniform     : aliased GLint_Type;
         MyTexture      : OpenGL.Textures.BGRATexture_Type;
      end record;

   overriding
   procedure ContextClose
     (Data : in out ContextCallBack_Type);

   overriding
   procedure ContextPaint
     (Data : in out ContextCallBack_Type);

   overriding
   procedure ContextCreate
     (Data : in out ContextCallBack_Type);

   overriding
   procedure Finalize
     (Data : in out ContextCallBack_Type);
   ---------------------------------------------------------------------------

   procedure Finalize
     (Data : in out ContextCallBack_Type) is
      pragma Unreferenced(Data);
   begin
      Put_Line("Data.Finalize");
   end Finalize;
   ---------------------------------------------------------------------------

   procedure ContextCreate
     (Data : in out ContextCallBack_Type) is

   begin

      Data.FragmentShader.Create
        (ShaderType => OpenGL.Program.ShaderFragment,
         Source     => FragmentShaderSource);
      Put_Line("Compile Fragment Shader:"&To_String(Data.FragmentShader.GetCompileLog));

      Data.VertexShader.Create
        (ShaderType => OpenGL.Program.ShaderVertex,
         Source     => VertexShaderSource);
      Put_Line("Compile Vertex Shader:"&To_String(Data.VertexShader.GetCompileLog));

      Data.Program.Create
        ((OpenGL.Program.ShaderVertex   => OpenGL.Program.Ref.MakeConstRef(Data.VertexShader'Unrestricted_Access),
          OpenGL.Program.ShaderFragment => OpenGL.Program.Ref.MakeConstRef(Data.FragmentShader'Unrestricted_Access)));
      Put_Line("Link:"&To_String(Data.Program.GetLinkLog));
      Data.Program.BindAttribLocation(0,"in_Position");
      Data.Program.BindAttribLocation(1,"in_TexCoord");
      Data.Program.UseProgram;

      glGenVertexArrays(1,Data.AttArray'Access);
      glBindVertexArray(Data.AttArray);

      glGenBuffers(1,Data.VertBuffer'Access);
      glBindBuffer(GL_ARRAY_BUFFER,Data.VertBuffer);
      glBufferData(GL_ARRAY_BUFFER,Vert'Size/8,Vert(0)'Address,GL_STATIC_DRAW);
      glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,0,System.Null_Address);
      glEnableVertexAttribArray(0);

      glGenBuffers(1,Data.TexBuffer'Access);
      glBindBuffer(GL_ARRAY_BUFFER,Data.TexBuffer);
      glBufferData(GL_ARRAY_BUFFER,Tex'Size/8,Tex(0)'Address,GL_STATIC_DRAW);
      glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,0,System.Null_Address);
      glEnableVertexAttribArray(1);

      glBindVertexArray(0);

      Data.TexUniform:=Data.Program.GetUniformLocation("tex");
      Data.Program.UseProgram;
      glUniform1i(Data.TexUniform,0);

      Data.MyTexture.Create
        (Height => 10,
         Width => 10);

      Data.MyTexture.Clear
        (Color => (Red=>255,Green=>0,Blue=>255,Alpha=>255));
      Data.MyTexture.Pixels(4,4):=(Red=>0,Green=>0,Blue=>0,Alpha=>255);

      Data.MyTexture.Upload;

   end ContextCreate;
   ---------------------------------------------------------------------------

   procedure ContextClose
     (Data : in out ContextCallBack_Type) is

   begin
      Data.FragmentShader.Reset;
      Data.VertexShader.Reset;
      Terminated:=True;
   end ContextClose;
   ---------------------------------------------------------------------------

   procedure ContextPaint
     (Data : in out ContextCallBack_Type) is
   begin
      glViewport(0,0,400,400);
      glClearColor(1.0,1.0,0.0,1.0);
      glClear(GL_COLOR_BUFFER_BIT);
      Data.MyTexture.Bind;
      Data.Program.UseProgram;
      glBindVertexArray(Data.AttArray);
      glDrawArrays(GL_TRIANGLES,0,3);
      glBindVertexArray(0);
      AssertError;
   end ContextPaint;
   ---------------------------------------------------------------------------

begin

   Graphics.Impl.Register;

   Configuration.SetImplementation( U("OpenGL"));
   Graphics.CreateConfig
     (Configuration => Configuration,
      WindowType    => Graphics.WindowTypeWindow,
      BufferKind    => Graphics.BufferKindDefault);

   declare
      Context:constant Graphics.Context_Ref:=Graphics.Implementations.Utilize(Configuration);
   begin

      declare
         ContextCallBack : ContextCallBack_Type;
      begin

         Context.I.CallBack:=ContextCallBack'Unrestricted_Access;

         while not Terminated loop
            GlobalLoop.Process;
         end loop;

      end;
      Put_Line("Leaving context area");

   end;

end GUITest;
