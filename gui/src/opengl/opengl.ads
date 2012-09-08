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
--   18.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Interfaces.C;
with Interfaces;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

package OpenGL is

   InvalidOpenGLVersion : Exception;

   OpenGLError : Exception;

   -- Portability : Maybe risky and non portable when using anything but
   --               the GNAT compiler
   subtype GLdouble_Type is Long_Float;
   subtype GLfloat is Float;
   subtype GLclampf_Type is Float;
   subtype GLfloat_Type is Float;

   type GLbitfield_Type is new Interfaces.C.unsigned;
   type GLint_Type is new Interfaces.C.int;
   type GLuint_Type is new Interfaces.C.unsigned;
   type GLint_Access is access all GLint_Type;
   type GLsizei_Type is new Interfaces.C.int;
   type GLsizei_Access is access all GLsizei_Type;
   type GLsizeiptr_Type is new Interfaces.C.int; -- <- why this one?
   type GLenum_Type is new Interfaces.Unsigned_32;
   subtype GLchar_Type is Interfaces.C.char;
   type GLchar_Access is access all GLchar_Type;
   type GLboolean_Type is new Interfaces.Unsigned_8;

   type CChar_Access is access all Interfaces.C.char;
   pragma Convention(C,CChar_Access);

   type OpenGLVersion_Type is
      record
         Major : aliased GLint_Type;
         Minor : aliased GLint_Type;
      end record;

   GL_MODELVIEW  : constant GLenum_Type:=16#1700#;
   GL_PROJECTION : constant GLenum_Type:=16#1701#;

   GL_SCISSOR_TEST : constant GLenum_Type:=16#C11#;
   GL_DEPTH_TEST   : constant GLenum_Type:=16#B71#;
   GL_BLEND        : constant GLenum_Type:=16#BE2#;
   GL_TEXTURE_2D   : constant GLenum_Type:=16#DE1#;

   GL_SRC_ALPHA           : constant GLenum_Type:=16#302#;
   GL_ONE_MINUS_SRC_ALPHA : constant GLenum_Type:=16#303#;
   GL_GREATER             : constant GLenum_Type:=16#204#;

   GL_COLOR_BUFFER_BIT : constant GLbitfield_Type:=16#4000#;
   GL_DEPTH_BUFFER_BIT : constant GLbitfield_Type:=16#100#;

   GL_RGBA            : constant:=16#1908#;
   GL_UNSIGNED_BYTE   : constant:=16#1401#;
   GL_BGRA            : constant:=16#80E1#;

   GL_QUADS : constant GLenum_Type:=7;

   GL_TEXTURE_MIN_FILTER : constant GLenum_Type:=16#2801#;
   GL_TEXTURE_MAG_FILTER : constant GLenum_Type:=16#2800#;
   GL_TEXTURE_WRAP_S     : constant GLenum_Type:=16#2802#;
   GL_TEXTURE_WRAP_T     : constant GLenum_Type:=16#2803#;

   GL_NEAREST            : constant GLint_Type:=16#2600#;
   GL_CLAMP              : constant GLint_Type:=16#2900#;
   GL_REPEAT             : constant GLint_Type:=16#2901#;

   GL_VERSION : constant GLenum_Type:=16#1F02#;

   GL_MAJOR_VERSION : constant GLenum_Type:=16#821B#;
   GL_MINOR_VERSION : constant GLenum_Type:=16#821C#;

   -- GetProc_Access expects null terminated strings and returns
   -- a pointer to a function/procedure of the OpenGL interface
   type GetProc_Access is
     access function
       (Name : String) return System.Address;

   type glGetString_Access is
     access function
       (name : GLenum_Type)
        return chars_ptr;
   pragma Convention(StdCall,glGetString_Access);

   function glGetString
     (name    : GLenum_Type;
      GetProc : not null GetProc_Access)
      return String;

   type glGetIntegerv_Access is
     access procedure
       (pname  : GLenum_Type;
        params : access GLint_Type);
   pragma Convention(StdCall,glGetIntegerv_Access);

   procedure glGetIntegerv
     (pname : GLenum_Type;
      params : access GLint_Type;
      GetProc : not null GetProc_Access);

   type glClearColor_Access is
     access procedure
       (red   : GLclampf_Type;
        green : GLclampf_Type;
        blue  : GLclampf_Type;
        alpha : GLclampf_Type);
   pragma Convention(StdCall,glClearColor_Access);

   type glClear_Access is
     access procedure
       (mask : GLbitfield_Type);

   procedure glFinish;
   pragma Import(StdCall,glFinish,"glFinish");
   glClearColor : glClearColor_Access:=null;
   glClear      : glClear_Access:=null;
   ---------------------------------------------------------------------------

   -- Buffer Objects

   type glGenBuffers_Access is
     access procedure
       (n       : GLsizei_Type;
        buffers : access GLuint_Type);

   type glBindBuffer_Access is
     access procedure
       (target : GLenum_Type;
        buffer : GLuint_Type);

   type glBufferData_Access is
     access procedure
       (target : GLenum_Type;
        size   : GLsizeiptr_Type;
        data   : System.Address;
        usage  : GLenum_Type);

   glGenBuffers : glGenBuffers_Access:=null;
   glBindBuffer : glBindBuffer_Access:=null;
   glBufferData : glBufferData_Access:=null;

   SupportBufferObjects : Boolean:=False;

   -- Vertex Attributes

   type glVertexAttribPointer_Access is
     access procedure
       (index      : GLuint_Type;
        size       : GLint_Type;
        ttype      : GLenum_Type;
        normalized : GLboolean_Type;
        stride     : GLsizei_Type;
        pointer    : System.Address);

   glVertexAttribPointer : glVertexAttribPointer_Access:=null;

   SupportVertexAttributes : Boolean:=False;

   -- GLSL --

   type glCreateProgram_Access is
     access function
     return GLuint_Type;
   pragma Convention(StdCall,glCreateProgram_Access);

   type glDeleteProgram_Access is
     access procedure
       (program : GLuint_Type);
   pragma Convention(StdCall,glDeleteProgram_Access);

   type glUseProgram_Access is
     access procedure
       (program : GLuint_Type);
   pragma Convention(StdCall,glUseProgram_Access);

   type glAttachShader_Access is
     access procedure
       (program : GLuint_Type;
        shader  : GLuint_Type);
   pragma Convention(StdCall,glAttachShader_Access);

   type glDetachShader_Access is
     access procedure
       (program : GLuint_Type;
        shader  : GLuint_Type);
   pragma Convention(StdCall,glDetachShader_Access);

   type glLinkProgram_Access is
     access procedure
       (program : GLuint_Type);
   pragma Convention(StdCall,glLinkProgram_Access);

   type glGetProgramInfoLog_Access is
     access procedure
       (program   : GLuint_Type;
        maxLength : GLsizei_Type;
        length    : access GLsizei_Type;
        infoLog   : access GLchar_Type);
   pragma Convention(StdCall,glGetProgramInfoLog_Access);

   type glGetProgramiv_Access is
     access procedure
       (program : GLuint_Type;
        pname   : GLenum_Type;
        params  : access GLint_Type);
   pragma Convention(StdCall,glGetProgramiv_Access);

   type glGetShaderInfoLog_Access is
     access procedure
       (shader    : GLuint_Type;
        maxLength : GLsizei_Type;
        length    : access GLsizei_Type;
        infoLog   : access GLchar_Type);
   pragma Convention(StdCall,glGetShaderInfoLog_Access);

   type glGetUniformLocation_Access is
     access function
       (program : GLuint_Type;
        name    : access GLchar_Type) -- const
        return GLint_Type;
   pragma Convention(StdCall,glGetUniformLocation_Access);

   type glCreateShader_Access is
     access function
       (shaderType : GLenum_Type)
        return GLuint_Type;
   pragma Convention(StdCall,glCreateShader_Access);

   type glDeleteShader_Access is
     access procedure
       (shader : GLuint_Type);
   pragma Convention(StdCall,glDeleteShader_Access);

   type glShaderSource_Access is
     access procedure
       (shader  : GLuint_Type;
        count   : GLsizei_Type;
        strings : access CChar_Access;
        lengths : access GLint_Type);
   pragma Convention(StdCall,glShaderSource_Access);

   type glCompileShader_Access is
     access procedure
       (shader : GLuint_Type);
   pragma Convention(StdCall,glCompileShader_Access);

   type glGetShaderiv_Access is
     access procedure
       (shader : GLuint_Type;
        pname  : GLenum_Type;
        params : access GLint_Type);
   pragma Convention(StdCall,glGetShaderiv_Access);

   glCreateProgram      : glCreateProgram_Access      := null;
   glDeleteProgram      : glDeleteProgram_Access      := null;
   glUseProgram         : glUseProgram_Access         := null;
   glAttachShader       : glAttachShader_Access       := null;
   glDetachShader       : glDetachShader_Access       := null;
   glLinkProgram        : glLinkProgram_Access        := null;
   glGetProgramiv       : glGetProgramiv_Access       := null;
   glGetShaderInfoLog   : glGetShaderInfoLog_Access   := null;
   glGetUniformLocation : glGetUniformLocation_Access := null;
   glGetProgramInfoLog  : glGetProgramInfoLog_Access  := null;

   glCreateShader  : glCreateShader_Access  := null;
   glDeleteShader  : glDeleteShader_Access  := null;
   glShaderSource  : glShaderSource_Access  := null;
   glCompileShader : glCompileShader_Access := null;
   glGetShaderiv   : glGetShaderiv_Access   := null;

   SupportProgram  : Boolean := False;

   GL_FRAGMENT_SHADER : constant GLenum_Type:=16#8B30#;
   GL_VERTEX_SHADER   : constant GLenum_Type:=16#8B31#;

   ---------------------------------------------------------------------------

   function GetVersion
     (GetProc : not null GetProc_Access)
      return OpenGLVersion_Type;

   -- These functions are like the usual opengl calls, but the
   -- function pointer is loaded on demand.
   -- This is necessary for initialisation

   -- TODO: Do not link static...
   function glGetError
     return GLenum_Type;
   pragma Import(StdCall,glGetError,"glGetError");

   procedure LoadFunctions
     (DefaultGetProc   : not null GetProc_Access;
      ExtensionGetProc : not null GetProc_Access;
      Compatible       : Boolean);

   procedure AssertError;

end OpenGL;
