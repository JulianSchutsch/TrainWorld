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

package OpenGL is

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
   type GLenum_Type is new Interfaces.Unsigned_32;

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

   function glGetString
     (name : GLenum_Type)
     return String;

   function glGetError
     return GLenum_Type;
   pragma Import(StdCall,glGetError,"glGetError");

   procedure AssertError;

end OpenGL;
