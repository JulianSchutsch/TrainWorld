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
--   25.Sep.2012 Julian Schutsch
--     - Original version

-- Usage
--   This library provides access to opengl textures in combination with
--   a main memory buffer.
--

with Textures;

package OpenGL.Textures is

   subtype BGRATexture_Inherited is Standard.Textures.BGRATexture_Type;
   type BGRATexture_Type is new BGRATexture_Inherited with
      record
         TextureID : aliased GLuint_Type:=0;
      end record;

   overriding
   procedure Finalize
     (Texture : in out BGRATexture_Type);

   overriding
   procedure Create
     (Texture : in out BGRATexture_Type;
      Height  : Natural;
      Width   : Natural);

   overriding
   procedure Bind
     (Texture : in out BGRATexture_Type);

   overriding
   procedure Upload
     (Texture : in out BGRATexture_Type);

end OpenGL.Textures;
