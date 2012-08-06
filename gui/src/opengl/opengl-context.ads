-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
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
--   29.Apr 2012 Julian Schutsch
--     - Correction : Texture coordinates are based on Canvas.Bounds,
--                    not on Canvas.ContentHeight or Width

pragma Ada_2005;

with Graphics; use Graphics;

package OpenGL.Context is

private

   type Context_Type is new Graphics.Context_Type with
      record
         null;
      end record;

   procedure Dummy;

end OpenGL.Context;
