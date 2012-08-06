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
--   25.Jan 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Interfaces;

package Types is

   type Integer16 is new Interfaces.Integer_16;
   type Integer32 is new Interfaces.Integer_32;
   type Integer64 is new Interfaces.Integer_64;

   type Cardinal16 is new Interfaces.Unsigned_16;
   type Cardinal32 is new Interfaces.Unsigned_32;
   type Cardinal64 is new Interfaces.Unsigned_64;

end Types;
