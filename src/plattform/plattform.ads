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
--   23.Apr 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   Unify all plattform detection issues in a single file

-- Usage
--   Initialize sets the Detected variable to the detected plattform.
--   If the plattform could not be detected, it remains PlattformUnknown.

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;

package Plattform is

   PlattformInitializationFailed : Exception;

   BasePath : Unbounded_String;

   type Plattform_Enum is
     (PlattformUnknown,
      PlattformLinux,
      PlattformBSD,
      PlattformWindowsNT);

   PlattformID:array(Plattform_Enum) of Unbounded_String:=
     (PlattformUnknown   => U("Unknown"),
      PlattformLinux     => U("Linux"),
      PlattformBSD       => U("BSD"),
      PlattformWindowsNT => U("WindowsNT"));

   Detected         : Plattform_Enum:=PlattformUnknown;
   ExecutableSuffix : Unbounded_String;

   procedure Initialize;

end Plattform;