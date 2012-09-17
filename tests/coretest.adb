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

with TestFrameWork; use TestFrameWork;

with Bytes.Test;
with Endianess.Test;
with Config.Test;
with GImplementationsTest;
with VersionParser.Test;
with GlobalLoop.Test;
with InterfaceTest;

procedure CoreTest is
   Tests : constant TestFrameWork.Test_Array:=
     Bytes.Test.Tests&
     Endianess.Test.Tests&
     Config.Test.Tests&
     GImplementationsTest.Tests&
     VersionParser.Test.Tests&
     GlobalLoop.Test.Tests&
     InterfaceTest.Tests;
begin
   TestFrameWork.Run(Tests);
end CoreTest;
