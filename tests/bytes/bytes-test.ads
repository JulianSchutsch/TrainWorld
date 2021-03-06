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

package Bytes.Test is

   procedure AccessTest;
   procedure AccessArithmeticTest;

   StrCoreBytesAccess           : aliased constant String:="Core.Bytes.Access";
   StrCoreBytesAccessArithmetic : aliased constant String:="Core.Bytes.AccessArithmetic";

   Tests : constant Test_Array:=
     ((Name => RefConstStr(StrCoreBytesAccess'Access),
       Test => AccessTest'Access),
      (Name => RefConstStr(StrCoreBytesAccessArithmetic'Access),
       Test=> AccessArithmeticTest'Access));

end Bytes.Test;
