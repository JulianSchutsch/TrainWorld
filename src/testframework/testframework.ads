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

with Basics; use Basics;

package TestFrameWork is

   type TestFunc_Access is access procedure;
   type Test_Type is
      record
         Name : String_Ref;
         Test : TestFunc_Access;
      end record;

   type Test_Array is array(Integer range <>) of Test_Type;

   procedure ReportIssue
     (Message:String);

   procedure Run
     (Tests: Test_Array);

end TestFrameWork;
