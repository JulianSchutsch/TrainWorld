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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers.Vectors;

package body TestFrameWork is

   IssueReported : Boolean:=False;

   procedure ReportIssue(Message:String) is
   begin
      Put_Line(" ISSUE : "&Message);
      IssueReported:=True;
   end ReportIssue;
   ---------------------------------------------------------------------------

   package TestVector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => String_Ref);

   ActiveTests : TestVector.Vector;

   procedure AddTest
     (Test : String_Ref) is
   begin
      ActiveTests.Append(Test);
   end AddTest;
   ---------------------------------------------------------------------------

   function HasTest
     (Test : String_Ref)
      return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      if ActiveTests.Length=0 then
         return True;
      end if;
      if ActiveTests.Contains(Test) then
         return True;
      end if;
      return False;
   end HasTest;
   ---------------------------------------------------------------------------

   procedure Run(Tests:Test_Array) is
   begin

      for i in 1..Ada.Command_Line.Argument_Count loop
         AddTest(RefStr(Ada.Command_Line.Argument(i)));
      end loop;

      for i in Tests'Range loop
         if HasTest(Tests(i).Name) then
            Put_Line("["&Tests(i).Name.Get&"]");
            Tests(i).Test.all;
         end if;
      end loop;
      if IssueReported then
         Put_Line("*******************************************************************************");
         Put_Line(" Some test(s) failed.");
         Put_Line("*******************************************************************************");
      end if;
   end Run;
   ---------------------------------------------------------------------------

end TestFrameWork;
