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

package body TestFrameWork is

   IssueReported : Boolean:=False;

   procedure ReportIssue(Message:String) is
   begin
      Put_Line(" ISSUE : "&Message);
      IssueReported:=True;
   end ReportIssue;
   ---------------------------------------------------------------------------

   procedure Run(Tests:Test_Array) is
   begin
      for i in Tests'Range loop
         Put_Line(To_String(Tests(i).Name));
         Tests(i).Test.all;
      end loop;
      if IssueReported then
         Put_Line("*******************************************************************************");
         Put_Line(" Some test(s) failed.");
         Put_Line("*******************************************************************************");
      end if;
   end Run;
   ---------------------------------------------------------------------------

end TestFrameWork;
