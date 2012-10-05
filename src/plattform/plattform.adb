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

pragma Ada_2005;

with PortableExec;

package body Plattform is

   procedure Detect is
   begin

      -- uname plattform detection, if possible
      PortableExec.QuickExec("uname","-s");
      if PortableExec.Success then
         if PortableExec.StringInOutput("MINGW") then
            Detected:=PlattformWindowsNT;
            ExecutableSuffix:=U(".exe");
            return;
         end if;
         if PortableExec.StringInOutput("Linux") then
            Detected:=PlattformLinux;
            return;
         end if;
         if PortableExec.StringInOutput("FreeBSD") then
            Detected:=PlattformFreeBSD;
            return;
         end if;
         if PortableExec.StringInOutput("NetBSD") then
            Detected:=PlattformNetBSD;
            return;
         end if;
      end if;

      if PortableExec.StringInEnvironment("Windows_NT","OS") then
         Detected:=PlattformWindowsNT;
         return;
      end if;

      Detected:=PlattformUnknown;

   end Detect;
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin
      Detect;
   end Initialize;
   ---------------------------------------------------------------------------

end Plattform;
