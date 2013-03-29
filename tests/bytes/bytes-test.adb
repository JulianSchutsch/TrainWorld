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

package body Bytes.Test is

   procedure AccessTest is

      Buffer : Byte_ArrayAccess:=new Byte_Array(1..1024);

   begin

      if AddressToByteAccess(Buffer(Buffer'First)'Address)/=Buffer(Buffer'First)'Access then
         ReportIssue("Access pointer is not aquivalent to a System.Address (using AddressToByteAccess)");
      end if;

      Free(Buffer);

   end AccessTest;
   ---------------------------------------------------------------------------

   procedure AccessArithmeticTest is

      Buffer  : Byte_ArrayAccess:=new Byte_Array(0..255);
      Current : Byte_Access:=Buffer(Buffer'First)'Access;
      Second  : Byte_Access;

   begin
      for i in Buffer'Range loop
         Buffer(i):=Byte_Type(i);
      end loop;

      -- Forward test
      for i in Buffer'Range loop
         if Buffer(i)/=Current.all then
            ReportIssue("Access Arithmetic failes with forward increment at index "&PtrInt_Type'Image(i));
         end if;
         Current:=Current+1;
      end loop;

      -- Backward test
      for i in reverse Buffer'Range loop
         Current:=Current-1;
         if Buffer(i)/=Current.all then
            ReportIssue("Access arithmetic failes with backward decrement at index "&PtrInt_Type'Image(i));
         end if;
      end loop;

      -- Large jump test
      Second:=Current+Integer'Last;
      if Second-Integer'Last/=Current then
         ReportIssue("Large jump with Integer'First failed");
      end if;

      Free(Buffer);

   end AccessArithmeticTest;
   ---------------------------------------------------------------------------

end Bytes.Test;
