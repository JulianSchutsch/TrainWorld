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

with Bytes; use Bytes;
with Ada.Unchecked_Conversion;

package body Endianess.Test is

   generic
      type Number_Type is private;
      type Encoded_Type is private;
      with function ToEnc(Value: Number_Type) return Encoded_Type;
      with function FromEnc(Value: Encoded_Type) return Number_Type;
      Direction : Integer;
   procedure Test
     (Value     : Number_Type);

   procedure Test
     (Value     : Number_Type) is

      pragma Warnings(Off);
      function ConvTo64 is new Ada.Unchecked_Conversion
        (Source => Number_Type,
         Target => Cardinal64);
      pragma Warnings(On);

      EncodedNumber : Encoded_Type;
      Position      : Byte_Access;

   begin

      if Number_Type'Size/=Encoded_Type'Size then
         ReportIssue("Number type and encoded type size do not match");
      end if;

      EncodedNumber:=ToEnc(Value);

      if FroMEnc(EncodedNumber)/=Value then
         ReportIssue("From(To(Number))/=Number");
      end if;

      Position:=AddressToByteAccess(EncodedNumber'Address);

      if Direction<0 then
         for i in reverse 0..Number_Type'Size/8-1 loop
            if Position.all/=Byte_Type(Shift_Right(ConvTo64(Value),Byte_Type'Size*i) and 16#FF#) then
               ReportIssue("Byte "&Integer'Image(i)& "incorrect.");
            end if;
            Position:=Position+1;
         end loop;
      else
         for i in 0..Number_Type'Size/8-1 loop
            if Position.all/=Byte_Type(Shift_Right(ConvTo64(Value),Byte_Type'Size*i) and 16#FF#) then
               ReportIssue("Byte "&Integer'Image(i)& "incorrect.");
            end if;
            Position:=Position+1;
         end loop;
      end if;

   end;
   ---------------------------------------------------------------------------

   procedure LittleEndian32Test is

      procedure TestCardinal is new Test(Cardinal32,LittleEndianCardinal32,To,From,1);
      procedure TestInteger is new Test(Integer32,LittleEndianInteger32,To,From,1);

   begin

      TestCardinal(16#12345678#);
      TestInteger(Integer32'First);
      TestInteger(Integer32'Last);
      TestInteger(-1);
      TestInteger(1);
   end LittleEndian32Test;
   ---------------------------------------------------------------------------

   procedure BigEndian32Test is

      procedure TestCardinal is new Test(Cardinal32,BigEndianCardinal32,ToBE,From,-1);
      procedure TestInteger is new Test(Integer32,BigEndianInteger32,ToBE,From,-1);

   begin

      TestCardinal(16#12345678#);
      TestInteger(Integer32'First);
      TestInteger(Integer32'Last);
      TestInteger(-1);
      TestInteger(1);
   end BigEndian32Test;
   ---------------------------------------------------------------------------

end Endianess.Test;
