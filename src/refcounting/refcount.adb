-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
--
--   TrainWorld is free software: you can redistribute it and/or modify
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

with Ada.Unchecked_Deallocation;

package body RefCount is

   procedure IncrementRefCount
     (Ref : in out Ref_Interface) is
   begin

      Ref.Count := Ref.Count+1;

   end IncrementRefCount;
   ---------------------------------------------------------------------------

   procedure DecrementRefCount
     (Ref : in out Ref_Interface) is
   begin

      Ref.Count := Ref.Count-1;
      if Ref.Count=0 then
         Ref.Finalize;
      end if;

   end DecrementRefCount;
   ---------------------------------------------------------------------------

   package body Ref is

      type Ref_Access is access all Ref_Interface;

      procedure SetNull
        (Ref : in out Ref_Type) is
      begin
         Finalize(Ref);
      end SetNull;
      ------------------------------------------------------------------------

      procedure Adjust
        (Ref: in out Ref_Type) is
      begin
         if Ref.I/=null then
            Ref_Access(Ref.I).Count:=Ref_Access(Ref.I).Count+1;
         end if;
      end Adjust;
      ------------------------------------------------------------------------

      procedure Finalize
        (Ref: in out Ref_Type) is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Interface_Type'Class,
            Name   => Interface_ClassAccess);

      begin

         if Ref.I/=null then
            Ref_Access(Ref.I).Count:=Ref_Access(Ref.I).Count-1;
            if Ref_Access(Ref.I).Count=0 then
               Free(Ref.I);
            end if;
            Ref.I:=null;
         end if;

      end Finalize;
      ------------------------------------------------------------------------

      function MakeInitialRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type is
      begin

         return R:Ref_Type do
            R.I:=Object;
         end return;

      end MakeInitialRef;
      ------------------------------------------------------------------------

      function MakeAdditionalRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type is
      begin
         return R:Ref_Type do
            R.I:=Object;
            Ref_Access(Object).Count:=Ref_Access(Object).Count+1;
         end return;
      end MakeAdditionalRef;
      ------------------------------------------------------------------------

   end Ref;

end RefCount;
