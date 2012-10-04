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

-- Revision History
--   4.Okt 2012 Julian Schutsch
--     - Original version

-- Usage
--   Reference counted data types are supposed to be freed and finalized
--   automatically once no more reference to them exists.
--   This permits a very limited form of garbage collection.
--
--   For this to work in Ada, all reference counted types must descend from
--   Ref_Interface. With this the package Ref can be specialized.
--   The actual object can be accessed by Ref_Type.I.
--
--   For new objects which should be freed once no more references are present,
--   use MakeInitialReference once to create the first reference.
--   MakeInitialReference must not be used twice and ownership of the object
--   is then exclusive to the Ref_Type. Other references should be handled
--   with care.
--
--   For objects whos memory should not be freed, e.g. simple variables
--   of object, MakeAdditionalReference can be used to create as many
--   references as needed.
--   As with all access variables, memory of the object must not be released
--   as long as references to it are hold.
--
--   Assignment and finalization of references are handled automatically.
--   Calls to IncrementRefCount and DecrementRefCount are not necessary under
--   normal circumstances.

pragma Ada_2012;

with Ada.Finalization;

package RefCount is

   type Ref_Interface is new Ada.Finalization.Limited_Controlled with private;

   not overriding
   procedure IncrementRefCount
     (Ref : in out Ref_Interface);

   not overriding
   procedure DecrementRefCount
     (Ref : in out Ref_Interface);

   generic

      type Interface_Type is abstract new Ref_Interface with private;
      type Interface_ClassAccess is access all Interface_Type'Class;

   package Ref is

      type Ref_Type is new Ada.Finalization.Controlled with
         record
            I : Interface_ClassAccess:=null;
         end record;

      not overriding
      procedure SetNull
        (Ref : in out Ref_Type);

      overriding
      procedure Adjust
        (Ref : in out Ref_Type);

      overriding
      procedure Finalize
        (Ref : in out Ref_Type);
      ------------------------------------------------------------------------

      function MakeAdditionalRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type;

      function MakeInitialRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type;

   end Ref;

private

   type Ref_Interface is new Ada.Finalization.Limited_Controlled with
      record
         Count : Natural:=1;
      end record;

end RefCount;
