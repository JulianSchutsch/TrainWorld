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

with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Config is

   -- Raised if any Config_Pointer is left pointing to a finalized Config_Type
   PointersLeft  : Exception;
   PointerToNull : Exception;
   ValueNotSet   : Exception;

   type Config_Pointer is new Ada.Finalization.Controlled with private;

   overriding
   procedure Finalize
     (Pointer : in out Config_Pointer);

   overriding
   procedure Adjust
     (Pointer : in out Config_Pointer);

   procedure SetValue
     (Pointer : in out Config_Pointer;
      Value   : Unbounded_String);

   function GetValue
     (Pointer : in Config_Pointer)
      return Unbounded_String;

   procedure GetNode
     (Pointer : in Config_Pointer;
      SubPath : Unbounded_String;
      Result  : out Config_Pointer'Class);

   function GetChildValue
     (Pointer : in Config_Pointer;
      Name    : Unbounded_String)
      return Unbounded_String;

   type Config_Type is new Ada.Finalization.Limited_Controlled with private;

   overriding
   procedure Finalize
     (Config : in out Config_Type);

   procedure GetNode
     (Config : in out Config_Type;
      Path   : Unbounded_String;
      Result : out Config_Pointer'Class;
      Create : Boolean := False);

   procedure CreateNode
     (Config : in out Config_Type;
      Path   : Unbounded_String;
      Value  : Unbounded_String);

private

   type ConfigNode_Type;
   type ConfigNode_Access is access all ConfigNode_Type;
   type ConfigNode_Type is
      record
         Name        : Unbounded_String;
         Value       : Unbounded_String;
         Childs      : ConfigNode_Access := null;
         NextSibling : ConfigNode_Access := null;
      end record;

   type Config_Access is access all Config_Type;
   type Config_Type is new Ada.Finalization.Limited_Controlled with
      record
         Root     : ConfigNode_Type;
         Pointers : Natural           := 0;
      end record;

   type Config_Pointer is new Ada.Finalization.Controlled with
      record
         Config : Config_Access     := null;
         Node   : ConfigNode_Access := null;
      end record;

end Config;
