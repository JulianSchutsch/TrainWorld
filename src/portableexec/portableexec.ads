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
--   19.Jun 2012 Julian Schutsch
--     - Original version

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

package PortableExec is

   type StringArray_Type is array(Integer range <>) of Unbounded_String;
   type StringArray_Access is access all StringArray_Type;

   package StringList_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Unbounded_String,
      "="          => "=");

   Output  : StringList_Pack.List;
   Success : Boolean;

   function StringInOutput
     (Str : String)
      return Boolean;

   function StringListToGprList
     (List : StringList_Pack.List)
      return Unbounded_String;

   function ExtractArgsFromOutput
     (Args : StringArray_Type)
      return StringList_Pack.List;

   function StringInEnvironment
     (Str : String;
      Env : String)
      return Boolean;

   procedure QuickExec
     (Command  : String;
      Argument : String);

   procedure DeleteFile
     (Name : String);

end PortableExec;
