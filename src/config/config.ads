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
--   2.Aug 2012 Julian Schutsch
--     - Original version

pragma Ada_2012;

with Ada.Finalization;
with Basics; use Basics;

package Config is

   MissingConfig : Exception;

   type ConfigPath_Type is array (Integer range <>) of String_Ref;

   type Config_Type is tagged private;
   type Config_ClassAccess is access all Config_Type'Class;

   type ConfigNode_Type is new Ada.Finalization.Controlled with private;
   type ConfigNode_Access is access all ConfigNode_Type;

   not overriding
   function GetImplementation
     (ConfigNode : ConfigNode_Type)
      return String_Ref;

   not overriding
   procedure SetImplementation
     (ConfigNode     : in out ConfigNode_Type;
      Implementation : String_Ref);

   not overriding
   function GetConfig
     (ConfigNode : ConfigNode_Type)
      return Config_ClassAccess;

   not overriding
   function GetImplConfig
     (ConfigNode     : ConfigNode_Type;
      Implementation : String_Ref)
      return Config_ClassAccess;

   not overriding
   function GetName
     (ConfigNode : ConfigNode_Type)
      return String_Ref;

   not overriding
   function GetChildNodeCount
     (ConfigNode : ConfigNode_Type)
      return Natural;

   not overriding
   function GetDeepChildNodeCount
     (ConfigNode : ConfigNode_Type)
      return Natural;

   not overriding
   procedure DebugTree
     (ConfigNode : ConfigNode_Type);

   overriding
   procedure Finalize
     (ConfigNode : in out ConfigNode_Type)
   with Post => (GetChildNodeCount(ConfigNode)=0);

   overriding
   procedure Adjust
     (ConfigNode : in out ConfigNode_Type);

   not overriding
   procedure SetConfig
     (ConfigNode : in out ConfigNode_Type;
      Config     : Config_ClassAccess)
   with Post => ConfigNode.GetConfig=Config;

   not overriding
   procedure SetImplConfig
     (ConfigNode     : in out ConfigNode_Type;
      Implementation : String_Ref;
      Config         : Config_ClassAccess)
   with Post => ConfigNode.GetImplConfig(Implementation)=Config;

   not overriding
   function GetPath
     (ConfigNode : in out ConfigNode_Type;
      Path       : ConfigPath_Type)
      return ConfigNode_Access
   with  Post =>
     (GetPath'Result=null) or
     (
        (GetPath'Result/=null) and then
        (
           (Path'Length=0) or
           (
              (Path'Length/=0)and then GetPath'Result.GetName=Path(Path'Last)
           )
        )
     );

   not overriding
   function CreatePath
     (ConfigNode : in out ConfigNode_Type;
      Path       : ConfigPath_Type)
      return ConfigNode_Access
   with Post => (GetChildNodeCount(ConfigNode)-GetChildNodeCount(ConfigNode)'Old>=0)
     and (CreatePath'Result/=null);

   -- ConfigNode --
   ---------------------------------------------------------------------------

private

   type Config_Type is tagged
      record
         Count : Natural:=1;
      end record;

   type ImplConfig_Type;
   type ImplConfig_Access is access all ImplConfig_Type;
   type ImplConfig_Type is
      record
         Implementation : String_Ref;
         Config         : Config_ClassAccess:=null;
         Next           : ImplConfig_Access;
      end record;

   type ConfigNode_Type is new Ada.Finalization.Controlled with
      record
         Count          : Natural:=1;
         Name           : String_Ref;
         Implementation : String_Ref;
         Config         : Config_ClassAccess:=null;
         ImplConfig     : ImplConfig_Access:=null;
         ChildNodes     : ConfigNode_Access:=null;
         Next           : ConfigNode_Access:=null;
      end record;

end Config;
