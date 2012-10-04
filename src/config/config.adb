-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
--
--   Trainworld is free software: you can redistribute it and/or modify
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

pragma Ada_2012;

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Config is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Config_Type'Class,
      Name   => Config_ClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ImplConfig_Type,
      Name   => ImplConfig_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ConfigNode_Type,
      Name   => ConfigNode_Access);

   procedure DebugTree
     (ConfigNode : ConfigNode_Type) is

      procedure DebugSubTree
        (Node   : ConfigNode_Access;
         Prefix : String) is

         p : ConfigNode_Access:=Node.ChildNodes;
         next : ConfigNode_Access:=Node.ChildNodes;

      begin
         Put_Line(Prefix&To_String(Node.Name));
         while p/=null loop
            next := p.Next;
            if next/=null then
               DebugSubTree(p,Prefix&" | ");
            else
               DebugSubTree(p,Prefix&"   ");
            end if;
            p := next;
         end loop;
      end DebugSubTree;

   begin
      DebugSubTree(ConfigNode'Unrestricted_Access," ");
   end DebugTree;
   ---------------------------------------------------------------------------

   procedure SetImplementation
     (ConfigNode     : in out ConfigNode_Type;
      Implementation : Unbounded_String) is
   begin
      ConfigNode.Implementation:=Implementation;
   end SetImplementation;
   ---------------------------------------------------------------------------

   function GetConfig
     (ConfigNode : ConfigNode_Type)
      return Config_ClassAccess is
   begin
      return ConfigNode.Config;
   end GetConfig;
   ---------------------------------------------------------------------------

   function GetImplementation
     (ConfigNode : ConfigNode_Type)
      return Unbounded_String is
   begin
      return ConfigNode.Implementation;
   end GetImplementation;
   ---------------------------------------------------------------------------

   function GetImplConfig
     (ConfigNode     : ConfigNode_Type;
      Implementation : Unbounded_String)
      return Config_ClassAccess is

      p : ImplConfig_Access:=ConfigNode.ImplConfig;

   begin

      while p/=null loop

         if p.Implementation=Implementation then
            return p.Config;
         end if;

         p:=p.Next;

      end loop;

      return null;

   end GetImplConfig;
   ---------------------------------------------------------------------------

   function GetName
     (ConfigNode : ConfigNode_Type)
      return Unbounded_String is
   begin
      return ConfigNode.Name;
   end GetName;
   ---------------------------------------------------------------------------

   function GetDeepChildNodeCount
     (ConfigNode : ConfigNode_Type)
      return Natural is

      p     : ConfigNode_Access:=ConfigNode.ChildNodes;
      Count : Natural:=0;

   begin

      while p/=null loop
         Count:=Count+p.GetDeepChildNodeCount+1;
         p:=p.Next;
      end loop;

      return Count;

   end GetDeepChildNodeCount;
   ---------------------------------------------------------------------------

   function GetChildNodeCount
     (ConfigNode : ConfigNode_Type)
      return Natural is

      p     : ConfigNode_Access:=ConfigNode.ChildNodes;
      Count : Natural:=0;

   begin

      while p/=null loop
         p:=p.Next;
         Count:=Count+1;
      end loop;

      return Count;

   end GetChildNodeCount;
   ---------------------------------------------------------------------------

   procedure Finalize
     (ConfigNode : in out ConfigNode_Type) is
   begin

      if ConfigNode.Config/=null then
         Free(ConfigNode.Config);
      end if;

      -- Remove all Implementation specific configurations
      declare
         p    : ImplConfig_Access;
         next : ImplConfig_Access;
      begin

         p:=ConfigNode.ImplConfig;
         while p/=null loop

            next:=p.Next;

            if p.Config/=null then
               Free(p.Config);
            end if;
            Free(p);

            p:=next;
         end loop;

      end;

      -- Remove all children
      declare
         p : ConfigNode_Access;
         next : ConfigNode_access;
      begin

         p:=ConfigNode.ChildNodes;
         while p/=null loop
            next:=p.Next;
            Free(p);
            p:=next;
         end loop;

         ConfigNode.ChildNodes:=null;

      end;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure SetConfig
     (ConfigNode : in out ConfigNode_Type;
      Config     : Config_ClassAccess) is
   begin

      if ConfigNode.Config/=null then
         Free(ConfigNode.Config);
      end if;
      ConfigNode.Config:=Config;

   end SetConfig;
   ---------------------------------------------------------------------------

   procedure SetImplConfig
     (ConfigNode     : in out ConfigNode_Type;
      Implementation : Unbounded_String;
      Config         : Config_ClassAccess) is
   begin
      -- Check if there is allready an entry for this implementation
      -- If so, replace the entry.
      declare
         p : ImplConfig_Access:=ConfigNode.ImplConfig;
      begin

         while p/=null loop

            if p.Implementation=Implementation then
               if p.Config/=null then
                  Free(p.Config);
               end if;
               p.Config:=Config;
               return;
            end if;

            p:=p.Next;

         end loop;

      end;

      -- Append new Implementation
      declare
         n : constant ImplConfig_Access:=new ImplConfig_Type;
      begin
         n.Next                := ConfigNode.ImplConfig;
         n.Implementation      := Implementation;
         n.Config              := Config;
         ConfigNode.ImplConfig := n;
      end;

   end SetImplConfig;
   ---------------------------------------------------------------------------

   function GetPath
     (ConfigNode : in out ConfigNode_Type;
      Path       : ConfigPath_Type)
      return ConfigNode_Access is

      p : ConfigNode_Access;

   begin

      if Path'Length=0 then
         return ConfigNode'Unrestricted_Access;
      end if;

      p:=ConfigNode.ChildNodes;

      while p/=null loop

         if p.Name=Path(Path'First) then
            return p.GetPath(Path(Path'First+1..Path'Last));
         end if;
         p:=p.Next;

      end loop;

      return null;

   end GetPath;
   ---------------------------------------------------------------------------

   function CreatePath
     (ConfigNode : in out ConfigNode_Type;
      Path       : ConfigPath_Type)
      return ConfigNode_Access is

      First : ConfigNode_Access;

   begin

      if Path'Length=0 then
         return ConfigNode'Unrestricted_Access;
      end if;

      First:=ConfigNode.GetPath(Path(Path'First..Path'First));
      if First=null then
         First      := new ConfigNode_Type;
         First.Name := Path(Path'First);
         First.Next := ConfigNode.ChildNodes;
         ConfigNode.ChildNodes:=First;
      end if;

      return First.CreatePath(Path(Path'First+1..Path'Last));

   end CreatePath;
   ---------------------------------------------------------------------------

end Config;
