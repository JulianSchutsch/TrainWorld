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

with Ada.Unchecked_Deallocation;
with Basics; use Basics;

package body Config is

   function SubSelect
     (Node   : ConfigNode_Access;
      Str    : Unbounded_String;
      Create : Boolean)
      return ConfigNode_Access is

      SubNode : ConfigNode_Access;

   begin

      if Node=null then
         return null;
      end if;

      SubNode:=Node.Childs;

      while SubNode/=null loop
         if SubNode.Name=Str then
            return SubNode;
         end if;
         SubNode := SubNode.NextSibling;
      end loop;

      if Create then
         SubNode             := new ConfigNode_Type;
         SubNode.Name        := Str;
         SubNode.NextSibling := Node.Childs;
         Node.Childs         := SubNode;
      end if;
      return SubNode;

   end SubSelect;
   ------------------------------------------------------------------------

   function GetSubNode
     (Node   : ConfigNode_Access;
      Path   : Unbounded_String;
      Create : Boolean)
      return ConfigNode_Access is

      WordStart : Positive:=1;
      CurrentNode : ConfigNode_Access;

   begin

      CurrentNode:=Node;
      for i in 1..Length(Path) loop
         if Element(Path,i)='.' then
            CurrentNode:=SubSelect(Node,Unbounded_Slice(Path,WordStart,i-1),Create);
            WordStart := i+1;
         end if;
      end loop;

      if WordStart<=Length(Path) then
         CurrentNode:=SubSelect(Node,Unbounded_Slice(Path,WordStart,Length(Path)),Create);
      end if;

      return CurrentNode;

   end;

   ---------------------------------------------------------------------------
   -- Config_Pointer --

   procedure Finalize
     (Pointer : in out Config_Pointer) is
   begin
      if Pointer.Config/=null then
         Pointer.Config.Pointers := Pointer.Config.Pointers - 1;
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Adjust
     (Pointer : in out Config_Pointer) is
   begin
      if Pointer.Config/=null then
         Pointer.Config.Pointers := Pointer.Config.Pointers + 1;
      end if;
   end Adjust;
   ---------------------------------------------------------------------------

   procedure SetValue
     (Pointer : in out Config_Pointer;
      Value   : Unbounded_String) is
   begin

      if Pointer.Node=null then
         raise PointerToNull;
      end if;
      Pointer.Node.Value:=Value;

   end SetValue;
   ---------------------------------------------------------------------------

   function GetValue
     (Pointer : in Config_Pointer)
      return Unbounded_String is
   begin

      if Pointer.Config=null then
         raise PointerToNull;
      end if;
      if Pointer.Node=null then
         return U("");
      else
         return Pointer.Node.Value;
      end if;

   end GetValue;
   ---------------------------------------------------------------------------

   function GetChildValue
     (Pointer : in Config_Pointer;
      Name    : Unbounded_String)
      return Unbounded_String is

      p : ConfigNode_Access;

   begin

      if Pointer.Node=null then
         raise ValueNotSet;
      end if;

      p:=Pointer.Node.Childs;
      while p/=null loop
         if p.Name=Name then
            return p.Value;
         end if;
         p:=p.NextSibling;
      end loop;

      raise ValueNotSet;

   end GetChildValue;
   ---------------------------------------------------------------------------

   procedure GetNode
     (Pointer : in Config_Pointer;
      SubPath : Unbounded_String;
      Result  : out Config_Pointer'Class) is
   begin

      Pointer.Config.Pointers:=Pointer.Config.Pointers+1;
      Result.Config := Pointer.Config;
      Result.Node   := GetSubNode(Pointer.Node,SubPath,False);

   end GetNode;
   ---------------------------------------------------------------------------

   -- /Config_Pointer --
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Config_Type --

   procedure GetNode
     (Config : in out Config_Type;
      Path   : Unbounded_String;
      Result : out Config_Pointer'Class;
      Create : Boolean := False) is

   begin

      Config.Pointers := Config.Pointers+1;
      Result.Config   := Config'Unrestricted_Access;
      Result.Node     := GetSubNode
        (Config.Root'Unrestricted_Access,
         Path,Create);

   end GetNode;
   ---------------------------------------------------------------------------

   procedure CreateNode
     (Config : in out Config_Type;
      Path   : Unbounded_String;
      Value  : Unbounded_String) is

      Pointer : Config_Pointer;

   begin
      Config.GetNode(Path,Pointer,True);
      Pointer.SetValue(Value);
   end CreateNode;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Config: in out Config_Type) is

      procedure Clear(Node : in out ConfigNode_Access) is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => ConfigNode_Type,
            Name   => ConfigNode_Access);

         next : ConfigNode_Access;
         p    : ConfigNode_Access;

      begin

         if Node=null then
            return;
         end if;

         p := Node.Childs;
         while p/=null loop
            next := p.NextSibling;
            Clear(p);
            p := next;
         end loop;

         Free(Node);

      end Clear;
      ------------------------------------------------------------------------

      next : ConfigNode_Access;
      p    : ConfigNode_Access;

   begin

      p:=Config.Root.Childs;
      while p/=null loop
         next := p.NextSibling;
         Clear(next);
         p := next;
      end loop;


      if Config.Pointers/=0 then
         raise PointersLeft;
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   -- /Config_Type --
   ---------------------------------------------------------------------------

end Config;
