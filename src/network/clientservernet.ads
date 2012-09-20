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
--   18.Sep 2012 Julian Schutsch
--     - Original version

pragma Ada_2012;

with Streams;
with RefCount;
with Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ClientServerNet is

   type StateCallBack_Interface is interface;
   type StateCallBack_ClassAccess is access all StateCallBack_Interface'Class;

   function NetworkReceive
     (T      : in out StateCallBack_Interface;
      Stream : in out Streams.ReadStream_Interface'Class)
      return StateCallBack_ClassAccess is abstract;

   type ConnectionCallBack_Interface is interface;
   type ConnectionCallBack_ClassAccess is access all ConnectionCallBack_Interface'Class;

   procedure NetworkDisconnect
     (T : in out ConnectionCallBack_Interface) is abstract;

   procedure NetworkFailedConnect
     (T : in out ConnectionCallBack_Interface) is abstract;

   function NetworkConnect
     (T      : in out ConnectionCallBack_Interface;
      Stream : Streams.WriteStream_Ref)
      return StateCallBack_ClassAccess is abstract;

   type ServerCallBack_Interface is interface;
   type ServerCallBack_ClassAccess is access all ServerCalLBack_Interface'Class;

   function NetworkAccept
     (T             : in out ServerCallBack_Interface;
      ClientAddress : Unbounded_String)
      return ConnectionCallBack_ClassAccess is abstract;

   type Server_Interface is new RefCount.Ref_Interface with
      record
         CallBack : ServerCallBack_ClassAccess;
      end record;

   type Server_ClassAccess is access all Server_Interface'Class;

   package ServerRef is new RefCount.Ref(Server_Interface,Server_ClassAccess);
   subtype Server_Ref is ServerRef.Ref_Type;

   type Client_Interface is new RefCount.Ref_Interface with
      record
         CallBack : ConnectionCallBack_ClassAccess:=null;
      end record;

   type Client_ClassAccess is access all Client_Interface'Class;

   package ClientRef is new RefCount.Ref(Client_Interface,Client_ClassAccess);
   subtype Client_Ref is ClientRef.Ref_Type;

   package ServerImplementations is new Standard.Implementations(Server_Ref);
   package ClientImplementations is new Standard.Implementations(Client_Ref);

   procedure X;

end ClientServerNet;
