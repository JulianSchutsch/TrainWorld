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

-- Usage
--   This package provides an interface to all implementations which provide
--   dual direction stream connections in a client server architecture.
--
--   Before use the client must be associated with a connection callback by
--   setting Client_Ref.I.CallBack and the server must be associated with
--   a server callback using Server_Ref.I.CallBack.
--
--   Both the client and the server hook themselves to the GlobalLoop.
--   GlobalLoop.Process must be called periodically.
--
--   Connection process:
--
--            Client                             Server
--
--      Initiate a connection           Bind to address and wait for
--    immediately after creation         connections after creation
--               |                                   |
--    Call either NetworkConnect       On connection call NetworkAccept
--     or NetworkFailedConnect        which returns a Connection-CallBack
--    NetworkConnect must return                     |
--           a state.                    Call NetworkConnection on the
--                                     Connection-CallBack which returns
--                                                a state
--
--
--  Disconnect:
--
--     For both client and server call NetworkDisconnect of the Connection
--     callback.
--
--  Sending data:
--
--     With a call to NetworkConnect a WriteStream_Ref is passed which can be
--     used to send data at any time. Sending is Not required to be threadsafe.
--     There must be no limit on the amount of data which can be send at the
--     same time.
--     The implementation is required to flush the WriteStream buffer on each
--     GlobalLoop call.
--
--  Receiving data:
--
--     Each connection is in a given state (StateCallBack_Interface).
--     Once data has been received the NetworkReceive method of the
--     current state is called.
--     One can read data from the passed ReadStream_Interface without
--     checking for presence of data in the stream, but one must follow within
--     a single NetworkReceive call:
--
--       1. Read all data first
--       2. Apply changes according to this data
--       3. Return new state
--
--     If not enough data is available in the stream, a StreamOverflow
--     exception will be thrown which is catched by network implementation
--     which must perform a rollback.
--     Once new data is received the same state is called with the same
--     position in the ReadStream as before.
--
--     The new state retured by NetworkReceive can be used to create chains
--     of states.
--
--  Failures:
--
--    If a connection failed (NetworkFailedConnect) ther must be no further
--    callbacks from the network implementation for this connection.

pragma Ada_2012;

with Streams;
with RefCount;
with Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ClientServerStream is

   AddressAllreadyInUse : Exception;

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

end ClientServerStream;
