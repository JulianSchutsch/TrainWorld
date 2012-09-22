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

-- Implementation details
--
--    Basic fifo streaming:
--
--    The stream component of this implementation is separated into two parts:
--      * Blocks of memory with stream content
--      * A protected queue of blocks
--    There is always one current block in a WriteStream which is written to,
--    and there may be one current block in a ReadStream from which is read.
--    Once a block is filled to its maximum amount its added to the queue
--    of blocks and a new block is created for further writting.
--    The ReadStream reads the current block until its end and then disposes
--    the memory. It only asks for a new block from the queue if necessary.
--
--    Server client address space and connecting:
--
--    The protected ServerList is responsible for maintaining a threadsafe
--    list of Server_Access entries.
--    Every connection attempt is started within the protection of ServerList
--    using its Connect method which adds a half connection to a server
--    on the clients behalf. These half connections are aknowledged by
--    the server by calling ServerList.ProcessConnections.
--    This method makes sure that no server delete or client delete during
--    the connection process can lead to unexpected exceptions since there is
--    no direct reference between the server and the client involved.
--
--    Server client communication:
--
--    All communication is over a shared component called BlockPipes.
--    There is one pipe for each direction in BlockPipes.
--    The ReadStream_Type requires a RollBack and a StoreBuffer to keep
--    blocks available which have been read, but must be read again due to
--    a Streams.StreamOverflow exception.
--
--    Server client disconnect:
--
--    The shared component BlockPipes contains a protected counter therefore
--    BlockPipes is reference counted.
--    Since no reference between server and client exists this solves
--    all disconnect issues. The counter can be used to check for one side
--    disconnect.
--
pragma Ada_2012;

with Bytes;
with System;
with Ada.Unchecked_Deallocation;
with Basics; use Basics;
with GlobalLoop;
with ProtectedBasics;
with Ada.Finalization;

package body ClientServerStream.SMPipe is

   type ClientState_Enum is
     (ClientStateJustConnected,
      ClientStateConnected,
      ClientStateJustFailedConnect,
      ClientStateFailedConnect,
      ClientStateDisconnected);

   BlockSize          : constant:=1024*256;
   ImplementationName : constant Unbounded_String:=U("Pipe");

   type Block_Type;
   type Block_Access is access all Block_Type;

   type Block_Type is new Ada.Finalization.Limited_Controlled with
      record
         Data   : Bytes.Byte_ArrayAccess:=null;
         Amount : Streams.StreamSize_Type:=0;
         NextRollBack : Block_Access:=null;
      end record;

   overriding
   procedure Finalize
     (Block : in out Block_Type);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Block_Type,
      Name   => Block_Access);
   ---------------------------------------------------------------------------

   procedure Finalize
     (Block : in out Block_Type) is
   begin
      Bytes.Free(Block.Data);
   end Finalize;
   ---------------------------------------------------------------------------

   package BlockQueue is new ProtectedBasics.Queue(Block_Access,null);

   type Server_Config is new Config.Config_Type with
      record
         BufferSize  : Streams.StreamSize_Type;
         Address     : Unbounded_String;
      end record;
   type Server_ConfigAccess is access all Server_Config;
   ---------------------------------------------------------------------------

   type Client_Config is new Config.Config_Type with
      record
         ClientAddress : Unbounded_String;
         ServerAddress : Unbounded_String;
      end record;
   type Client_ConfigAccess is access all Client_Config;
   ---------------------------------------------------------------------------

   type BlockPipes_Type is new Ada.Finalization.Limited_Controlled with
      record
         ToServer   : aliased BlockQueue.Queue_Type;
         FromServer : aliased BlockQueue.Queue_Type;
         Counter    : ProtectedBasics.Counter_Type;
      end record;
   type BlockPipes_Access is access all BlockPipes_Type;

   overriding
   procedure Initialize
     (BlockPipes : in out BlockPipes_Type);

   overriding
   procedure Finalize
     (BlockPipes : in out BlockPipes_Type);

   procedure Release
     (BlockPipes : in out BlockPipes_Access);
   ---------------------------------------------------------------------------

   procedure Release
     (BlockPipes : in out BlockPipes_Access) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => BlockPipes_Type,
         Name   => BlockPipes_Access);
      ------------------------------------------------------------------------

      Count : Integer;

   begin
      BlockPipes.Counter.Decrement(Count);
      if Count=0 then
         Free(BlockPipes);
      else
         BlockPipes:=null;
      end if;
   end Release;
   ---------------------------------------------------------------------------

   procedure Finalize
     (BlockPipes : in out BlockPipes_Type) is

      Block : Block_Access;

   begin

      loop
         BlockPipes.ToServer.Get(Block);
         exit when Block=null;
         Free(Block);
      end loop;

      loop
         BlockPipes.FromServer.Get(Block);
         exit when Block=null;
         Free(Block);
      end loop;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Initialize
     (BlockPipes : in out BlockPipes_Type) is
   begin
      BlockPipes.Counter.Set(2);
   end Initialize;
   ---------------------------------------------------------------------------

   type PipeReadStream_Type is new Streams.ReadStream_Interface with
      record
         Buffer           : Block_Access:=null;
         Position         : Streams.StreamSize_Type:=0;
         SourceBlockPipe  : BlockQueue.Queue_Access:=null;
         RollBackFirst    : Block_Access:=null;
         RollBackLast     : Block_Access:=null;
         RollBackPosition : Streams.StreamSize_Type;
         StoredFirst      : Block_Access:=null;
         StoredLast       : Block_Access:=null;
      end record;
   type PipeReadStream_Access is access all PipeReadStream_Type;

   overriding
   procedure ReadBuffer
     (Stream     : in out PipeReadStream_Type;
      Buffer     : System.Address;
      BufferSize : Streams.StreamSize_Type);

   overriding
   procedure Finalize
     (Stream : in out PipeReadStream_Type);

   pragma Warnings(Off);
   -- Dispatching is not important here
   function Empty
     (Stream : in PipeReadStream_Type)
      return Boolean;

   procedure RollBack
     (Stream : in out PipeReadStream_Type);

   procedure BeginRead
     (Stream : in out PipeReadStream_Type);

   procedure EndRead
     (Stream : in out PipeReadStream_Type);

   pragma Warnings(On);
   ---------------------------------------------------------------------------

   type PipeWriteStream_Type is new Streams.WriteStream_Interface with
      record
         Buffer        : Block_Access     := null;
         DestBlockPipe : BlockQueue.Queue_Access := null;
      end record;
   type PipeWriteStream_Access is access all PipeWriteStream_Type;

   overriding
   procedure WriteBuffer
     (Stream     : in out PipeWriteStream_Type;
      Buffer     : System.Address;
      BufferSize : Streams.StreamSize_Type);

   overriding
   procedure Flush
     (Stream : in out PipeWriteStream_Type);

   overriding
   procedure Initialize
     (Stream : in out PipeWriteStream_Type);

   overriding
   procedure Finalize
     (Stream : in out PipeWriteStream_Type);
   ---------------------------------------------------------------------------

   procedure RollBack
     (Stream : in out PipeReadStream_Type) is

   begin

      -- Add current Stream.Buffer to rollback
      pragma Assert((Stream.RollBackFirst/=null and Stream.RollBackLast/=null)
                    or (Stream.RollBackFirst=null and Stream.RollBackLast=null));

      if Stream.Buffer/=null then

         Stream.Buffer.NextRollBack:=null;
         if Stream.RollBackLast/=null then
            Stream.RollBackLast.NextRollBack:=Stream.Buffer;
         else
            Stream.RollBackFirst:=Stream.Buffer;
         end if;
         Stream.RollBackLast:=Stream.Buffer;

      end if;

      pragma Assert((Stream.RollBackFirst/=null and Stream.RollBackLast/=null)
                    or (Stream.RollBackFirst=null and Stream.RollBackLast=null));

      -- Extract first Buffer from RollBack
      Stream.Buffer:=Stream.RollBackFirst;

      if Stream.RollBackFirst/=null then
         Stream.RollBackFirst:=Stream.RollBackFirst.NextRollBack;
         if Stream.RollBackFirst=null then
            Stream.RollBackLast:=null;
         end if;
      end if;

      pragma Assert((Stream.RollBackFirst/=null and Stream.RollBackLast/=null)
                    or (Stream.RollBackFirst=null and Stream.RollBackLast=null));

      -- Move RollBacks to beginning of the Storage
      if Stream.RollBackLast/=null then

         Stream.RollBackLast.NextRollBack:=Stream.StoredFirst;

         if Stream.StoredFirst=null then
            Stream.StoredLast:=Stream.RollBackLast;
         end if;

         Stream.StoredFirst := Stream.RollBackFirst;

      end if;

      Stream.RollBackFirst := null;
      Stream.RollBackLast  := null;

      Stream.Position    := Stream.RollBackPosition;

   end RollBack;
   ---------------------------------------------------------------------------

   procedure BeginRead
     (Stream : in out PipeReadStream_Type) is
   begin

      Stream.RollBackPosition:=Stream.Position;

   end BeginRead;
   ---------------------------------------------------------------------------

   procedure EndRead
     (Stream : in out PipeReadStream_Type) is

      Current : Block_Access;
      Next    : Block_Access;

   begin

      Current:=Stream.RollBackFirst;
      while Current/=null loop
         Next:=Current.NextRollBack;
         if Next=null then
            pragma Assert(Current=Stream.RollBackLast);
         end if;
         Free(Current);
         Current:=Next;
      end loop;
      Stream.RollBackFirst:=null;
      Stream.RollBackLast:=null;

   end EndRead;
   ---------------------------------------------------------------------------

   function Empty
     (Stream : in PipeReadStream_Type)
      return Boolean is
      use type BlockQueue.Queue_Access;
      use type Streams.StreamSize_Type;
   begin
      pragma Assert(Stream.SourceBlockPipe/=null);
      if Stream.Buffer/=null then
         pragma Assert(Stream.Position<Stream.Buffer.Amount);
         return False;
      end if;
      if Stream.StoredFirst/=null then
         return False;
      end if;
      return Stream.SourceBlockPipe.Empty;
   end Empty;
   ---------------------------------------------------------------------------

   procedure ReadBuffer
     (Stream     : in out PipeReadStream_Type;
      Buffer     : System.Address;
      BufferSize : Streams.StreamSize_Type) is

      use type Streams.StreamSize_Type;
      use type Bytes.Byte_Access;

      RemainingAmount : Streams.StreamSize_Type;
      ReadAmount      : Streams.StreamSize_Type;
      Pointer         : Bytes.Byte_Access:=Bytes.AddressToByteAccess(Buffer);

      procedure NextBlock is
      begin

         -- First check if there are stored blocks
         if Stream.StoredFirst/=null then
            Stream.Buffer:=Stream.StoredFirst;
            Stream.StoredFirst:=Stream.StoredFirst.NextRollBack;
            if Stream.StoredFirst=null then
               Stream.StoredLast:=null;
            end if;
            return;
         end if;

         -- Get block from protected queue
         Stream.SourceBlockPipe.Get(Stream.Buffer);
         if Stream.Buffer=null then
            raise Streams.StreamOverflow;
            end if;

      end NextBlock;
      ------------------------------------------------------------------------

      procedure PushRollBack is
      begin
         pragma Assert(Stream.Buffer/=null);
         pragma Assert((Stream.RollBackFirst/=null and Stream.RollBackLast/=null)
                       or (Stream.RollBackFirst=null and Stream.RollBackLast=null));

         Stream.Buffer.NextRollBack:=null;

         if Stream.RollBackLast/=null then
            Stream.RollBackLast.NextRollBack:=Stream.Buffer;
         else
            Stream.RollBackFirst:=Stream.Buffer;
         end if;

         Stream.RollBackLast:=Stream.Buffer;

         pragma Assert((Stream.RollBackFirst/=null and Stream.RollBackLast/=null)
                       or (Stream.RollBackFirst=null and Stream.RollBackLast=null));

         Stream.Buffer:=null;
         Stream.Position:=0;

      end PushRollBack;
      ------------------------------------------------------------------------

   begin

      pragma Assert((Stream.RollBackFirst/=null and Stream.RollBackLast/=null)
                    or (Stream.RollBackFirst=null and Stream.RollBackLast=null));

      if BufferSize=0 then
         return;
      end if;

      -- Ensure Buffer is filled with something
      if Stream.Buffer=null then
         NextBlock;
      end if;

      RemainingAmount:=BufferSize;
      loop

         pragma Assert(Stream.Buffer.Amount/=Stream.Position);
         -- Read as much as possible
         ReadAmount:=Streams.StreamSize_Type'Min(RemainingAmount,Stream.Buffer.Amount-Stream.Position);
         for i in 0..ReadAmount-1 loop
            Pointer.all:=Stream.Buffer.Data(Integer(Stream.Position));
            Stream.Position := Stream.Position+1;
            Pointer         := Pointer+1;
         end loop;

         RemainingAmount:=RemainingAmount-ReadAmount;
         exit when RemainingAmount=0;

         PushRollBack;
         NextBlock;

      end loop;

      pragma Assert(Stream.Position<=Stream.Buffer.Amount);

      if Stream.Position=Stream.Buffer.Amount then
         PushRollBack;
      end if;

   end ReadBuffer;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Stream : in out PipeReadStream_Type) is
   begin
      Free(Stream.Buffer);
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Stream : in out PipeWriteStream_Type) is
   begin
      Stream.Buffer:=new Block_Type;
      Stream.Buffer.Data:=new Bytes.Byte_Array(0..BlockSize-1);
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Stream : in out PipeWriteStream_Type) is
   begin
      Free(Stream.Buffer);
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Flush
     (Stream : in out PipeWriteStream_Type) is

      use type Streams.StreamSize_Type;

   begin

      if (Stream.Buffer/=null) and then (Stream.Buffer.Amount/=0) then
         Stream.DestBlockPipe.Put(Stream.Buffer);
         Stream.Buffer:=new Block_Type;
         Stream.Buffer.Data:=new Bytes.Byte_Array(0..BlockSize-1);
      end if;

   end Flush;
   ---------------------------------------------------------------------------

   procedure WriteBuffer
     (Stream     : in out PipeWriteStream_Type;
      Buffer     : System.Address;
      BufferSize : Streams.StreamSize_Type) is

      use type Bytes.Byte_Access;
      use type Streams.StreamSize_Type;

      RemainingAmount : Streams.StreamSize_Type:=BufferSize;
      WriteAmount     : Streams.StreamSize_Type;
      Pointer         : Bytes.Byte_Access:=Bytes.AddressToByteAccess(Buffer);

   begin

      pragma Assert(Pointer/=null);
      loop
         -- Write as much as possible
         exit when RemainingAmount=0;
         WriteAmount:=Streams.StreamSize_Type'Min(RemainingAmount,Stream.Buffer.Data'Length-Stream.Buffer.Amount);
         for i in 0..WriteAmount-1 loop
            Stream.Buffer.Data(Integer(Stream.Buffer.Amount)):=Pointer.all;
            Stream.Buffer.Amount:=Stream.Buffer.Amount+1;
            Pointer:=Pointer+1;
         end loop;
         RemainingAmount:=RemainingAmount-WriteAmount;
         exit when RemainingAmount=0;

         Stream.DestBlockPipe.Put(Stream.Buffer);

         Stream.Buffer:=new Block_Type;
         Stream.Buffer.Data:=new Bytes.Byte_Array(0..BlockSize-1);

      end loop;

   end WriteBuffer;
   ---------------------------------------------------------------------------

   type ServerConnection_Type;
   type ServerConnection_Access is access all ServerConnection_Type;
   type ServerConnection_Type is
      record
         BlockPipes    : BlockPipes_Access:=null;
         ClientAddress : Unbounded_String;
         ReceiveState  : StateCallBack_ClassAccess:=null;
         ReadStreamP   : PipeReadStream_Access:=null;
         ReadStream    : Streams.ReadStream_Ref;
         Next          : ServerConnection_Access:=null;
         Last          : ServerConnection_Access:=null;
         CallBack      : ConnectionCallBack_ClassAccess:=null;
         WriteStream   : Streams.WriteStream_Ref;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ServerConnection_Type,
      Name   => ServerConnection_Access);
   ---------------------------------------------------------------------------

   type Server_Type;
   type Server_Access is access all Server_Type;

   type Server_Process is new GlobalLoop.Process_Type with
      record
         Server : Server_Access:=null;
      end record;

   overriding
   procedure Process
     (P : in out Server_Process);
   ---------------------------------------------------------------------------

   type Server_Type is new Server_Interface with
      record
         Address        : Unbounded_String;
         Connections    : ServerConnection_Access:=null;
         -- Connections which require an "accept" call to the user
         NewConnections : ServerConnection_Access:=null;
         -- This must never be accessed outside the protected object ServerList
         -- It depends on the global synchronisation since other threads
         -- may add connections at any time.
         HalfConnections : ServerConnection_Access:=null;
         Next            : Server_Access:=null;
         Last            : Server_Access:=null;
         LoopProcess     : Server_Process;
      end record;

   overriding
   procedure Finalize
     (Server : in out Server_Type);
   ---------------------------------------------------------------------------

   type Client_Type;
   type Client_Access is access all Client_Type;

   type Client_Process is new GlobalLoop.Process_Type with
      record
         Client     : Client_Access:=null;
      end record;

   overriding
   procedure Process
     (P : in out Client_Process);
   ---------------------------------------------------------------------------

   type Client_Type is new Client_Interface with
      record
         BlockPipes   : BlockPipes_Access:=null;
         State        : ClientState_Enum:=ClientStateDisconnected;
         LoopProcess  : Client_Process;
         ReceiveState : StateCallBack_ClassAccess:=null;
         ReadStreamP  : PipeReadStream_Access:=null;
         ReadStream   : Streams.ReadStream_Ref;
         WriteStream  : Streams.WriteStream_Ref;
      end record;

   overriding
   procedure Finalize
     (Client : in out Client_Type);
   ---------------------------------------------------------------------------

   protected type ServerList_Type is

      procedure Add
        (Server : Server_Access);
      procedure Remove
        (Server : Server_Access);
      procedure Connect
        (Client        : Client_Access;
         ClientAddress : Unbounded_String;
         ServerAddress : Unbounded_String;
         Success       : out Boolean);
      procedure ProcessConnect
        (Server : Server_Access);

   private
      First : Server_Access;
   end ServerList_Type;

   protected body ServerList_Type is

      function Exists
        (Address : Unbounded_String)
         return Boolean is

         Server : Server_Access:=First;

      begin

         while Server/=null loop
            if Server.Address=Address then
               return True;
            end if;
            Server:=Server.Next;
         end loop;
         return False;

      end Exists;
      ------------------------------------------------------------------------

      procedure Add
        (Server : Server_Access) is
      begin

         if Exists(Server.Address) then
            raise AddressAllreadyInUse;
         end if;

         Server.Next:=First;
         if First/=null then
            First.Last:=Server;
         end if;
         First:=Server;

      end Add;
      ------------------------------------------------------------------------

      procedure Remove
        (Server : Server_Access) is
      begin

         if Server.Next/=null then
            Server.Next.Last:=Server.Last;
         end if;
         if Server.Last/=null then
            Server.Last.Next:=Server.Next;
         else
            First:=Server.Next;
         end if;
         Server.Next:=null;
         Server.Last:=null;

      end Remove;
      ------------------------------------------------------------------------

      -- Called only by clients!
      procedure Connect
        (Client        : Client_Access;
         ClientAddress : Unbounded_String;
         ServerAddress : Unbounded_String;
         Success       : out Boolean) is

         Server : Server_Access:=First;

      begin

         -- Find the server by address
         while Server/=null loop
            if Server.Address=ServerAddress then
               exit;
            end if;
            Server:=Server.Next;
         end loop;

         if Server=null then
            Success:=False;
            return;
         end if;

         -- Register a half connection to the server
         declare
            Connection : constant ServerConnection_Access:=new ServerConnection_Type;
         begin

            Connection.BlockPipes:=new BlockPipes_Type;
            Connection.ClientAddress:=ClientAddress;

            Client.BlockPipes:=Connection.BlockPipes;

            Connection.ReadStreamP:=new PipeReadStream_Type;
            Connection.ReadStream:=Streams.ReadStreamRef.MakeNewRef(Streams.ReadStream_ClassAccess(Connection.ReadStreamP));
            Connection.ReadStreamP.SourceBlockPipe:=Connection.BlockPipes.ToServer'Access;

            Client.ReadStreamP:=new PipeReadStream_Type;
            Client.ReadStream:=Streams.ReadStreamRef.MakeNewRef(Streams.readStream_ClassAccess(Client.ReadStreamP));
            Client.ReadStreamP.SourceBlockPipe:=Client.BlockPipes.FromServer'Access;

            Connection.Next:=Server.HalfConnections;
            if Server.HalfConnections/=null then
               Server.HalfConnections.Last:=Connection;
            end if;
            Server.HalfConnections:=Connection;
         end;

         Success:=True;

      end Connect;
      ------------------------------------------------------------------------

      -- called only by servers!
      procedure ProcessConnect
        (Server : Server_Access) is

         Connection     : ServerConnection_Access;
         NextConnection : ServerConnection_Access;

      begin
         -- Move all half connections to the list of new connections
         Connection:=Server.HalfConnections;

         while Connection/=null loop

            NextConnection  := Connection.Next;
            Connection.Next := Server.NewConnections;
            Connection.Last := null;
            if Server.NewConnections/=null then
               Server.NewConnections.Last:=Connection;
            end if;
            Server.NewConnections:=Connection;
            Connection:=NextConnection;

         end loop;

         Server.HalfConnections:=null;

      end ProcessConnect;
      ------------------------------------------------------------------------

   end ServerList_Type;
   ---------------------------------------------------------------------------

   ServerList : ServerList_Type;

   procedure Process
     (P : in out Server_Process) is

      use type ServerCallBack_ClassAccess;
      use type ConnectionCallBack_ClassAccess;

      Server : Server_Access renames P.Server;

   begin
      pragma Assert(P.Server/=null);
      ServerList.ProcessConnect(Server);
      if Server.CallBack=null then
         return;
      end if;

      -- Process new connections
      if Server.NewConnections/=null then
         declare
            Connection     : ServerConnection_Access;
            NextConnection : ServerConnection_Access;
         begin
            Connection:=Server.NewConnections;
            while Connection/=null loop

               NextConnection:=Connection.Next;
               Connection.Next:=Server.Connections;
               Connection.Last:=null;
               if Server.Connections/=null then
                  Server.Connections.Last:=Connection;
               end if;
               Server.Connections:=Connection;

               -- Ask for connection callback
               Connection.CallBack:=Server.CallBack.NetworkAccept(Connection.ClientAddress);
               pragma Assert(Connection.CallBack/=null);

               -- Create and report WriteStream to CallBack+Get receive state
               declare
                  WriteStream : constant PipeWriteStream_Access:=new PipeWriteStream_Type;
               begin
                  Connection.WriteStream:=Streams.WriteStreamRef.MakeNewRef(Streams.WriteStream_ClassAccess(WriteStream));
                  WriteStream.DestBlockPipe:=Connection.BlockPipes.FromServer'Access;
               end;

               Connection.ReceiveState:=Connection.CallBack.NetworkConnect(Connection.WriteStream);

               Connection:=NextConnection;
            end loop;
            Server.NewConnections:=null;
         end;
      end if;

      declare
         Connection     : ServerConnection_Access;
         NextConnection : ServerConnection_Access;
      begin
         -- TODO: Wonder if someone could try to free connections
         --       from the callback while this runs!
         --       Not possible, but a disconnect call should be added
         Connection:=Server.Connections;
         while Connection/=null loop
            NextConnection:=Connection.Next;

            if Connection.BlockPipes.Counter.Get=1 then

               Connection.CallBack.NetworkDisconnect;

               Release(Connection.BlockPipes);

               if Connection.Last/=null then
                  Connection.Last.Next:=Connection.Next;
               else
                  Server.Connections:=Connection.Next;
               end if;
               if Connection.Next/=null then
                  Connection.Next.Last:=Connection.Last;
               end if;
               Free(Connection);

            else

               while not Empty(Connection.ReadStreamP.all) loop
                  begin
                     BeginRead(Connection.ReadStreamP.all);
                     Connection.ReceiveState:=Connection.ReceiveState.NetworkReceive(Connection.ReadStream.I.all);
                     EndRead(Connection.ReadStreamP.all);
                  exception
                     when Streams.StreamOverflow =>
                        RollBack(Connection.ReadStreamP.all);
                  end;
               end loop;

               Connection.WriteStream.I.Flush;

            end if;
            Connection:=NextConnection;
         end loop;
      end;

   end Process;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Client : in out Client_Type) is
   begin

      if Client.BlockPipes/=null then
         Release(Client.BlockPipes);
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Process
     (P : in out Client_Process) is

      Client : Client_Access renames P.Client;

   begin

      pragma Assert(P.Client/=null);

      if Client.CallBack=null then
         return;
      end if;

      if Client.State=ClientStateJustConnected then
         declare
            WriteStream : constant PipeWriteStream_Access:=new PipeWriteStream_Type;
         begin
            WriteStream.DestBlockPipe:=Client.BlockPipes.ToServer'Access;
            Client.WriteStream:=Streams.WriteStreamRef.MakeNewRef(Streams.WriteStream_ClassAccess(WriteStream));
         end;
         Client.ReceiveState:=Client.CallBack.NetworkConnect(Client.WriteStream);
         pragma Assert(Client.ReceiveState/=null);
         Client.State:=ClientStateConnected;
      end if;

      if Client.State=ClientStateJustFailedConnect then
         Client.CallBack.NetworkFailedConnect;
         Client.State:=ClientStateFailedConnect;
      end if;

      if Client.BlockPipes=null then
         return;
      end if;

      if Client.BlockPipes.Counter.Get=1 then
         Client.CallBack.NetworkDisconnect;
         Release(Client.BlockPipes);
         return;
      end if;

      while not Empty(Client.ReadStreamP.all) loop
         begin
            Beginread(Client.ReadStreamP.all);
            Client.ReceiveState:=Client.ReceiveState.NetworkReceive(Client.ReadStream.I.all);
            EndRead(Client.ReadStreamP.all);
         exception
            when Streams.StreamOverflow =>
               RollBack(Client.ReadStreamP.all);
         end;
      end loop;
      Client.WriteStream.I.Flush;

   end Process;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Server : in out Server_Type) is
      Connection     : ServerConnection_Access;
      NextConnection : ServerConnection_Access;
   begin

      -- Ensure no more halfconnection and newconnection are there by lifting them
      -- to full connections
      Server.LoopProcess.Process;
      -- Then free all connections
      Connection:=Server.Connections;
      while Connection/=null loop
         NextConnection:=Connection.Next;
         if Connection.BlockPipes/=null then
            Connection.CallBack.NetworkDisconnect;
            Release(Connection.BlockPipes);
         end if;
         Free(Connection);
         Connection:=NextConnection;
      end loop;
      Server.Connections:=null;

      ServerList.Remove(Server'Unrestricted_Access);

   end Finalize;
   ---------------------------------------------------------------------------

   function ServerConstructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess)
      return Server_Ref is

      pragma Unreferenced(GenConfig);

      Server  : constant Server_Access := new Server_Type;
      IConfig : constant Server_ConfigAccess := Server_ConfigAccess(ImplConfig);

   begin

      if IConfig=null then
         raise Config.MissingConfig;
      end if;
      Server.Address:=IConfig.Address;
      Server.LoopProcess.Server:=Server;
      Server.LoopProcess.Enable;
      ServerList.Add(Server);
      return R:Server_Ref do
         R.I:=Server_ClassAccess(Server);
      end return;

   end ServerConstructor;
   ---------------------------------------------------------------------------

   function ClientConstructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess)
      return Client_Ref is

      pragma Unreferenced(GenConfig);

      Client  : constant Client_Access:=new Client_Type;
      IConfig : constant Client_ConfigAccess := Client_ConfigAccess(ImplConfig);
      Success : Boolean;

   begin

      if IConfig=null then
         raise Config.MissingConfig;
      end if;
      Client.LoopProcess.Client:=Client;
      Client.LoopProcess.Enable;
      ServerList.Connect
        (Client        => Client,
         ClientAddress => IConfig.ClientAddress,
         ServerAddress => IConfig.ServerAddress,
         Success       => Success);
      if not Success then
         Client.State:=ClientStateJustFailedConnect;
      else
         Client.State:=ClientStateJustConnected;
      end if;
      return R:Client_Ref do
         R.I:=Client_ClassAccess(Client);
      end return;

   end ClientConstructor;
   ---------------------------------------------------------------------------

   procedure CreateServerConfig
     (Configuration : in out Config.ConfigNode_Type;
      Address       : Unbounded_String;
      BufferSize    : Streams.StreamSize_Type:=1024) is
   begin

      Configuration.SetImplConfig
        (ImplementationName,
         new Server_Config'(
           BufferSize => BufferSize,
           Address    => Address));

   end CreateServerConfig;
   ---------------------------------------------------------------------------

   procedure CreateClientConfig
     (Configuration : in out Config.ConfigNode_Type;
      ClientAddress : Unbounded_String;
      ServerAddress : Unbounded_String) is

   begin

      Configuration.SetImplConfig
        (ImplementationName,
         new Client_Config'(
           ClientAddress => ClientAddress,
           ServerAddress => ServerAddress));

   end CreateClientConfig;
   ---------------------------------------------------------------------------

   procedure Register is
   begin
      ServerImplementations.Register(ImplementationName,ServerConstructor'Access);
      ClientImplementations.Register(ImplementationName,ClientConstructor'Access);
   end Register;
   ---------------------------------------------------------------------------

end ClientServerStream.SMPipe;
