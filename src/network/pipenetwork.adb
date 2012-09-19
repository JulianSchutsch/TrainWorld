with Bytes;
with System;
with Ada.Unchecked_Deallocation;
with Basics; use Basics;
with Network;
with GlobalLoop;
with Ada.Text_IO; use Ada.Text_IO;

package body PipeNetwork is

   protected type DualRef_Type is
      procedure Decrement
        (ReachedZero : out Boolean);
   private
      Counter : Natural:=2;
   end DualRef_Type;

   protected body DualRef_Type is
      procedure Decrement
        (ReachedZero : out Boolean) is
      begin
         Counter:=Counter-1;
         ReachedZero:=Counter=0;
      end Decrement;
   end DualRef_Type;
   ---------------------------------------------------------------------------

   type ClientState_Enum is
     (ClientStateJustConnected,
      ClientStateConnected,
      ClientStateJustFailedConnect,
      ClientStateFailedConnect,
      ClientStateDisconnected);

   BlockSize          : constant:=1024;
   ImplementationName : constant Unbounded_String:=U("Pipe");

   type Block_Type;
   type Block_Access is access all Block_Type;

   type Block_Type is
      record
         Data   : Bytes.Byte_ArrayAccess:=null;
         Amount : Streams.StreamSize_Type:=0;
         Next   : Block_Access:=null;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Block_Type,
      Name   => Block_Access);
   ---------------------------------------------------------------------------

   type Server_Config is new Config.Config_Type with
      record
         BufferSize  : Streams.StreamSize_Type;
         Address     : Unbounded_String;
      end record;
   type Server_ConfigAccess is access all Server_Config;
   ---------------------------------------------------------------------------

   type Client_Config is new Config.Config_Type with
      record
         Address : Unbounded_String;
      end record;
   type Client_ConfigAccess is access all Client_Config;
   ---------------------------------------------------------------------------

   protected type BlockPipe_Type is
      procedure Put
        (Block : not null Block_Access);
      procedure Get
        (Block : out Block_Access);
   private
      First : Block_Access:=null;
      Last  : Block_Access:=null;
   end BlockPipe_Type;
   ---------------------------------------------------------------------------

   protected body BlockPipe_Type is

      procedure Put
        (Block : not null Block_Access) is
      begin
         if Last/=null then
            Last.Next:=Block;
         end if;
         Last:=Block;
      end Put;
      ------------------------------------------------------------------------

      procedure Get
        (Block : out Block_Access) is

      begin

         Block:=First;

         if First/=null then
            First:=First.Next;
            if First=null then
               Last:=null;
            end if;
         end if;

      end Get;
      ------------------------------------------------------------------------

   end BlockPipe_Type;

   type BlockPipe_Access is access all BlockPipe_Type;

   type BlockPipes_Type is
      record
         ToServer   : aliased BlockPipe_Type;
         FromServer : aliased BlockPipe_Type;
         DualRef    : DualRef_Type;
      end record;
   type BlockPipes_Access is access all BlockPipes_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => BlockPipes_Type,
      Name   => BlockPipes_Access);

   type PipeReadStream_Type is new Streams.ReadStream_Interface with
      record
         Buffer          : Block_Access:=null;
         Position        : Streams.StreamSize_Type:=0;
         SourceBlockPipe : BlockPipe_Access:=null;
      end record;

   overriding
   procedure ReadBuffer
     (Stream     : in out PipeReadStream_Type;
      Buffer     : System.Address;
      BufferSize : Streams.StreamSize_Type);

   overriding
   procedure Finalize
     (Stream : in out PipeReadStream_Type);
   ---------------------------------------------------------------------------

   type PipeWriteStream_Type is new Streams.WriteStream_Interface with
      record
         Buffer        : Block_Access     := null;
         DestBlockPipe : BlockPipe_Access := null;
      end record;

   overriding
   procedure WriteBuffer
     (Stream     : in out PipeWriteStream_Type;
      Buffer     : System.Address;
      BufferSize : Streams.StreamSize_Type);

   overriding
   procedure Initialize
     (Stream : in out PipeWriteStream_Type);

   overriding
   procedure Finalize
     (Stream : in out PipeWriteStream_Type);
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

   begin

      if BufferSize=0 then
         return;
      end if;

      -- Ensure Buffer is filled with something
      if Stream.Buffer=null then
         Stream.SourceBlockPipe.Get(Stream.Buffer);
         Stream.Position := 0;
         if Stream.Buffer=null then
            raise Streams.StreamOverflow;
         end if;
      end if;

      RemainingAmount:=BufferSize;
      loop

         -- Read as much as possible
         ReadAmount:=Streams.StreamSize_Type'Min(RemainingAmount,Stream.Buffer.Amount-Stream.Position);
         for i in 0..ReadAmount-1 loop
            Pointer.all:=Stream.Buffer.Data(Integer(Stream.Position));
            Stream.Position := Stream.Position+1;
            Pointer         := Pointer+1;
         end loop;
         RemainingAmount:=RemainingAmount-ReadAmount;
         exit when RemainingAmount=0;

         Bytes.Free(Stream.Buffer.Data);
         Free(Stream.Buffer);

         -- Get next buffer since last had not enough data for the request
         Stream.SourceBlockPipe.Get(Stream.Buffer);
         Stream.Position := 0;
         if Stream.Buffer=null then
            raise Streams.StreamOverflow;
         end if;

      end loop;

   end ReadBuffer;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Stream : in out PipeReadStream_Type) is
   begin
      Bytes.Free(Stream.Buffer.Data);
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
      Bytes.Free(Stream.Buffer.Data);
      Free(Stream.Buffer);
   end Finalize;
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
         Next          : ServerConnection_Access:=null;
         Last          : ServerConnection_Access:=null;
         CallBack      : Network.ConnectionCallBack_ClassAccess:=null;
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

   type Server_Type is new Network.Server_Interface with
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
         Client : Client_Access:=null;
      end record;

   overriding
   procedure Process
     (P : in out Client_Process);
   ---------------------------------------------------------------------------

   type Client_Type is new Network.Client_Interface with
      record
         BlockPipes   : BlockPipes_Access:=null;
         State        : ClientState_Enum:=ClientStateDisconnected;
         LoopProcess  : Client_Process;
         ReceiveState : Network.StateCallBack_ClassAccess:=null;
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
        (Client  : Client_Access;
         Address : Unbounded_String;
         Success : out Boolean);
      procedure ProcessConnect
        (Server : Server_Access);

   private
      First : Server_Access;
   end ServerList_Type;

   protected body ServerList_Type is

      procedure Add
        (Server : Server_Access) is
      begin
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
        (Client  : Client_Access;
         Address : Unbounded_String;
         Success : out Boolean) is

         Server : Server_Access:=First;

      begin
         -- Find the server by address
         while Server.Address/=Address loop
            Server:=Server.Next;
            if Server=null then
               Success:=False;
               return;
            end if;
         end loop;

         -- Register a half connection to the server
         declare
            Connection : constant ServerConnection_Access:=new ServerConnection_Type;
         begin
            Connection.BlockPipes:=new BlockPipes_Type;

            Client.BlockPipes:=Connection.BlockPipes;

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

            NextConnection:=Connection.Next;
            Connection.Next:=Server.NewConnections;
            Connection.Last:=null;
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

      use type Network.ServerCallBack_ClassAccess;
      use type Network.ConnectionCallBack_ClassAccess;

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
            Connection     : ServerConnection_Access:=null;
            NextConnection : ServerConnection_Access:=null;
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
               Connection.CallBack:=Server.CallBack.NetworkAccept;
               pragma Assert(Connection.CallBack/=null);
               Connection:=NextConnection;
            end loop;
            Server.NewConnections:=null;
         end;
      end if;
      -- TODO : Process receive
   end Process;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Client : in out Client_Type) is
   begin
      Put_Line("Client.Finalize");
      if Client.BlockPipes/=null then
         Put_Line("Check BlockPipes");
         declare
            ReachedZero : Boolean;
         begin
            Client.BlockPipes.DualRef.Decrement(ReachedZero);
            if ReachedZero then
               Put_Line("Free BlockPipes");
               Free(Client.BlockPipes);
            end if;
         end;
         Client.BlockPipes:=null;
      end if;
      Put_Line("Client.Finalize/");
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Process
     (P : in out Client_Process) is

      use type Network.ConnectionCallBack_ClassAccess;
      use type Network.StateCallBack_ClassAccess;

      Client : Client_Access renames P.Client;

   begin
      pragma Assert(P.Client/=null);
      if Client.CallBack=null then
         return;
      end if;
      if Client.State=ClientStateJustConnected then
         -- TODO: Create a Pipestream with correct link to the protected Pipe
         Client.ReceiveState:=Client.CallBack.NetworkConnect(Streams.WriteStreamRef.MakeRef(new PipeWriteStream_Type));
         pragma Assert(Client.ReceiveState/=null);
         Client.State:=ClientStateConnected;
      end if;
      if Client.State=ClientStateJustFailedConnect then
         Client.CallBack.NetworkFailedConnect;
         Client.State:=ClientStateFailedConnect;
      end if;
      -- TODO : Process receive
   end Process;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Server : in out Server_Type) is
      Connection     : ServerConnection_Access;
      NextConnection : ServerConnection_Access;
   begin
      Put_Line("Server.Finalize");
      -- Ensure no more halfconnection and newconnection are there
      Server.LoopProcess.Process;
      Connection:=Server.Connections;
      while Connection/=null loop
         NextConnection:=Connection.Next;
         declare
            ReachedZero : Boolean;
         begin
            Connection.BlockPipes.DualRef.Decrement(ReachedZero);
            if ReachedZero then
               Free(Connection.BlockPipes);
            end if;
         end;
         Free(Connection);
         Connection:=NextConnection;
      end loop;
      Server.Connections:=null;
      ServerList.Remove(Server'Unrestricted_Access);
      Put_Line("Server.Finalize/");
   end Finalize;
   ---------------------------------------------------------------------------

   function ServerConstructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess)
      return Network.Server_Ref is

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
      return R:Network.Server_Ref do
         R.I:=Network.Server_ClassAccess(Server);
      end return;

   end ServerConstructor;
   ---------------------------------------------------------------------------

   function ClientConstructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess)
      return Network.Client_Ref is

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
      ServerList.Connect(Client,IConfig.Address,Success);
      if not Success then
         Client.State:=ClientStateJustFailedConnect;
      else
         Client.State:=ClientStateJustConnected;
      end if;
      return R:Network.Client_Ref do
         R.I:=Network.Client_ClassAccess(Client);
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
      Address       : Unbounded_String) is

      use type Network.Server_ClassAccess;

   begin

      Configuration.SetImplConfig
        (ImplementationName,
         new Client_Config'(
           Address => Address));

   end CreateClientConfig;
   ---------------------------------------------------------------------------

   procedure Register is
   begin
      Network.ServerImplementations.Register(ImplementationName,ServerConstructor'Access);
      Network.ClientImplementations.Register(ImplementationName,ClientConstructor'Access);
   end Register;
   ---------------------------------------------------------------------------

end PipeNetwork;
