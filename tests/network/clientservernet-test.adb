pragma Ada_2012;

with ClientServerNet;
with ClientServerNet.SMPipe;
with Config;
with Streams;
with GlobalLoop;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;
with ProtectedBasics;
with Bytes;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

package body ClientServerNet.Test is

   Exc : Exception;

   subtype Server_Range is Integer range 0..4;
   subtype Client_Range is Integer range 0..15;

   type Connected_Array is array(Client_Range) of Boolean;

   type ServerControl_Type;
   type ServerControl_Access is access all ServerControl_Type;

   type Connection_Type;
   type Connection_Access is access all Connection_Type;

   type State_Type is new ClientServerNet.StateCallBack_Interface with
      record
         Server     : ServerControl_Access := null;
         Connection : Connection_Access := null;
      end record;

   overriding
   function NetworkReceive
     (State  : in out State_Type;
      Stream : in out Streams.ReadStream_Interface'Class)
      return ClientServerNet.StateCallBack_ClassAccess;
   ---------------------------------------------------------------------------

   type Connection_Type is new ClientServerNet.ConnectionCallBack_Interface with
      record
         ClientAddress   : Integer;
         ServerAddress   : Integer; -- We set a -1 here if its a connection on the server's side
         Server          : ServerControl_Access:=null;
         WriteStream     : Streams.WriteStream_Ref;
         FailedConnect   : Boolean:=False;
         Connected       : Boolean:=False;
         Disconnected    : Boolean:=False;
         State           : State_Type;
         ReceiveBuffer   : Bytes.Byte_ArrayAccess:=null;
         ReceiveFilled   : Natural:=0;
      end record;

   overriding
   procedure NetworkDisconnect
     (Connection : in out Connection_Type);

   overriding
   function NetworkConnect
     (Connection : in out Connection_Type;
      Stream     : Streams.WriteStream_Ref)
      return ClientServerNet.StateCallBack_ClassAccess;

   overriding
   procedure NetworkFailedConnect
     (Connection : in out Connection_Type);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Connection_Type,
      Name   => Connection_Access);
   ---------------------------------------------------------------------------

   type ServerControl_Type is new ClientServerNet.ServerCallBack_Interface with
      record
         Connected        : Connected_Array:=(others => False);
         Supposed         : Connected_Array:=(others => False);
         LatestConnection : Connection_Access:=null;
      end record;

   overriding
   function NetworkAccept
     (ServerControl : in out ServerControl_Type;
      ClientAddress : Unbounded_String)
      return ClientServerNet.ConnectionCallBack_ClassAccess;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ServerControl_Type,
      Name   => ServerControl_Access);

   ---------------------------------------------------------------------------

   function NetworkReceive
     (State  : in out State_Type;
      Stream : in out Streams.ReadStream_Interface'Class)
      return ClientServerNet.StateCallback_ClassAccess is

      Connection : Connection_Access renames State.Connection;

      Rnd        : Ada.Numerics.Float_Random.Generator;
      MaxAmount  : constant Natural:=Connection.ReceiveBuffer'Last-Connection.ReceiveFilled+1;
      ReadAmount : Natural;

      use Ada.Numerics.Float_Random;

   begin

      Reset(Rnd);
      ReadAmount:=Integer(Float'Rounding(Random(Rnd)*Float(MaxAmount)));
      if ReadAmount=0 then
         return State'Unrestricted_Access;
      end if;

      Put_Line("Task:"&Integer'Image(Connection.ReceiveFilled)&"->"&Integer'Image(Connection.ReceiveBuffer'Last+1));
      Stream.ReadBuffer
        (Buffer     => Connection.ReceiveBuffer(Connection.ReceiveFilled)'Address,
         BufferSize => Streams.StreamSize_Type(ReadAmount));
      if Random(Rnd)<0.5 then
         Put_Line("Cause overflow");
         raise Streams.StreamOverflow;
      end if;
      Connection.ReceiveFilled:=Connection.ReceiveFilled+ReadAmount;
      return State'Unrestricted_Access;

   end NetworkReceive;
   ---------------------------------------------------------------------------

   function NetworkConnect
     (Connection : in out Connection_Type;
      Stream     : Streams.WriteStream_Ref)
      return ClientServerNet.StateCallBack_ClassAccess is
   begin
      if Connection.FailedConnect then
         ReportIssue("Connect after FailedConnect");
      end if;
      if Connection.Connected then
         ReportIssue("Connect called twice");
      end if;
      if Connection.Disconnected then
         ReportIssue("Connect after Disconnect");
      end if;
      Connection.WriteStream:=Stream;
      Connection.Connected:=True;
      Connection.State.Connection:=Connection'Unrestricted_Access;
      Connection.State.Server:=Connection.Server;
      if Connection.Server/=null then
         Connection.Server.LatestConnection:=Connection'Unrestricted_Access;
      end if;

      return Connection.State'Unrestricted_Access;
   end NetworkConnect;
   ---------------------------------------------------------------------------

   procedure NetworkFailedConnect
     (Connection : in out Connection_Type) is
   begin
      if Connection.Connected then
         ReportIssue("FailedConnect after Connect");
      end if;
      if Connection.FailedConnect then
         ReportIssue("FailedConnect called twice");
      end if;
      if Connection.Disconnected then
         ReportIssue("FailedConnect after Disconnect");
      end if;
      Connection.FailedConnect:=True;
   end NetworkFailedConnect;
   ---------------------------------------------------------------------------

   procedure NetworkDisconnect
     (Connection : in out Connection_Type) is
   begin

      Connection.Disconnected:=True;

      if Connection.Server/=null then
         Connection.Server.Connected(Connection.ClientAddress):=False;
      end if;

   end NetworkDisconnect;
   ---------------------------------------------------------------------------

   function NetworkAccept
     (ServerControl : in out ServerControl_Type;
      ClientAddress : Unbounded_String)
      return ClientServerNet.ConnectionCallBack_ClassAccess is

      ClientInt  : constant Integer:=Integer'Value(To_String(ClientAddress));
      Connection : constant Connection_Access:=new Connection_Type;
   begin

      ServerControl.Connected(ClientInt):=True;

      Connection.ClientAddress := ClientInt;
      Connection.ServerAddress := -1;
      Connection.Server        := ServerControl'Unrestricted_Access;

      return ConnectionCallBack_ClassAccess(Connection);

   end NetworkAccept;
   ---------------------------------------------------------------------------

   type Server_Array is array(Integer range <>) of ClientServerNet.Server_Ref;
   type Client_Array is array(Integer range <>) of ClientServerNet.Client_Ref;

   type ServerControl_Array is array(Integer range <>) of ServerControl_Access;
   type Connection_Array is array(Integer range <>) of Connection_Access;

   function CreateServerConfigSMPipe
     (Address : Integer)
      return Config.ConfigNode_Type is
   begin
      return C:Config.ConfigNode_Type do
         ClientServerNet.SMPipe.CreateServerConfig(C,U(Integer'Image(Address)));
      end return;
   end CreateServerConfigSMPipe;
   ---------------------------------------------------------------------------

   function CreateClientConfigSMPipe
     (ClientAddress : Integer;
      ServerAddress : Integer)
      return Config.ConfigNode_Type is
   begin
      return C:Config.ConfigNode_Type do
         ClientServerNet.SMPipe.CreateClientConfig
           (Configuration => C,
            ClientAddress => U(Integer'Image(ClientAddress)),
            ServerAddress => U(Integer'Image(ServerAddress)));
      end return;
   end CreateClientConfigSMPipe;
   ---------------------------------------------------------------------------

   type CreateClientConfig_Access is
     access function
       (ClientAddress : Integer;
        ServerAddress : Integer)
        return Config.ConfigNode_Type;

   type CreateServerConfig_Access is
     access function
       (Address : Integer)
        return Config.ConfigNode_Type;

   procedure ConnectionMonteCarlo
     (CreateClientConfig : CreateClientConfig_Access;
      CreateServerConfig : CreateServerConfig_Access) is

      type Operation_Enum is
        (OperationSwitchServer,
         OperationSwitchClient);

      type Check_Enum is
        (CheckConnected,
         CheckFailedConnect,
         CheckServerDisconnect,
         CheckClientDisconnects,
         CheckNothing);

      package RandomServer_Pack is new Ada.Numerics.Discrete_Random(Server_Range);
      package RandomClient_Pack is new Ada.Numerics.Discrete_Random(Client_Range);

      RandomServer    : RandomServer_Pack.Generator;
      RandomClient    : RandomClient_Pack.Generator;
      RandomOperation : Ada.Numerics.Float_Random.Generator;

      use RandomServer_Pack;
      use RandomClient_Pack;
      use Ada.Numerics.Float_Random;

      Servers : Server_Array(Server_Range);
      ServerC : ServerControl_Array(Server_Range):=(others=>null);
      Clients : Client_Array(Client_Range);
      ClientC : Connection_Array(Client_Range):=(others=>null);

      Operation : Operation_Enum;

      Check        : Check_Enum;
      CheckAddress : Integer;
      CheckRemoteAddress : Integer;

   begin

      Reset(RandomServer);
      Reset(RandomClient);
      Reset(RandomOperation);

      for TestNr in 1..10000 loop

         if Random(RandomOperation)<0.2 then
            Operation:=OperationSwitchServer;
         else
            Operation:=OperationSwitchClient;
         end if;

         Check:=CheckNothing;

         case Operation is
            when OperationSwitchServer =>
               declare
                  Server : constant Integer:=Random(RandomServer);
               begin
                  if Servers(Server).I=null then
                     Servers(Server):=ClientServerNet.ServerImplementations.Utilize(CreateServerConfig(Server));
                     ServerC(Server):=new ServerControl_Type;
                     Servers(Server).I.CallBack:=ServerCallBack_ClassAccess(ServerC(Server));
                  else

                     Servers(Server).SetNull;

                     Check:=CheckClientDisconnects;
                     CheckAddress:=Server;

                     if ServerC(CheckAddress)=null then
                        raise Exc with "Unexpected missing of ServerC entry before setting CheckClientDisconnects";
                     end if;

                  end if;
               end;
            when OperationSwitchClient =>
               declare
                  Client : constant Integer:=Random(RandomClient);
                  Server : constant Integer:=Random(RandomServer);
               begin

                  if Clients(Client).I=null then

                     CheckAddress:=Client;

                     Clients(Client):=ClientServerNet.ClientImplementations.Utilize(CreateClientConfig(Client,Server));
                     ClientC(Client):=new Connection_Type;
                     Clients(Client).I.CallBack:=ConnectionCallBack_ClassAccess(ClientC(Client));
                     ClientC(Client).ServerAddress:=Server;
                     ClientC(Client).ClientAddress:=-1;

                     if ServerC(Server)/=null then
                        ServerC(Server).Supposed(Client):=True;
                        Check:=CheckConnected;
                     else
                        Check:=CheckFailedConnect;
                     end if;

                  else
                     -- Remove Connection from the Server side
                     declare
                        ServerA : constant Integer:=ClientC(Client).ServerAddress;
                     begin

                        Check:=CheckServerDisconnect;
                        CheckAddress:=ServerA;
                        CheckRemoteAddress:=Client;

                        if not ServerC(ServerA).Supposed(Client) then
                           raise Exc with "Supposed does not include connected client";
                        end if;
                        ServerC(ServerA).Supposed(Client):=False;
                     end;
                     Clients(Client).SetNull;
                     Free(ClientC(Client));

                  end if;
               end;

         end case;

         for Loops in 1..10 loop
            GlobalLoop.Process;
         end loop;

         case Check is
            when CheckNothing =>
               null;

            when CheckConnected =>
               if not ClientC(CheckAddress).Connected then
                  ReportIssue("Supposed connection failed (client)");
               end if;
               if not ServerC(ClientC(CheckAddress).ServerAddress).Connected(CheckAddress) then
                  ReportIssue("Client shows connected, but Server doesn't");
               end if;

            when CheckFailedConnect =>
               if not ClientC(CheckAddress).FailedConnect then
                  ReportIssue("Supposed failed connect missing (client)");
               end if;
               Clients(CheckAddress).SetNull;
               Free(ClientC(CheckAddress));

            when CheckServerDisconnect =>
               -- TODO: Add check for explicit disconnect call (client side)
               if ServerC(CheckAddress).Connected(CheckRemoteAddress) then
                  ReportIssue("Missing disconnect for server connection, Connected flag still set");
               end if;

            when CheckClientDisconnects =>
               for Client in Client_Range loop
                  if ServerC(CheckAddress)=null then
                     raise Exc with "Unexpected ServerC(Entry)=null during CheckClientDisconnects";
                  end if;
                  if ServerC(CheckAddress).Supposed(Client) then
                     if ServerC(CheckAddress).Connected(Client) then
                        ReportIssue("Server was freed, but not all disconnects were called");
                     end if;
                     if not ClientC(Client).Disconnected then
                        ReportIssue("Server was freed, but disconnect didn't arrive at client");
                     end if;
                     Clients(Client).SetNull;
                     Free(ClientC(Client));
                  end if;

               end loop;
               Free(ServerC(CheckAddress));

         end case;

         for Server in Server_Range loop

            if ServerC(Server)/=null then
               if ServerC(Server).Connected/=ServerC(Server).Supposed then
                  ReportIssue("Not all connections are as supposed");
--                  for i in Client_Range loop
--                     Put_Line(">"&Boolean'Image(ServerC(Server).Connected(i))&":"&Boolean'Image(ServerC(Server).Supposed(i)));
--                  end loop;
               end if;
            end if;

         end loop;

      end loop;

   end ConnectionMonteCarlo;
   ---------------------------------------------------------------------------

   pragma Warnings(off);
   procedure TransferMonteCarlo
     (CreateClientConfig : CreateClientConfig_Access;
      CreateServerConfig : CreateServerConfig_Access) is

      use Ada.Numerics.Float_Random;
      use type Streams.WriteStream_ClassAccess;

      LoopCount     : constant:=1000;
      MaxDataAmount : constant:=10000.0;

      Barrier      : ProtectedBasics.PollingBarrier_Type;
      RandomData   : Bytes.Byte_ArrayAccess;

      procedure SendRandomData
        (Stream : Streams.WriteStream_Ref) is
         RndGen  : Ada.Numerics.Float_Random.Generator;
      begin
         Reset(RndGen);
         declare
            Remaining  : Integer:=RandomData'Length;
            SendLength : Integer;
            Position   : Integer:=RandomData'First;
         begin
            while Remaining/=0 loop
               SendLength:= Integer(Float'Rounding(Random(RndGen)*Float(Remaining)));
               Stream.I.WriteBuffer
                 (Buffer     => RandomData(Position)'Address,
                  BufferSize => Streams.StreamSize_Type(SendLength));
               Remaining := Remaining-SendLength;
               Position  := Position+SendLength;
            end loop;
         end;
         Stream.I.Flush;
      end SendRandomData;
      ------------------------------------------------------------------------

      procedure CreateReceiveBuffer
        (Connection : Connection_Access) is
      begin
         Connection.ReceiveBuffer := new Bytes.Byte_Array(RandomData'Range);
         Connection.ReceiveFilled := Connection.ReceiveBuffer'First;
      end CreateReceiveBuffer;
      ------------------------------------------------------------------------

      procedure CompareReceiveBuffer
        (Connection : Connection_Access) is
         use type Bytes.Byte_Type;
      begin
         for i in RandomData'Range loop
            if Connection.ReceiveBuffer(i)/=RandomData(i) then
               ReportIssue("Difference at "&Integer'Image(i));
            end if;
         end loop;
      end CompareReceiveBuffer;
      ------------------------------------------------------------------------

      task ServerTask;
      task body ServerTask is
         Server  : Server_Ref;
         ServerC : aliased ServerControl_Type;
         RndGen  : Ada.Numerics.Float_Random.Generator;
         Success : Boolean;
         BarrierState : ProtectedBasics.BarrierState_Enum;
         WriteStream  : Streams.WriteStream_Ref;
      begin
         Reset(RndGen);
         Put_Line("ServerTask");
         begin
            Server:=ClientServerNet.ServerImplementations.Utilize(CreateServerConfig(0));
            Server.I.CallBack:=ServerC'Unrestricted_Access;
            Barrier.Join(BarrierState);
            while Barrier.GetMemberCount/=2 loop
               Delay Duration'Small;
            end loop;
            while ServerC.LatestConnection=null loop
               GlobalLoop.Process;
               Delay Duration'Small;
            end loop;
            WriteStream:=ServerC.LatestConnection.WriteStream;

            pragma Assert(WriteStream.I/=null);

            for Loops in 1..LoopCount loop

               -- Generate large chunk of data
               declare
                  Length : constant Integer:=Integer(Float'Rounding(Random(RndGen)*MaxDataAmount));
               begin
                  RandomData:=new Bytes.Byte_Array(0..Length-1);
                  for i in RandomData'Range loop
                     RandomData(i) := Bytes.Byte_Type(Float'Rounding(Random(RndGen)*255.0));
                  end loop;
                  Put_Line("Random Block of Length:"&Integer'Image(Length));
               end;
               loop
                  Barrier.TestBarrier(BarrierState,Success);
                  exit when Success;
               end loop;

               CreateReceiveBuffer(ServerC.LatestConnection);
               -- Send large chunk of data in random large pieces
               SendRandomData(WriteStream);

               -- Receive large chunk of data
--               while ServerC.LatestConnection.ReceiveFilled<=ServerC.LatestConnection.ReceiveBuffer'Last loop
--                  Put_Line("SR:"&Integer'Image(ServerC.LatestConnection.ReceiveFilled)&":"&Integer'Image(serverC.LatestConnection.ReceiveBuffer'Last+1));
--                  GlobalLoop.Process;
--               end loop;
--               CompareReceiveBuffer(ServerC.LatestConnection);
               Put_Line("SERVERBARRIER******"&Integer'Image(ServerC.LatestConnection.ReceiveFilled));
               -- Barrier
               loop
                  Barrier.TestBarrier(BarrierState,Success);
                  exit when Success;
               end loop;
               Bytes.Free(ServerC.LatestConnection.ReceiveBuffer);
               Bytes.Free(RandomData);
            end loop;
         exception
            when E:others =>
               Put_Line("SERVEREXCEPTION");
               Put_Line(Ada.Exceptions.Exception_Message(E));
         end;
         Put_Line("ENDOFSERVER");
         Server.SetNull;
      end ServerTask;

      Client       : Client_Ref;
      ClientC      : aliased Connection_Type;
      Success      : Boolean;
      BarrierState : ProtectedBasics.BarrierState_Enum;
      WriteStream  : Streams.WriteStream_Ref;

   begin
      Barrier.Join(BarrierState);
      while Barrier.GetMemberCount/=2 loop
         Delay Duration'Small;
      end loop;
      Put_Line("Create Client");
      Client:=ClientServerNet.ClientImplementations.Utilize(CreateClientConfig(1,0));
      Client.I.CallBack:=ClientC'Unrestricted_Access;
      Put_Line("Wait for connection to server");
      while not ClientC.Connected loop
         GlobalLoop.Process;
         delay Duration'Small;
      end loop;
      WriteStream:=ClientC.WriteStream;
      pragma Assert(WriteStream.I/=null);
      for Loops in 1..LoopCount loop
         -- Wait for random data
         loop
            Barrier.TestBarrier(BarrierState,Success);
            exit when Success;
         end loop;

         Put_Line("Client.CreateReceiveBuffer");

         CreateReceiveBuffer(ClientC'Unrestricted_Access);

--         SendRandomData(WriteStream);

         while ClientC.ReceiveFilled<=ClientC.ReceiveBuffer'Last loop
--            Put_Line("CL"&Integer'Image(ClientC.ReceiveFilled)&"::"&Integer'Image(ClientC.ReceiveBuffer'Last+1));
            GlobalLoop.Process;
         end loop;
         CompareReceiveBuffer(ClientC'Unrestricted_Access);
         Put_Line("CLIENTBARRIER*****"&Integer'Image(ClientC.ReceiveFilled));

         loop
            Barrier.TestBarrier(BarrierState,Success);
            exit when Success;
         end loop;
         Bytes.Free(ClientC.ReceiveBuffer);
      end loop;
      Put_Line("ENDOFCLIENT");
   exception
      when E:others =>
         Put_Line("EXCEPTIONM");
         Put_Line(Ada.Exceptions.Exception_Message(E));
   end TransferMonteCarlo;

   procedure ConnectionMonteCarloSMPipe is
   begin
      ConnectionMonteCarlo
        (CreateClientConfig => CreateClientConfigSMPipe'Access,
         CreateServerConfig => CreateServerConfigSMPipe'Access);
   end ConnectionMontecarloSMPipe;
   ---------------------------------------------------------------------------

   procedure TransferMonteCarloSMPipe is
   begin
      TransferMonteCarlo
        (CreateClientConfig => CreateClientConfigSMPipe'Access,
         CreateServerConfig => CreateServerConfigSMpipe'Access);
   end TransferMonteCarloSMPipe;
   ---------------------------------------------------------------------------

end ClientServerNet.Test;
