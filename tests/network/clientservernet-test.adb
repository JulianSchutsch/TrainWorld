pragma Ada_2012;

with ClientServerNet;
with Ada.Text_IO; use Ada.Text_IO;
with ClientServerNet.SMPipe;
with Config;
with Streams;
with GlobalLoop;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package body ClientServerNet.Test is

   type Connection_Type is new ClientServerNet.ConnectionCallBack_Interface with
      record
         WriteStream : Streams.WriteStream_Ref;
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
   ---------------------------------------------------------------------------

   function NetworkConnect
     (Connection : in out Connection_Type;
      Stream     : Streams.WriteStream_Ref)
      return ClientServerNet.StateCallBack_ClassAccess is
   begin
      Connection.WriteStream:=Stream;
      Put_Line("Network.Connect");
      return null;
   end NetworkConnect;
   ---------------------------------------------------------------------------

   type ServerControl_Type is new ClientServerNet.ServerCallBack_Interface with
      record
         null;
      end record;

   overriding
   function NetworkAccept
     (ServerControl : in out ServerControl_Type)
      return ClientServerNet.ConnectionCallBack_ClassAccess;
   ---------------------------------------------------------------------------

   procedure NetworkFailedConnect
     (Connection : in out Connection_Type) is
   begin
      null;
   end NetworkFailedConnect;
   ---------------------------------------------------------------------------

   procedure NetworkDisconnect
     (Connection : in out Connection_Type) is
   begin
      null;
   end NetworkDisconnect;
   ---------------------------------------------------------------------------

   function NetworkAccept
     (ServerControl : in out ServerControl_Type)
      return ClientServerNet.ConnectionCallBack_ClassAccess is
      pragma Unreferenced(ServerControl);
   begin
      Put_Line("Accept called");
      return new Connection_Type;
   end NetworkAccept;
   ---------------------------------------------------------------------------

   type Server_Array is array(Integer range <>) of ClientServerNet.Server_Ref;
   type Client_Array is array(Integer range <>) of ClientServerNet.Client_Ref;

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
     (Address : Integer)
      return Config.ConfigNode_Type is
   begin
      return C:Config.ConfigNode_Type do
         ClientServerNet.SMPipe.CreateClientConfig(C,U(Integer'Image(Address)));
      end return;
   end CreateClientConfigSMPipe;
   ---------------------------------------------------------------------------

   type CreateClientConfig_Access is
     access function
       (Address : Integer)
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

      subtype Server_Range is Integer range 0..4;
      subtype Client_Range is Integer range 0..15;

      package RandomServer_Pack is new Ada.Numerics.Discrete_Random(Server_Range);
      package RandomClient_Pack is new Ada.Numerics.Discrete_Random(Client_Range);

      RandomServer    : RandomServer_Pack.Generator;
      RandomClient    : RandomClient_Pack.Generator;
      RandomOperation : Ada.Numerics.Float_Random.Generator;

      use RandomServer_Pack;
      use RandomClient_Pack;
      use Ada.Numerics.Float_Random;

      Servers : Server_Array(Server_Range);
      Clients : Client_Array(Client_Range);

      Operation : Operation_Enum;

   begin

      Reset(RandomServer);
      Reset(RandomClient);
      Reset(RandomOperation);

      for TestNr in 1..100000 loop

         if Random(RandomOperation)<0.2 then
            Operation:=OperationSwitchServer;
         else
            Operation:=OperationSwitchClient;
         end if;

         case Operation is
            when OperationSwitchServer =>
               declare
                  Server : constant Integer:=Random(RandomServer);
               begin
                  if Servers(Server).I=null then
                     Servers(Server):=ClientServerNet.ServerImplementations.Utilize(CreateServerConfig(Server));
                     Servers(Server).I.CallBack:=new ServerControl_Type;
                  else
                     Servers(Server).SetNull;
                  end if;
               end;
            when OperationSwitchClient =>
               declare
                  Client : constant Integer:=Random(RandomClient);
                  Server : constant Integer:=Random(RandomServer);
               begin
                  if Clients(Client).I=null then
                     Clients(Client):=ClientServerNet.ClientImplementations.Utilize(CreateClientConfig(Server));
                     Clients(CLient).I.CalLBack:=new Connection_Type;
                  else
                     Clients(Client).SetNull;
                  end if;
               end;

         end case;

         for Loops in 1..10 loop
            GlobalLoop.Process;
         end loop;

      end loop;

   end ConnectionMonteCarlo;
   ---------------------------------------------------------------------------

   procedure ConnectionMonteCarloSMPipe is
   begin
      ConnectionMonteCarlo
        (CreateClientConfig => CreateClientConfigSMPipe'Access,
         CreateServerConfig => CreateServerConfigSMPipe'Access);
   end ConnectionMontecarloSMPipe;
   ---------------------------------------------------------------------------

end ClientServerNet.Test;
