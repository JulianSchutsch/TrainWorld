pragma Ada_2012;

with Network;
with Ada.Text_IO; use Ada.Text_IO;
with PipeNetwork;
with Config;
with Streams;
with Basics; use Basics;
with GlobalLoop;

package body NetDemo is

   type Connection_Type is new Network.ConnectionCallBack_Interface with
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
      return Network.StateCallBack_ClassAccess;

   overriding
   procedure NetworkFailedConnect
     (Connection : in out Connection_Type);
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

   function NetworkConnect
     (Connection : in out Connection_Type;
      Stream     : Streams.WriteStream_Ref)
      return Network.StateCallBack_ClassAccess is
   begin
      Connection.WriteStream:=Stream;
      return null;
   end NetworkConnect;
   ---------------------------------------------------------------------------

   type ServerControl_Type is new Network.ServerCallBack_Interface with
      record
         null;
      end record;

   function NetworkAccept
     (ServerControl : in out ServerControl_Type)
      return Network.ConnectionCallBack_ClassAccess is
      pragma Unreferenced(ServerControl);
   begin
      Put_Line("Accept called");
      return new Connection_Type;
   end NetworkAccept;
   ---------------------------------------------------------------------------

   procedure Run is
      Server       : Network.Server_Ref;
      Client       : Network.Client_Ref;
      ServerConfig : Config.ConfigNode_Type;
      ClientConfig : Config.ConfigNode_Type;
   begin

      PipeNetwork.Register;

      PipeNetwork.CreateServerConfig(ServerConfig,U("1"));
      PipeNetwork.CreateClientConfig(ClientConfig,U("1"));

      Put_Line("Create Server");
      Server:=Network.ServerImplementations.Utilize(ServerConfig);
      Put_Line("Create Client");
      Client:=Network.ClientImplementations.Utilize(ClientConfig);
      Put_Line("Set CallBacks");
      Server.I.CallBack:=new ServerControl_Type;
      Client.I.CallBack:=new Connection_Type;
      Put_Line("Loop");
      for i in 1..10000 loop
         GlobalLoop.Process;
      end loop;
      Put_Line("Done..:");

   end Run;

end NetDemo;
