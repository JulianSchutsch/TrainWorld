pragma Ada_2012;

with Streams;
with RefCount;
with Implementations;

package Network is

   type StateCallBack_Interface is interface;
   type StateCallBack_ClassAccess is access all StateCallBack_Interface'Class;

   function NetworkReceive
     (T      : in out StateCallBack_Interface;
      Stream : Streams.ReadStream_ClassAccess)
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
     (T : in out ServerCallBack_Interface)
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

end Network;
