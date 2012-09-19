with Config;
with Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package PipeNetwork is

   procedure Register;

   procedure CreateServerConfig
     (Configuration : in out Config.ConfigNode_Type;
      Address       : Unbounded_String;
      BufferSize    : Streams.StreamSize_Type:=1024);

   procedure CreateClientConfig
     (Configuration : in out Config.ConfigNode_Type;
      Address       : Unbounded_String);

end PipeNetwork;
