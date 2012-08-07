pragma Ada_2005;

with Ada.Unchecked_Deallocation;

package body Config.Test is

   -- An exception must be raised if a pointer keeps pointing at the config.
   procedure PointersLeftTest is

      type Config_Access is access all Config_Type;

      Pointer : Config_Pointer;
      Config  : Config_Access:=new Config_Type;

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Config_Type,
         Name   => Config_Access);

   begin
      Config.GetNode(U("Node"),Pointer,True);
      Free(Config);
   exception
      when Standard.Config.PointersLeft =>
         null;
      when Program_Error =>
         null;
      when others =>
         ReportIssue("Wrong Exception raised, expected PointersLeft");
   end PointersLeftTest;
   ---------------------------------------------------------------------------

   procedure PointerFinalizeTest is

      Config  : Config_Type;
      Pointer : Config_Pointer;

   begin
      Config.GetNode(U("Node"),Pointer);
   end PointerFinalizeTest;
   ---------------------------------------------------------------------------

   procedure PathCreationTest is

      Config       : Config_Type;
      Pointer      : Config_Pointer;
      Pointer2     : Config_Pointer;
      RootPointer  : Config_Pointer;
      Node1Pointer : Config_Pointer;
      Node2Pointer : Config_Pointer;
      Node3Pointer : Config_Pointer;
      Node4Pointer : Config_Pointer;

   begin

      Config.GetNode(U("Node1.Node2.Node3"),Pointer);

      Config.GetNode(U(""),RootPointer);

      if RootPointer.GetValue/="" then
         ReportIssue("Invalid Value for Root node");
      end if;

      RootPointer.GetNode(U("Node1"),Node1Pointer);

      if Node1Pointer.GetValue/="" then
         ReportIssue("Invalid Value for Node1 node");
      end if;

      Node1Pointer.GetNode(U("Node1"),Node2Pointer);

      if Node2Pointer.GetValue/="" then
         ReportIssue("Invalid Value for Node2 node");
      end if;

      Node2Pointer.GetNode(U("Node2"),Node3Pointer);

      if Node3Pointer.GetValue/="" then
         ReportIssue("Invalid Value for Node3 node");
      end if;

      if Node3Pointer/=Pointer then
         ReportIssue("Pointer=Node3 failed");
      end if;

      Config.GetNode(U("Node1.Node4"),Pointer2);

      Node1Pointer.GetNode(U("Node4"),Node4Pointer);
      if Node1Pointer/=Pointer2 then
         ReportIssue("Pointer2/=Node4Pointer => Sister creation failed");
      end if;

   end PathCreationTest;
   ---------------------------------------------------------------------------

end Config.Test;
