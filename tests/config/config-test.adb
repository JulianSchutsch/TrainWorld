pragma Ada_2012;

with Implementations;

package body Config.Test is

   TestConfigID     : constant Unbounded_String:=U("SomeConfigID#####");
   TestImplConfigID : constant Unbounded_String:=U("SomeImplConfigID#####");

   type TestConfig_Type is new Config.Config_Type with
      record
         ID : Unbounded_String:=TestConfigID;
      end record;
   type TestConfig_Access is access all TestConfig_Type;

   type TestImplConfig_Type is new Config.Config_Type with
      record
         ID  : Unbounded_String:=TestImplConfigID;
      end record;
   type TestImplConfig_Access is access all TestImplConfig_Type;

   procedure PassConfigTest is

      RootNode   : ConfigNode_Type;
      TestConfig : constant TestConfig_Access:=new TestConfig_Type;
      TestImpl1  : constant TestImplConfig_Access:=new TestImplConfig_Type;
      TestImpl2  : constant TestImplConfig_Access:=new TestImplConfig_Type;

      type Impl_Type is
         record
            ID : Unbounded_String;
         end record;

      package Impl is new Implementations
        (Implementation_Type => Impl_Type);

      function Constructor1
        (GenConfig  : Config.Config_ClassAccess;
         ImplConfig : config.Config_ClassAccess)
         return Impl_Type is
         X : constant Impl_Type:=(ID=>U("1"));
      begin
         if TestConfig_Access(GenConfig)/=TestConfig then
            ReportIssue("Implementation 1 created with wrong generic config");
         end if;
         if TestImplConfig_Access(ImplConfig)/=TestImpl1 then
            ReportIssue("Implementation 1 created with wrong specific config");
         end if;
         return X;
      end Constructor1;
      ------------------------------------------------------------------------

      function Constructor2
        (GenConfig : Config.Config_ClassAccess;
         ImplConfig : Config.Config_ClassAccess)
         return Impl_Type is
         X : constant Impl_Type:=(ID=>U("2"));
      begin
         if TestConfig_Access(GenConfig)/=TestConfig then
            ReportIssue("Implementation 2 created with wrong generic config");
         end if;
         if TestImplConfig_Access(ImplConfig)/=TestImpl2 then
            ReportIssue("Implementation 2 created with wrong specific config");
         end if;
         return X;
      end Constructor2;
      ------------------------------------------------------------------------

      CR1 : Impl_Type;
      CR2 : Impl_Type;
      CR3 : Impl_Type;
      pragma Unreferenced(CR3);

   begin

      -- This is expected to work after ConfigTest
      RootNode.SetConfig(Config_ClassAccess(TestConfig));
      RootNode.SetImplConfig(U("1"),Config_ClassAccess(TestImpl1));
      RootNode.SetImplConfig(U("2"),Config_ClassAccess(TestImpl2));

      Impl.Register(U("1"),Constructor1'Access);
      Impl.Register(U("2"),Constructor2'Access);

      RootNode.SetImplementation(U("1"));
      CR1:=Impl.Utilize(RootNode);
      if CR1.ID/=U("1") then
         ReportIssue("Wrong constructor called, expected ID=1");
      end if;

      RootNode.SetImplementation(U("2"));
      CR2:=Impl.Utilize(RootNode);
      if CR2.ID/=U("2") then
         ReportIssue("Wrong constructor called, expected ID=2");
      end if;

      RootNode.SetImplementation(U("Something else"));
      begin
         CR3:=Impl.Utilize(RootNode);
         ReportIssue("Expected exception after asking for a non existent implementation");
      exception
         when Impl.ImplementationNotFound =>
            null; -- Expected
         when others =>
            ReportIssue("Unexpected exception, expected ImplementationNotFound after asking for a non existent implementation");
      end;

   end PassConfigTest;
   ---------------------------------------------------------------------------

   procedure ConfigTest is
      RootNode    : ConfigNode_Type;
      TestConfig  : constant TestConfig_Access     := new TestConfig_Type;
      TestConfig2 : constant TestConfig_Access     := new TestConfig_Type;
      TestImpl1   : constant TestImplConfig_Access := new TestImplConfig_Type;
      TestImpl2   : constant TestImplConfig_Access := new TestImplConfig_Type;
      TestImpl12  : constant TestImplConfig_Access := new TestImplConfig_Type;
      TestImpl22  : constant TestImplConfig_Access := new TestImplConfig_Type;
   begin

      -- Straightforward setting/replacing tests
      -- At the end only
      --  TestConfig2, TestImpl12 and TestImpl22 remain in the config.
      RootNode.SetConfig(Config_ClassAccess(TestConfig));
      if TestConfig_Access(RootNode.GetConfig)/=TestConfig then
         ReportIssue("TestConfig not set properly");
      end if;
      RootNode.SetConfig(Config_ClassAccess(TestConfig2));
      if TestConfig_Access(RootNode.GetConfig)/=TestConfig2 then
         ReportIssue("TestConfig2 not set properly");
      end if;

      RootNode.SetImplConfig(U("1"),Config_ClassAccess(TestImpl1));
      if TestImplConfig_Access(RootNode.GetImplConfig(U("1")))/=TestImpl1 then
         ReportIssue("TestImpl1 not set properly");
      end if;
      RootNode.SetImplConfig(U("2"),Config_ClassAccess(TestImpl2));
      if TestImplConfig_Access(RootNode.GetImplConfig(U("2")))/=TestImpl2 then
         ReportIssue("TestImpl2 not set properly");
      end if;
      if TestImplConfig_Access(RootNode.GetImplConfig(U("1")))/=TestImpl1 then
         ReportIssue("TestImpl1 corrupted after adding TestImpl2");
      end if;

      RootNode.SetImplConfig(U("1"),Config_ClassAccess(TestImpl12));
      if TestImplConfig_Access(RootNode.GetImplConfig(U("1")))/=TestImpl12 then
         ReportIssue("TestImpl12 not set properly");
      end if;
      if TestImplConfig_Access(RootNode.GetImplconfig(U("2")))/=TestImpl2  then
         ReportIssue("TestImpl2 lost after replacing TestImpl1 with TestImpl12");
      end if;

      RootNode.SetImplConfig(U("2"),Config_ClassAccess(TestImpl22));
      if TestImplConfig_Access(RootNode.GetImplConfig(U("2")))/=TestImpl22 then
         ReportIssue("TestImpl22 not set proplery");
      end if;
      if TestImplConfig_Access(RootNode.GetImplConfig(U("1")))/=TestImpl12 then
         ReportIssue("TestImpl12 lost after replacing TestImpl2 with TestImpl22");
      end if;

   end ConfigTest;
   ---------------------------------------------------------------------------

   procedure PathTest is
      RootNode : ConfigNode_Type;

      -- Test path for nesting
      Path1 : constant ConfigPath_Type:=(U("C1"),U("C2"),U("C3"));
      -- Test path for second child
      Path2 : constant ConfigPath_Type:=(0=>U("C4"));
      -- Test path for empty path
      Path3 : ConfigPath_Type(0..-1);
      -- Test path for second child selection
      Path4 : constant ConfigPath_Type:=(U("C4"),U("C5"));
      -- Test path for first child selection
      Path5 : constant ConfigPath_Type:=(U("C1"),U("C6"));

      function CreatePath
        (Path:ConfigPath_Type)
         return ConfigNode_Access is
      begin
         return RootNode.CreatePath(Path);
      end CreatePath;
      ------------------------------------------------------------------------

      procedure WalkPath
        (Path : ConfigPath_Type;
         Node : ConfigNode_Access) is
         C : ConfigNode_Access;
      begin

         C:=RootNode.GetPath(Path(Path'First..Path'First));
         if C.GetName/=Path(Path'First) then
            ReportIssue("Name in Path incorret");
         end if;
         for i in Path'First+1..Path'Last loop
            C:=C.GetPath(Path(i..i));
            if C.GetName/=Path(i) then
               ReportIssue("Name in Path "&Integer'Image(i)&" incorrect");
            end if;
         end loop;

         if C/=RootNode.GetPath(Path)
           or C/=Node then
            ReportIssue("Walk Path failed");
         end if;

      end WalkPath;
      ------------------------------------------------------------------------

      Path1Node : ConfigNode_Access;
      Path2Node : ConfigNode_Access;
      Path3Node : ConfigNode_Access;
      Path4Node : ConfigNode_Access;
      Path5Node : ConfigNode_Access;

   begin

      Path1Node:=CreatePath(Path1);
      if RootNode.GetDeepChildNodeCount/=3 then
         ReportIssue("Invalid Child Node Count after CreatePath Path1");
      end if;
      WalkPath(Path1,Path1Node);
      Path2Node:=CreatePath(Path2);
      if RootNode.GetDeepChildNodeCount/=4 then
         ReportIssue("Invalid Child Node Count after CreatePath Path2");
      end if;
      WalkPath(Path2,Path2Node);
      Path3Node:=CreatePath(Path3);
      if Path3Node/=RootNode'Unrestricted_Access then
         ReportIssue("CreatePath returned /=RootNode for an empty path");
      end if;
      if RootNode.GetDeepChildNodeCount/=4 then
         ReportIssue("Invalid Child Node Count after createPath Path3");
      end if;
      Path4Node:=CreatePath(Path4);
      WalkPath(Path4,Path4Node);
      if RootNode.GetDeepChildNodeCount/=5 then
         ReportIssue("Invalid Child Node Count after CreatePath Path4");
         ReportIssue("We get :" &Integer'Image(RootNode.GetDeepChildNodeCount));
         RootNode.DebugTree;
      end if;
      Path5Node:=CreatePath(Path5);
      WalkPath(Path5,Path5Node);
      if RootNode.GetDeepChildNodeCount/=6 then
         ReportIssue("Invalid Child Node Count after CreatePath Path5");
      end if;

      if Path1Node/=CreatePath(Path1) then
         ReportIssue("CreatePath result for path1 should be identical");
      end if;
      if Path1Node/=RootNode.GetPath(Path1) then
         ReportIssue("GetPath result for path1 should be identical to CreatePath");
      end if;
      if Path2Node/=CreatePath(Path2) then
         ReportIssue("CreatePath result for path2 should be identical");
      end if;
      if Path2Node/=RootNode.GetPath(Path2) then
         ReportIssue("GetPath result for path2 should be identical to CreatePath");
      end if;
      if Path4Node/=CreatePath(Path4) then
         ReportIssue("CreatePath result for path4 should be identical");
      end if;
      if Path4Node/=RootNode.GetPath(Path4) then
         ReportIssue("GetPath result for path4 should be identical to CreatePath");
      end if;
      if Path5Node/=CreatePath(Path5) then
         ReportIssue("CreatePath result for path5 should be identical");
      end if;
      if Path5Node/=RootNode.Getpath(Path5) then
         ReportIssue("GetPath result for path5 should be identical to CreatePath");
      end if;

      if RootNode.GetDeepChildNodeCount/=6 then
         ReportIssue("Invalid Child Node count after path recreation tests");
      end if;

   end PathTest;
   ---------------------------------------------------------------------------

end Config.Test;
