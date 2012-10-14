pragma Ada_2012;

with Implementations;

package body Config.Test is

   TestConfigID     : aliased constant String:="SomeConfigID#####";
   TestImplConfigID : aliased constant String:="SomeImplConfigID#####";

   type TestConfig_Type is new Config.Config_Type with
      record
         ID : String_Ref:=RefConstStr(TestConfigID'Access);
      end record;
   type TestConfig_Access is access all TestConfig_Type;

   type TestImplConfig_Type is new Config.Config_Type with
      record
         ID  : String_Ref:=RefConstStr(TestImplConfigID'Access);
      end record;
   type TestImplConfig_Access is access all TestImplConfig_Type;

   S1 : aliased constant String:="1";
   S2 : aliased constant String:="2";
   SomeElse : aliased constant String:="Something else";

   procedure PassConfigTest is

      RootNode   : ConfigNode_Type;
      TestConfig : constant TestConfig_Access:=new TestConfig_Type;
      TestImpl1  : constant TestImplConfig_Access:=new TestImplConfig_Type;
      TestImpl2  : constant TestImplConfig_Access:=new TestImplConfig_Type;

      type Impl_Type is
         record
            ID : String_Ref;
         end record;
      type Parameter_Type is null record;

      package Impl is new Implementations
        (Implementation_Type => Impl_Type,
         Parameter_Type      => Parameter_Type);

      function Constructor1
        (GenConfig  : Config.Config_ClassAccess;
         ImplConfig : config.Config_ClassAccess;
         Parameters : Parameter_Type)
         return Impl_Type is
         pragma Unreferenced(Parameters);
         X : constant Impl_Type:=(ID=>RefConstStr(S1'Access));
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
        (GenConfig  : Config.Config_ClassAccess;
         ImplConfig : Config.Config_ClassAccess;
         Parameters : Parameter_Type)
         return Impl_Type is
         pragma Unreferenced(Parameters);
         X : constant Impl_Type:=(ID=>RefConstStr(S2'Access));
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

      function Compatible
        (GenConfig  : Config.Config_ClassAccess;
         ImplConfig : Config.Config_ClassAccess;
         Parameters : Parameter_Type)
         return Boolean is
         pragma Unreferenced(GenConfig,ImplConfig,Parameters);
      begin
         return True;
      end Compatible;
      ------------------------------------------------------------------------

      CR1 : Impl_Type;
      CR2 : Impl_Type;
      CR3 : Impl_Type;
      pragma Unreferenced(CR3);

   begin

      -- This is expected to work after ConfigTest
      RootNode.SetConfig(Config_ClassAccess(TestConfig));
      RootNode.SetImplConfig(RefConstStr(S1'Access),Config_ClassAccess(TestImpl1));
      RootNode.SetImplConfig(RefConstStr(S2'Access),Config_ClassAccess(TestImpl2));

      Impl.Register(RefConstStr(S1'Access),Compatible'Access,Constructor1'Access);
      Impl.Register(RefConstStr(S2'Access),Compatible'Access,Constructor2'Access);

      RootNode.SetImplementation(RefConstStr(S1'Access));
      CR1:=Impl.Utilize(RootNode,(null record));
      if CR1.ID/=S1 then
         ReportIssue("Wrong constructor called, expected ID=1");
      end if;

      RootNode.SetImplementation(RefConstStr(S2'Access));
      CR2:=Impl.Utilize(RootNode,(null record));
      if CR2.ID/=S2 then
         ReportIssue("Wrong constructor called, expected ID=2");
      end if;

      RootNode.SetImplementation(RefConstStr(SomeElse'Access));
      begin
         CR3:=Impl.Utilize(RootNode,(null record));
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

      RootNode.SetImplConfig(RefConstStr(S1'Access),Config_ClassAccess(TestImpl1));
      if TestImplConfig_Access(RootNode.GetImplConfig(RefConstStr(S1'Access)))/=TestImpl1 then
         ReportIssue("TestImpl1 not set properly");
      end if;
      RootNode.SetImplConfig(RefConstStr(S2'Access),Config_ClassAccess(TestImpl2));
      if TestImplConfig_Access(RootNode.GetImplConfig(RefConstStr(S2'Access)))/=TestImpl2 then
         ReportIssue("TestImpl2 not set properly");
      end if;
      if TestImplConfig_Access(RootNode.GetImplConfig(RefConstStr(S1'Access)))/=TestImpl1 then
         ReportIssue("TestImpl1 corrupted after adding TestImpl2");
      end if;

      RootNode.SetImplConfig(RefConstStr(S1'Access),Config_ClassAccess(TestImpl12));
      if TestImplConfig_Access(RootNode.GetImplConfig(RefConstStr(S1'Access)))/=TestImpl12 then
         ReportIssue("TestImpl12 not set properly");
      end if;
      if TestImplConfig_Access(RootNode.GetImplconfig(RefConstStr(S2'Access)))/=TestImpl2  then
         ReportIssue("TestImpl2 lost after replacing TestImpl1 with TestImpl12");
      end if;

      RootNode.SetImplConfig(RefConstStr(S2'Access),Config_ClassAccess(TestImpl22));
      if TestImplConfig_Access(RootNode.GetImplConfig(RefConstStr(S2'Access)))/=TestImpl22 then
         ReportIssue("TestImpl22 not set proplery");
      end if;
      if TestImplConfig_Access(RootNode.GetImplConfig(RefConstStr(S1'Access)))/=TestImpl12 then
         ReportIssue("TestImpl12 lost after replacing TestImpl2 with TestImpl22");
      end if;

   end ConfigTest;
   ---------------------------------------------------------------------------

   C1 : aliased constant String:="C1";
   C2 : aliased constant String:="C2";
   C3 : aliased constant String:="C3";
   C4 : aliased constant String:="C4";
   C5 : aliased constant String:="C5";
   C6 : aliased constant String:="C6";

   procedure PathTest is
      RootNode : ConfigNode_Type;

      -- Test path for nesting
      Path1 : constant ConfigPath_Type:=(RefConstStr(C1'Access),RefConstStr(C2'Access),RefConstStr(C3'Access));
      -- Test path for second child
      Path2 : constant ConfigPath_Type:=(0=>RefConstStr(C4'Access));
      -- Test path for empty path
      Path3 : ConfigPath_Type(0..-1);
      -- Test path for second child selection
      Path4 : constant ConfigPath_Type:=(RefConstStr(C4'Access),RefConstStr(C5'Access));
      -- Test path for first child selection
      Path5 : constant ConfigPath_Type:=(RefConstStr(C1'Access),RefConstStr(C6'Access));

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
