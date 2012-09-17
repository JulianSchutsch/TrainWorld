package body InterfaceTest is

   package Pack is
      type Test_Interface is interface;

      procedure Method1
        (T : in out Test_Interface) is abstract;

      procedure Method2
        (T : in out Test_Interface) is abstract;

      procedure Method3
        (T : in out Test_Interface) is abstract;

      type Direct_Type is new Test_Interface with
         record
            CallCount  : Integer:=0;
            LastCalled : Integer:=0;
         end record;


      overriding
      procedure Method1
        (T : in out Direct_Type);

      overriding
      procedure Method2
        (T : in out Direct_Type);

      procedure Method3
        (T : in out Direct_Type);

   end Pack;

   package body Pack is

      procedure Method1
        (T : in out Direct_Type) is
      begin
         T.LastCalled:=1;
         T.CallCount:=T.CallCount+1;
      end Method1;

      procedure Method2
        (T : in out Direct_Type) is
      begin
         T.LastCalled:=2;
         T.CallCount:=T.CallCount+1;
      end Method2;

      procedure Method3
        (T : in out Direct_Type) is
      begin
         T.LastCalled:=3;
         T.CallCount:=T.CallCount+1;
      end Method3;

   end Pack;

   procedure Test is
      use Pack;
      Direct  : aliased Direct_Type;
      DirectI : access Test_Interface'Class;
   begin

      DirectI:=Direct'Access;

      DirectI.Method1;
      if (Direct.CallCount/=1) or (Direct.LastCalled/=1) then
         ReportIssue("Failed to call Method1 of Direct_Type");
      end if;

      DirectI.Method2;
      if (Direct.CallCount/=2) or (Direct.LastCalled/=2) then
         ReportIssue("Failed to call Method2 of Direct_Type");
      end if;

      DirectI.Method3;
      if (Direct.CallCount/=3) or (Direct.LastCalled/=3) then
         ReportIssue("Failed to call Method3 of Direct_Type");
      end if;

   end Test;

end InterfaceTest;
