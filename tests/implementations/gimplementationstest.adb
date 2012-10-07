with Implementations;
with Config;

package body GImplementationsTest is

   type Impl_Type is null record;
   type Parameter_Type is null record;

   package Impl is new Implementations(Impl_Type,Parameter_Type);

   function Constructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess;
      Parameters : Parameter_Type)
      return Impl_Type is
      pragma Unreferenced(GenConfig,ImplConfig,Parameters);

      X : Impl_Type;

   begin
      return X;
   end Constructor;
   ---------------------------------------------------------------------------

   function Compatible
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess;
      Parameters : Parameter_Type)
      return Boolean is
      pragma Unreferenced(GenConfig,ImplConfig,Parameters);
   begin
      return True;
   end Compatible;
   ---------------------------------------------------------------------------

   procedure CreateTest is
   begin
      Impl.Register(U("1"),Compatible'Access,Constructor'Access);
      if not Impl.Has(U("1")) then
         ReportIssue("Missing Implementation 1");
      end if;
      Impl.Register(U("2"),Compatible'Access,Constructor'Access);
      if not Impl.Has(U("2")) then
         ReportIssue("Missing Implementation 2");
      end if;
      if not Impl.Has(U("1")) then
         ReportIssue("Missing Implementation 1 after creation of Impl 2");
      end if;
      begin
         Impl.Register(U("1"),Compatible'Access,Constructor'Access);
         ReportIssue("Unexpected exception");
      exception
         when Impl.ImplementationRegisteredTwice =>
            null;
         when others =>
            ReportIssue("Unexpected exception, expected ImplementationRegisteredTwice");
      end;

   end CreateTest;
   ---------------------------------------------------------------------------

end GImplementationsTest;
