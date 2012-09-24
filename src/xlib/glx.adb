with DynamicLibraries;
with Ada.Text_IO; use Ada.Text_IO;

package body glX is

   LibraryHandle : DynamicLibraries.Handle_Type;

   function Conv is new Ada.Unchecked_Conversion(System.Address,GLXGetProcAddressARB_Access);

   function GetProcAddressARB
     (Name : String)
      return System.Address is
      use type Interfaces.C.char;
      CName : Interfaces.C.char_array:=Interfaces.C.To_C(Name);
   begin
      pragma Assert(CName(CName'Last)=Interfaces.C.char'Val(0));
      return glXGetProcAddressARB(CName(CName'First)'Access);
   end GetProcAddressARB;
   ---------------------------------------------------------------------------

   function GetProcAddress
     (Name : String)
      return System.Address is
      use type Interfaces.C.char;
      CName : interfaces.C.char_array:=Interfaces.C.To_C(Name);
   begin
      pragma Assert(CName(CName'Last)=Interfaces.C.char'Val(0));
      return glxGetProcAddress(CName(CName'First)'Access);
   end GetProcAddress;
   ---------------------------------------------------------------------------

   function QueryExtensionsString
     (Display : Display_Access;
      Screen  : Interfaces.C.int)
      return String is
   begin
      return Interfaces.C.Strings.Value(glXQueryExtensionsString
        (dpy    => Display,
         screen => Screen));
   end QueryExtensionsString;
   ---------------------------------------------------------------------------

   procedure LoadGLX
     (Display : Display_Access) is

      use type Interfaces.C.int;

      Major : aliased GLint_Type;
      Minor : aliased GLint_Type;

   begin
      --TODO: Add queryextension
      -- TODO: Check if Lib is allready open
      LibraryHandle.Open("libX11.so");
      if glXQueryVersion
        (dpy   => Display,
         major => Major'Unrestricted_Access,
         minor => Minor'Unrestricted_Access)=0 then
         raise FailedGLXLoading with "call to glXQueryVersion failed";
      end if;

      if (major>=2) or ((major=1) and (minor>=4)) then

         Put_Line("GetProcAddressARB");
         glXGetProcAddressARB:=Conv(GetProcAddress("glXGetProcAddressARB"&Character'Val(0)));
         if glXGetProcAddressARB=null then
            raise FailedGLXLoading with "glXGetProcAddressARB=null with glX>=1.4";
         end if;

      end if;
   end LoadGLX;
   ---------------------------------------------------------------------------

end glX;
