package body ResourceFileSystem is

   function RFSFolder
     (BasePath : String)
      return RFS_Ref is
      pragma Unreferenced(BasePath);
   begin
      return R:RFS_Ref do
         null;
      end return;
   end RFSFolder;
   ---------------------------------------------------------------------------

end ResourceFileSystem;
