pragma Warnings(off);
with Streams;
with ResourceFileSystem;

procedure RFSTest is
   RFS : ResourceFileSystem.RFS_Ref;
begin
   RFS:=ResourceFileSystem.RFSFolder("/");
end RFSTest;
