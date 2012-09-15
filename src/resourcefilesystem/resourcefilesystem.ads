pragma Ada_2012;

with RefCount;

package ResourceFileSystem is

   type RFS_Interface is abstract new RefCount.Ref_Interface with null record;
   type RFS_ClassAccess is access all RFS_Interface'Class;

   package RFSRef is new RefCount.Ref(RFS_Interface,RFS_ClassAccess);

   subtype RFS_Ref is RFSRef.Ref_Type;

   type RFSFile_Interface is new RefCount.Ref_Interface with null record;
   type RFSFile_ClassAccess is access all RFSFile_Interface'Class;

   package RFSFileRef is new RefCount.Ref(RFSFile_Interface,RFSFile_ClassAccess);

   subtype RFSFile_Ref is RFSFileRef.Ref_Type;

   function RFSFolder
     (BasePath : String)
      return RFS_Ref;

end ResourceFileSystem;
