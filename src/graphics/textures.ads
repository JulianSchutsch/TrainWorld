with Bytes;
with RefCount;

package Textures is

   UploadEmptyTexture : Exception;

   type BGRAPixel_Type is
      record
         Blue  : Bytes.Byte_Type;
         Green : Bytes.Byte_Type;
         Red   : Bytes.Byte_Type;
         Alpha : Bytes.Byte_Type;
      end record;

   type BGRAPixel_2DArray is array(Natural range <>, Natural range <>) of aliased BGRAPixel_Type;
   pragma Convention(C,BGRAPixel_2DArray);
   type BGRAPixel_2DArrayAccess is access all BGRAPixel_2DArray;

   type BGRATexture_Type is new RefCount.Ref_Interface with
      record
         Height : Natural;
         Width  : Natural;
         Pixels : BGRAPixel_2DArrayAccess:=null;
      end record;
   type BGRATexture_ClassAccess is access all BGRATexture_Type'Class;

   overriding
   procedure Finalize
     (Texture : in out BGRATexture_Type);

   not overriding
   procedure Create
     (Texture : in out BGRATexture_Type;
      Height  : Natural;
      Width   : Natural);

   not overriding
   procedure Clear
     (Texture : in out BGRATexture_Type;
      Color   : BGRAPixel_Type);

   not overriding
   procedure Bind
     (Texture : in out BGRATexture_Type) is null;

   not overriding
   procedure Upload
     (Texture : in out BGRATexture_Type) is null;
   ---------------------------------------------------------------------------

   package BGRATextureRef is new RefCount.Ref(BGRATexture_Type,BGRATexture_ClassAccess);

   subtype BGRATexture_Ref is BGRATextureRef.Ref_Type;

end Textures;
