package body OpenGL.Textures is

   procedure Bind
     (Texture : in out BGRATexture_Type) is
   begin
      if ActiveTexture/=Texture.TextureID then
         glBindTexture
           (target  => GL_TEXTURE_2D,
            texture => Texture.TextureID);
      end if;
   end Bind;
   ---------------------------------------------------------------------------

   procedure Upload
     (Texture : in out BGRATexture_Type) is

      use type Standard.Textures.BGRAPixel_2DArrayAccess;

   begin
      if ActiveTexture/=Texture.TextureID then
         glBindTexture
           (target  => GL_TEXTURE_2D,
            texture => Texture.TextureID);
      end if;

      if Texture.Pixels=null then
         raise Standard.Textures.UploadEmptyTexture;
      end if;

      glTexImage2D
        (target => GL_TEXTURE_2D,
         level => 0,
         internalFormat => GL_RGBA,
         width => GLsizei_Type(Texture.Width),
         height => GLsizei_Type(Texture.Height),
         border => 0,
         format => GL_BGRA,
         ttype => GL_UNSIGNED_BYTE,
         data => Texture.Pixels(0,0)'Address);

   end Upload;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Texture : in out BGRATexture_Type) is
   begin

      BGRATexture_Inherited(Texture).Finalize;
      if Texture.TextureID/=0 then
         glDeleteTextures
           (n        => 1,
            textures => Texture.TextureID'Access);
         Texture.TextureID:=0;
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Create
     (Texture : in out BGRATexture_Type;
      Height  : Natural;
      Width   : Natural) is
   begin

      BGRATexture_Inherited(Texture).Create
        (Height => Height,
         Width  => Width);

      glGenTextures
        (n => 1,
         textures => Texture.TextureID'Access);
      glBindTexture
        (target  => GL_TEXTURE_2D,
         texture => Texture.TextureID);
      glTexParameteri
        (target => GL_TEXTURE_2D,
         pname  => GL_TEXTURE_MIN_FILTER,
         param  => GL_LINEAR);
      glTexParameteri
        (target => GL_TEXTURE_2D,
         pname  => GL_TEXTURE_MAG_FILTER,
         param  => GL_LINEAR);
      glTexParameteri
        (target => GL_TEXTURE_2D,
         pname  => GL_TEXTURE_WRAP_S,
         param  => GL_CLAMP);
      glTexParameteri
        (target => GL_TEXTURE_2D,
         pname  => GL_TEXTURE_WRAP_T,
         param  => GL_CLAMP);

   end Create;
   ---------------------------------------------------------------------------

end OpenGL.Textures;
