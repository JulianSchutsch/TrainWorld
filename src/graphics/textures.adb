with Ada.Unchecked_Deallocation;

package body Textures is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => BGRAPixel_2DArray,
      Name   => BGRAPixel_2DArrayAccess);

   procedure Finalize
     (Texture : in out BGRATexture_Type) is
   begin

      if Texture.Pixels/=null then
         Free(Texture.Pixels);
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Create
     (Texture : in out BGRATexture_Type;
      Height  : Natural;
      Width   : Natural) is
   begin

      if Texture.Pixels/=null then
         Free(Texture.Pixels);
      end if;

      Texture.Height:=Height;
      Texture.Width:=Width;
      Texture.Pixels:=new BGRAPixel_2DArray(0..Height-1,0..Width-1);

   end Create;
   ---------------------------------------------------------------------------

   procedure Clear
     (Texture : in out BGRATexture_Type;
      Color   : BGRAPixel_Type) is
   begin

      if Texture.Pixels=null then
         return;
      end if;

      for i in Texture.Pixels'Range(1) loop
         for n in Texture.Pixels'Range(2) loop
            Texture.Pixels(i,n):=Color;
         end loop;
      end loop;

   end Clear;
   ---------------------------------------------------------------------------

end Textures;
