with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Interfaces.C;

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

   function BGRAPixelAccessToSizeT is new Ada.Unchecked_Conversion
     (Source => BGRAPixel_Access,
      Target => Interfaces.C.size_t);

   function SizeTToBGRAPixelAccess is new Ada.Unchecked_Conversion
     (Source => Interfaces.C.size_t,
      Target => BGRAPixel_Access);

   function AddressToBGRAPixelAccess is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => BGRAPixel_Access);

   -- TODO PORTABILITY : Replace 8 by Element'Size

   function "+"
     (Left  : BGRAPixel_Access;
      Right : Integer)
      return BGRAPixel_Access is

      use type Interfaces.C.size_t;

   begin
      return SizeTToBGRAPixelAccess
        (BGRAPixelAccessToSizeT(Left)+BGRAPixel_Type'Size/8*Interfaces.C.size_t(Right));
   end "+";
   ---------------------------------------------------------------------------

   procedure CopyFromRawData
     (Texture : in out BGRATexture_Type;
      Data    : System.Address) is

      Pointer : BGRAPixel_Access:=AddressToBGRAPixelAccess(Data);
   begin
      pragma Assert(Pointer/=null);
      if Texture.Pixels/=null then
         for i in Texture.Pixels'Range(1) loop
            for n in Texture.Pixels'Range(2) loop
               Texture.Pixels(i,n):=Pointer.all;
               Pointer:=Pointer+1;
            end loop;
         end loop;
      end if;
   end CopyFromRawData;
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
