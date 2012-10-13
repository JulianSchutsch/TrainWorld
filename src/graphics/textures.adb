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

   procedure CopyToRawData
     (Texture : in out BGRATexture_Type;
      Data    : System.Address) is

      Pointer : BGRAPixel_Access:=AddressToBGRAPixelAccess(Data);

   begin

      pragma Assert(Pointer/=null);
      if Texture.Pixels/=null then
         for i in Texture.Pixels'Range(1) loop
            for n in Texture.Pixels'Range(2) loop
               Pointer.all:=Texture.Pixels(i,n);
               Pointer:=Pointer+1;
            end loop;
         end loop;
      end if;

   end;

   procedure CopyToRawDataSwapRB
     (Texture : in out BGRATexture_Type;
      Data    : System.Address) is

      Pointer : BGRAPixel_Access:=AddressToBGRAPixelAccess(Data);

   begin

      pragma Assert(Pointer/=null);
      if Texture.Pixels/=null then
         for i in Texture.Pixels'Range(1) loop
            for n in Texture.Pixels'Range(2) loop
               declare
                  Source : BGRAPixel_Type renames Texture.Pixels(i,n);
               begin
                  Pointer.all.Red   := Source.Blue;
                  Pointer.all.Green := Source.Green;
                  Pointer.all.Blue  := Source.Red;
                  Pointer.all.Alpha := Source.Alpha;
                  Pointer:=Pointer+1;
               end;
            end loop;
         end loop;
      end if;

   end CopyToRawDataSwapRB;
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

   procedure CopyFromRawDataSwapRB
     (Texture : in out BGRATexture_Type;
      Data    : System.Address) is

   Pointer : BGRAPixel_Access:=AddressToBGRAPixelAccess(Data);

   begin

      pragma Assert(Pointer/=null);
      if Texture.Pixels/=null then
         for i in Texture.Pixels'Range(1) loop
            for n in Texture.Pixels'Range(2) loop
               declare
                  Target : BGRAPixel_Type renames Texture.Pixels(i,n);
               begin
                  Target.Red   := Pointer.all.Blue;
                  Target.Green := Pointer.all.Green;
                  Target.Blue  := Pointer.all.Red;
                  Target.Alpha := Pointer.all.Alpha;
                  Pointer:=Pointer+1;
               end;
            end loop;
         end loop;
      end if;

   end CopyFromRawDataSwapRB;
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

   procedure VertLine
     (Texture : in out BGRATexture_Type;
      X       : Integer;
      Y       : Integer;
      Height  : Natural;
      Color   : BGRAPixel_Type) is

      DrawHeight : Integer;
      DrawY      : Integer;

   begin
      if (Texture.Pixels=null)
        or (X<0)
        or (X>=Texture.Width) then
         return;
      end if;

      DrawHeight := Height;
      DrawY      := Y;

      if DrawY<0 then
         DrawHeight:=DrawHeight+DrawY;
         DrawY:=0;
      end if;

      if DrawY+DrawHeight>Texture.Height then
         DrawHeight:=Texture.Height-DrawY;
      end if;

      if DrawHeight<=0 then
         return;
      end if;

      for i in DrawY..DrawY+DrawHeight-1 loop
         Texture.Pixels(i,X):=Color;
      end loop;

   end VertLine;
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
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => RGBAPixel_2DArray,
      Name   => RGBAPixel_2DArrayAccess);

   procedure Finalize
     (Texture : in out RGBATexture_Type) is
   begin

      if Texture.Pixels/=null then
         Free(Texture.Pixels);
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   function RGBAPixelAccessToSizeT is new Ada.Unchecked_Conversion
     (Source => RGBAPixel_Access,
      Target => Interfaces.C.size_t);

   function SizeTToRGBAPixelAccess is new Ada.Unchecked_Conversion
     (Source => Interfaces.C.size_t,
      Target => RGBAPixel_Access);

   function AddressToRGBAPixelAccess is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => RGBAPixel_Access);

   -- TODO PORTABILITY : Replace 8 by Element'Size

   function "+"
     (Left  : RGBAPixel_Access;
      Right : Integer)
      return RGBAPixel_Access is

      use type Interfaces.C.size_t;

   begin
      return SizeTToRGBAPixelAccess
        (RGBAPixelAccessToSizeT(Left)+BGRAPixel_Type'Size/8*Interfaces.C.size_t(Right));
   end "+";
   ---------------------------------------------------------------------------

   procedure CopyToRawData
     (Texture : in out RGBATexture_Type;
      Data    : System.Address) is

      Pointer : RGBAPixel_Access:=AddressToRGBAPixelAccess(Data);

   begin

      pragma Assert(Pointer/=null);
      if Texture.Pixels/=null then
         for i in Texture.Pixels'Range(1) loop
            for n in Texture.Pixels'Range(2) loop
               Pointer.all:=Texture.Pixels(i,n);
               Pointer:=Pointer+1;
            end loop;
         end loop;
      end if;

   end;

   procedure CopyToRawDataSwapRB
     (Texture : in out RGBATexture_Type;
      Data    : System.Address) is

      Pointer : RGBAPixel_Access:=AddressToRGBAPixelAccess(Data);

   begin

      pragma Assert(Pointer/=null);
      if Texture.Pixels/=null then
         for i in Texture.Pixels'Range(1) loop
            for n in Texture.Pixels'Range(2) loop
               declare
                  Source : RGBAPixel_Type renames Texture.Pixels(i,n);
               begin
                  Pointer.all.Red   := Source.Blue;
                  Pointer.all.Green := Source.Green;
                  Pointer.all.Blue  := Source.Red;
                  Pointer.all.Alpha := Source.Alpha;
                  Pointer:=Pointer+1;
               end;
            end loop;
         end loop;
      end if;

   end CopyToRawDataSwapRB;
   ---------------------------------------------------------------------------

   procedure CopyFromRawData
     (Texture : in out RGBATexture_Type;
      Data    : System.Address) is

      Pointer : RGBAPixel_Access:=AddressToRGBAPixelAccess(Data);

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

   procedure CopyFromRawDataSwapRB
     (Texture : in out RGBATexture_Type;
      Data    : System.Address) is

   Pointer : RGBAPixel_Access:=AddressToRGBAPixelAccess(Data);

   begin

      pragma Assert(Pointer/=null);
      if Texture.Pixels/=null then
         for i in Texture.Pixels'Range(1) loop
            for n in Texture.Pixels'Range(2) loop
               declare
                  Target : RGBAPixel_Type renames Texture.Pixels(i,n);
               begin
                  Target.Red   := Pointer.all.Blue;
                  Target.Green := Pointer.all.Green;
                  Target.Blue  := Pointer.all.Red;
                  Target.Alpha := Pointer.all.Alpha;
                  Pointer:=Pointer+1;
               end;
            end loop;
         end loop;
      end if;

   end CopyFromRawDataSwapRB;
   ---------------------------------------------------------------------------

   procedure Create
     (Texture : in out RGBATexture_Type;
      Height  : Natural;
      Width   : Natural) is
   begin

      if Texture.Pixels/=null then
         Free(Texture.Pixels);
      end if;

      Texture.Height:=Height;
      Texture.Width:=Width;
      Texture.Pixels:=new RGBAPixel_2DArray(0..Height-1,0..Width-1);

   end Create;
   ---------------------------------------------------------------------------

   procedure VertLine
     (Texture : in out RGBATexture_Type;
      X       : Integer;
      Y       : Integer;
      Height  : Natural;
      Color   : RGBAPixel_Type) is

      DrawHeight : Integer;
      DrawY      : Integer;

   begin
      if (Texture.Pixels=null)
        or (X<0)
        or (X>=Texture.Width) then
         return;
      end if;

      DrawHeight := Height;
      DrawY      := Y;

      if DrawY<0 then
         DrawHeight:=DrawHeight+DrawY;
         DrawY:=0;
      end if;

      if DrawY+DrawHeight>Texture.Height then
         DrawHeight:=Texture.Height-DrawY;
      end if;

      if DrawHeight<=0 then
         return;
      end if;

      for i in DrawY..DrawY+DrawHeight-1 loop
         Texture.Pixels(i,X):=Color;
      end loop;

   end VertLine;
   ---------------------------------------------------------------------------

   procedure Clear
     (Texture : in out RGBATexture_Type;
      Color   : RGBAPixel_Type) is
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
