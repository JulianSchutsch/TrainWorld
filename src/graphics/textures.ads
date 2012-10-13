-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-- Revision History
--   28.Sep 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--    A common denominator for all implementations relying on textures
--    including implementations for loading, manipulating and displaying.
--    Providing a low level interface to handle raw data.

-- Usage
--    The texture types store a picture of Height*Width where the data
--    is stored in Pixels with first row and then column.
--    Create allocates memory for the picture and the texture types
--    are controlled, therefore deallocate themselves.
--
--    All extensions must ensure Create and Finalize are called.

with Bytes;
with RefCount;
with System;

package Textures is

   UploadEmptyTexture : Exception;

   type RGBAPixel_Type is
      record
         Red   : Bytes.Byte_Type;
         Green : Bytes.Byte_Type;
         Blue  : Bytes.Byte_Type;
         Alpha : Bytes.Byte_Type;
      end record;
   pragma Convention(C,RGBAPixel_Type);
   type RGBAPixel_Access is access all RGBAPixel_Type;
   pragma No_Strict_Aliasing(RGBAPixel_Access);

   type RGBAPixel_2Darray is array(Natural range <>, Natural range <>) of aliased RGBAPixel_Type;
   pragma Convention(C,RGBAPixel_2DArray);
   type RGBAPixel_2DArrayAccess is access all RGBAPixel_2DArray;

   type RGBATexture_Type is new RefCount.Ref_Interface with
      record
         Height : Natural;
         Width  : Natural;
         Pixels : RGBAPixel_2DArrayAccess:=null;
      end record;
   type RGBATexture_ClassAccess is access all RGBATexture_Type'Class;

   overriding
   procedure Finalize
     (Texture : in out RGBATexture_Type);

   not overriding
   procedure Create
     (Texture : in out RGBATexture_Type;
      Height  : Natural;
      Width   : Natural);

   not overriding
   procedure Clear
     (Texture : in out RGBATexture_Type;
      Color   : RGBAPixel_Type);

   not overriding
   procedure VertLine
     (Texture : in out RGBATexture_Type;
      X       : Integer;
      Y       : Integer;
      Height  : Natural;
      Color   : RGBAPixel_Type);

   not overriding
   procedure Bind
     (Texture : in out RGBATexture_Type) is null;

   not overriding
   procedure Upload
     (Texture : in out RGBATexture_Type) is null;

   not overriding
   procedure CopyFromRawData
     (Texture : in out RGBATexture_Type;
      Data    : System.Address);

   not overriding
   procedure CopyFromRawDataSwapRB
     (Texture : in out RGBATexture_Type;
      Data    : System.Address);

   not overriding
   procedure CopyToRawData
     (Texture : in out RGBATexture_Type;
      Data    : System.Address);

   not overriding
   procedure CopyToRawDataSwapRB
     (Texture : in out RGBATexture_Type;
      Data    : System.Address);

   package RGBATextureRef is new RefCount.Ref(RGBATexture_Type,RGBATexture_ClassAccess);

   subtype RGBATexture_Ref is RGBATextureRef.Ref_Type;
   ---------------------------------------------------------------------------

   type BGRAPixel_Type is
      record
         Blue  : Bytes.Byte_Type;
         Green : Bytes.Byte_Type;
         Red   : Bytes.Byte_Type;
         Alpha : Bytes.Byte_Type;
      end record;
   pragma Convention(C,BGRAPixel_Type);
   type BGRAPixel_Access is access all BGRAPixel_Type;
   pragma No_Strict_Aliasing(BGRAPixel_Access);

   type BGRAPixel_2DArray is array(Natural range <>, Natural range <>) of aliased BGRAPixel_Type;
   pragma Convention(C,BGRAPixel_2DArray);
   type BGRAPixel_2DArrayAccess is access all BGRAPixel_2DArray;

   type BGRATexture_Type is new RefCount.Ref_Interface with
      record
         Height : Natural; -- Do only read
         Width  : Natural; -- Do only read
         Pixels : BGRAPixel_2DArrayAccess:=null; -- Do not allocate manually
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
   procedure VertLine
     (Texture : in out BGRATexture_Type;
      X       : Integer;
      Y       : Integer;
      Height  : Natural;
      Color   : BGRAPixel_Type);

   not overriding
   procedure Bind
     (Texture : in out BGRATexture_Type) is null;

   not overriding
   procedure Upload
     (Texture : in out BGRATexture_Type) is null;

   not overriding
   procedure CopyFromRawData
     (Texture : in out BGRATexture_Type;
      Data    : System.Address);

   not overriding
   procedure CopyFromRawDataSwapRB
     (Texture : in out BGRATexture_Type;
      Data    : System.Address);

   not overriding
   procedure CopyToRawData
     (Texture : in out BGRATexture_Type;
      Data    : System.Address);

   not overriding
   procedure CopyToRawDataSwapRB
     (Texture : in out BGRATexture_Type;
      Data    : System.Address);
   ---------------------------------------------------------------------------

   package BGRATextureRef is new RefCount.Ref(BGRATexture_Type,BGRATexture_ClassAccess);

   subtype BGRATexture_Ref is BGRATextureRef.Ref_Type;

end Textures;
