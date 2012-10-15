with System.Address_Image;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Basics is

   function Add
     (Left  : Data_Access;
      Right : PtrInt_Type)
      return Data_Access is

      function AccessToPtrInt is new Ada.Unchecked_Conversion
        (Source => Data_Access,
         Target => PtrInt_Type);

      function PtrIntToAccess is new Ada.Unchecked_Conversion
        (Source => PtrInt_Type,
         Target => Data_Access);

   begin
      -- TODO: Replace 8 by element size
      return PtrIntToAccess(AccessToPtrInt(Left)+Right*Data_Type'Size/8);
   end Add;
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => String_Cont,
      Name   => String_ContAccess);

   procedure PutAddr
     (Address : System.Address) is
   begin
      Put(System.Address_Image(Address));
   end PutAddr;
   ---------------------------------------------------------------------------

   function "="
     (Left  : String_Ref;
      Right : String_Ref)
      return Boolean is
   begin
      return Left.Get=Right.Get;
   end "=";
   ---------------------------------------------------------------------------

   function "="
     (Left  : String_Ref;
      Right : String)
      return Boolean is
   begin
      return Left.Get=Right;
   end "=";
   ---------------------------------------------------------------------------

   procedure Finalize
     (StrRef : in out String_Ref) is
   begin

      if StrRef.NonConst/=null then
         StrRef.NonConst.Count:=StrRef.NonConst.Count-1;
         if StrRef.NonConst.Count=0 then
            Free(StrRef.NonConst);
         end if;
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Adjust
     (StrRef : in out String_Ref) is
   begin

      if StrRef.NonConst/=null then
         StrRef.NonConst.Count:=StrRef.NonConst.Count+1;
      end if;

   end Adjust;
   ---------------------------------------------------------------------------

   function Get
     (StrRef : String_Ref)
      return String is
   begin

      if StrRef.Const/=null then
         return StrRef.Const.all;
      end if;

      if StrRef.NonConst/=null then
         return StrRef.NonConst.Content;
      end if;

      return "";

   end Get;
   ---------------------------------------------------------------------------

   function RefStr
     (Str : String)
      return String_Ref is
   begin
      return S:String_Ref do
         S.NonConst:=new String_Cont(Str'Length);
         S.NonConst.Content:=Str;
      end return;
   end RefStr;
   ---------------------------------------------------------------------------

   function RefConstStr
     (Str : not null access constant String)
      return String_Ref is

   begin
      return S:String_Ref do
         S.Const:=Str;
      end return;
   end RefConstStr;
   ---------------------------------------------------------------------------

end Basics;
