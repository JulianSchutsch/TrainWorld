pragma Ada_2012;

with System;
with RefCount;

package Streams is

   -- This exception may be thrown if either no more data is available
   -- or no more data can be written to a limited length stream
   StreamOverflow : Exception;

   type StreamSize_Type is new Natural;

   type ReadStream_Interface is abstract new RefCount.Ref_Interface with null record;
   type ReadStream_ClassAccess is access all ReadStream_Interface'Class;

   procedure ReadBuffer
     (ReadStream : in out ReadStream_Interface;
      Buffer     : System.Address;
      BufferSize : StreamSize_Type) is abstract;
   ---------------------------------------------------------------------------

   package ReadStreamRef is new RefCount.Ref(ReadStream_Interface,ReadStream_ClassAccess);

   subtype ReadStream_Ref is ReadStreamRef.Ref_Type;

   type WriteStream_Interface is abstract new RefCount.Ref_Interface with null record;
   type WriteStream_ClassAccess is access all WriteStream_Interface'Class;

   procedure WriteBuffer
     (WriteStream : in out WriteStream_Interface;
      Buffer      : System.Address;
      BufferSize  : StreamSize_Type) is abstract;
   ---------------------------------------------------------------------------

   procedure Flush
     (WriteStream : in out WriteStream_Interface) is abstract;

   package WriteStreamRef is new RefCount.Ref(WriteStream_Interface,WriteStream_ClassAccess);

   subtype WriteStream_Ref is WriteStreamRef.Ref_Type;

end Streams;
