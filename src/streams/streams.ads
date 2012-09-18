with Ada.Finalization;
with System;
with Types; use Types;

package Streams is

   -- This exception may be thrown if either no more data is available
   -- or no more data can be written to a limited length stream
   StreamOverflow : Exception;

   type StreamSize_Type is new Natural;

   type ReadStream_Type is abstract new Ada.Finalization.Controlled with null record;
   type ReadStream_ClassAccess is access all ReadStream_Type'Class;

   function Read
     (ReadStream : ReadStream_Type)
      return Integer32 is abstract;

   procedure ReadBuffer
     (ReadStream : ReadStream_Type;
      Buffer     : System.Address;
      BufferSize : StreamSize_Type) is abstract;

   function Size
     (ReadStream : ReadStream_Type)
      return StreamSize_Type is abstract;
   ---------------------------------------------------------------------------

   type WriteStream_Type is abstract new Ada.Finalization.Controlled with null record;
   type WriteStream_ClassAccess is access all WriteStream_Type'Class;

   procedure Write
     (WriteStream : WriteStream_Type;
      Int         : Integer32) is abstract;

   procedure WriteBuffer
     (WriteStream : WriteStream_Type;
      Buffer      : System.Address;
      BufferSize  : StreamSize_Type) is abstract;
   ---------------------------------------------------------------------------

end Streams;
