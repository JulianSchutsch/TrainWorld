with OpenGL.Test; use OpenGL.Test;
with Ada.Numerics.Float_Random;

package body OpenGL.LinearBuffer.Test is

   procedure TestAllocation is
   begin

      BindEvents;
      -- Automatic finalization
      declare
         Buffers     : LinearBuffers_Type;
         BufferRange : LinearRange_Ref;
      begin
         Buffers.SetBufferBlockSize(1024*1024);
         Buffers.Allocate(1024,BufferRange);
      end;

      -- Manual finalization for BufferRange
      declare
         Buffers     : LinearBuffers_Type;
         BufferRange : LinearRange_Ref;
      begin
         Buffers.SetBufferBlockSize(1024*1024);
         Buffers.Allocate(1024,BufferRange);
         BufferRange.SetNull;
      end;

      UnbindEvents;

   end TestAllocation;
   ---------------------------------------------------------------------------

   procedure TestMonteCarlo is

      RangeCount : constant:=10000;
      MaxSize    : constant:=1024*1024;

      use Ada.Numerics.Float_Random;

   begin

      BindEvents;

      declare
         Buffers : LinearBuffers_Type;
         Ranges  : array(0..RangeCount-1) of LinearRange_Ref;
         Gen     : Ada.Numerics.Float_Random.Generator;
         Current : Natural;
         Size    : PtrInt_Type;

      begin
         Reset(Gen);
         Buffers.SetBufferBlockSize(1024*1024);
         for i in 1..100000 loop
            Current:=Natural(Float'Rounding(Random(Gen)*Float(RangeCount-1)));
            if Ranges(Current).I=null then
               Size:=PtrInt_Type(Float'Rounding(Random(Gen)*Float(MaxSize-1)))+1;
               begin
                  Buffers.Allocate(Size,Ranges(Current));
               exception
                  when FailedAllocate =>
                     null;
               end;
            else
               Ranges(Current).SetNull;
            end if;
         end loop;
      end;

      UnBindEvents;

   end TestMonteCarlo;
   ---------------------------------------------------------------------------

end OpenGL.LinearBuffer.Test;
