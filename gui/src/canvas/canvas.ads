with Ada.Finalization;

package Canvas is

   type Canvas_Type is new Ada.Finalization.Limited_Controlled with private;

   procedure Finalization
     (Canvas : in out Canvas_Type);

private

   type Canvas_Type is new Ada.Finalization.Limited_Controlled with
      record
         null;
      end record;

end Canvas;
