with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Basics is
   function U(Str:String) return Unbounded_String renames To_Unbounded_String;
end Basics;
