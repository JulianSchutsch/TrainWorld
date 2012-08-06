with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type Implementation_Type(<>) is private;
package Implementations is

   type Implementation_Constructor is
   access function
     return Implementation_Type;

   ImplementationRegisteredTwice : Exception;
   ImplementationNotFound        : Exception;

   function FindImplementation
     (Name : Unbounded_String)
      return Implementation_Type;

   function FindAnyImplementation
     return Implementation_Type;

   procedure RegisterImplementation
     (Name           : Unbounded_String;
      Constructor    : Implementation_Constructor);

end Implementations;
