with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config;

generic
   type Implementation_Type is private;
package Implementations is

   type Implementation_Constructor is
     access function
       (GenConfig  : Config.Config_ClassAccess;
        ImplConfig : Config.Config_ClassAccess)
        return Implementation_Type;

   ImplementationRegisteredTwice : Exception;
   ImplementationNotFound        : Exception;

   function Has
     (Name : Unbounded_String)
      return Boolean;

   function Utilize
     (ConfigNode : Config.ConfigNode_Type)
      return Implementation_Type;

   procedure Register
     (Name           : Unbounded_String;
      Constructor    : Implementation_Constructor);

end Implementations;
