with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config;

generic
   type Implementation_Type is private;
   type Parameter_Type is private;
package Implementations is

   type Implementation_Constructor is
     access function
       (GenConfig  : Config.Config_ClassAccess;
        ImplConfig : Config.Config_ClassAccess;
        Parameters : Parameter_Type)
        return Implementation_Type;

   type Implementation_Compatible is
     access function
       (GenConfig  : Config.Config_ClassAccess;
        ImplConfig : Config.Config_ClassAccess;
        Parameters : Parameter_Type)
        return Boolean;

   ImplementationRegisteredTwice : Exception;
   ImplementationNotFound        : Exception;

   function Has
     (Name : Unbounded_String)
      return Boolean;

   function Utilize
     (ConfigNode : Config.ConfigNode_Type;
      Parameters : Parameter_Type)
      return Implementation_Type;

   procedure Register
     (Name           : Unbounded_String;
      Compatible     : Implementation_Compatible;
      Constructor    : Implementation_Constructor);

end Implementations;
