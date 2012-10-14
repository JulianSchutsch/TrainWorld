with Config;
with Basics; use Basics;

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
     (Name : String_Ref)
      return Boolean;

   function Utilize
     (ConfigNode : Config.ConfigNode_Type;
      Parameters : Parameter_Type)
      return Implementation_Type;

   procedure Register
     (Name           : String_Ref;
      Compatible     : Implementation_Compatible;
      Constructor    : Implementation_Constructor);

end Implementations;
