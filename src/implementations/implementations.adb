with Ada.Containers.Doubly_Linked_Lists;

package body Implementations is

   type List_Entry is
      record
         Name           : Unbounded_String;
         Constructor    : Implementation_Constructor;
      end record;

   package List_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => List_Entry,
      "="          => "=");

   List : List_Pack.List;

   function Utilize
     (ConfigNode : Config.ConfigNode_Type)
      return Implementation_Type is

      use type List_Pack.Cursor;

      Cursor         : List_Pack.Cursor;
      Implementation : constant Unbounded_String:=ConfigNode.GetImplementation;

   begin

      if Implementation="" then
         if List.First=List_Pack.No_Element then
            raise ImplementationNotFound;
         end if;
         declare
            First : constant List_Entry:=List_Pack.Element(List.First);
         begin
            return First.Constructor
              (ConfigNode.GetConfig,
               ConfigNode.GetImplConfig(First.Name));
         end;
      end if;

      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         if List_Pack.Element(Cursor).Name=Implementation then
            return List_Pack.Element(Cursor).Constructor
              (ConfigNode.GetConfig,
               ConfigNode.GetImplConfig(Implementation));
         end if;
         Cursor:=List_Pack.Next(Cursor);
      end loop;
      raise ImplementationNotFound;

   end Utilize;
   ---------------------------------------------------------------------------

   function Has
     (Name : Unbounded_String)
      return Boolean is

      use type List_Pack.Cursor;

      Cursor : List_Pack.Cursor;

   begin

      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         if List_Pack.Element(Cursor).Name=Name then
            return True;
         end if;
         Cursor:=List_Pack.Next(Cursor);
      end loop;
      return False;

   end Has;
   ---------------------------------------------------------------------------

   procedure Register
     (Name        : Unbounded_String;
      Constructor : Implementation_Constructor) is
   begin

      if Has(Name) then
         raise ImplementationRegisteredTwice;
      end if;
      List.Append
        ((Name        => Name,
          Constructor => Constructor));

   end Register;
   ---------------------------------------------------------------------------

end Implementations;
