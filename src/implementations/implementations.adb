with Ada.Containers.Doubly_Linked_Lists;

package body Implementations is

   type List_Entry is
      record
         Name           : String_Ref;
         Compatible     : Implementation_Compatible;
         Constructor    : Implementation_Constructor;
      end record;

   package List_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => List_Entry,
      "="          => "=");

   List : List_Pack.List;

   function Utilize
     (ConfigNode : Config.ConfigNode_Type;
      Parameters : Parameter_Type)
      return Implementation_Type is

      use type List_Pack.Cursor;

      Cursor         : List_Pack.Cursor;
      Implementation : constant String_Ref:=ConfigNode.GetImplementation;

   begin

      if Implementation="" then
         if List.First=List_Pack.No_Element then
            raise ImplementationNotFound;
         end if;

         Cursor:=List.First;

         while Cursor/=List_Pack.No_Element loop
            declare
               Element : constant List_Entry:=List_Pack.Element(Cursor);
            begin
               if Element.Compatible
                 (ConfigNode.GetConfig,
                  ConfigNode.GetImplConfig(Element.Name),
                  Parameters) then
                  return Element.Constructor
                    (ConfigNode.GetConfig,
                     ConfigNode.GetImplConfig(Element.Name),
                     Parameters);
               end if;
            end;
            Cursor:=List_Pack.Next(Cursor);
         end loop;
         raise ImplementationNotFound;
      end if;

      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         declare
            Element : constant List_Entry:=List_Pack.Element(Cursor);
         begin
            if Element.Name=Implementation and then
              Element.Compatible
                (ConfigNode.GetConfig,
                 ConfigNode.GetImplConfig(Implementation),
                 Parameters) then
               return List_Pack.Element(Cursor).Constructor
                 (ConfigNode.GetConfig,
                  ConfigNode.GetImplConfig(Implementation),
                  Parameters);
            end if;
         end;
         Cursor:=List_Pack.Next(Cursor);
      end loop;
      raise ImplementationNotFound;

   end Utilize;
   ---------------------------------------------------------------------------

   function Has
     (Name : String_Ref)
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
     (Name        : String_Ref;
      Compatible  : Implementation_Compatible;
      Constructor : Implementation_Constructor) is
   begin

      if Has(Name) then
         raise ImplementationRegisteredTwice;
      end if;
      List.Append
        ((Name        => Name,
          Compatible  => Compatible,
          Constructor => Constructor));

   end Register;
   ---------------------------------------------------------------------------

end Implementations;
