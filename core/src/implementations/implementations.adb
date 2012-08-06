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

   function FindImplementation
     (Name : Unbounded_String)
      return Implementation_Type is

      use type List_Pack.Cursor;

      Cursor  : List_Pack.Cursor;

   begin

      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         if List_Pack.Element(Cursor).Name=Name then
            return List_Pack.Element(Cursor).Constructor.all;
         end if;
      end loop;
      raise ImplementationNotFound;

   end FindImplementation;
   ---------------------------------------------------------------------------

   function FindAnyImplementation
     return Implementation_Type is

      use type List_Pack.Cursor;

   begin

      if List.First=List_Pack.No_Element then
         raise ImplementationNotFound;
      end if;
      return List_Pack.Element(List.First).Constructor.all;

   end FindAnyImplementation;
   ---------------------------------------------------------------------------

   function HasImplementation
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

   end HasImplementation;
   ---------------------------------------------------------------------------

   procedure RegisterImplementation
     (Name        : Unbounded_String;
      Constructor : Implementation_Constructor) is
   begin

      if HasImplementation(Name) then
         raise ImplementationRegisteredTwice;
      end if;
      List.Append
        ((Name        => Name,
          Constructor => Constructor));

   end RegisterImplementation;
   ---------------------------------------------------------------------------

end Implementations;
