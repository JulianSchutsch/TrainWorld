with Ada.Task_Identification; use Ada.Task_Identification;

package body ThreadLocalStorage is

   type StorageElement_Type is
      record
         TaskID  : Task_Id:=Null_Task_Id;
         Content : Content_ClassAccess:=null;
      end record;

   type StorageElement_Array is array(0..999) of StorageElement_Type;

   protected type Storage_Type is

      procedure Set
        (ID      : Task_ID;
         Content : Content_ClassAccess);

      procedure Get
        (ID      : Task_ID;
         Content : out Content_ClassAccess);

   private

      Elements : StorageElement_Array;

   end Storage_Type;

   protected body Storage_Type is

      procedure Set
        (ID      : Task_ID;
         Content : Content_ClassAccess) is
      begin

         for i in Elements'Range loop

            if Elements(i).TaskID=ID then
               Elements(i).Content := Content;
               return;
            end if;

            if Elements(i).TaskID=Null_Task_Id then
               Elements(i).TaskID  := ID;
               Elements(i).Content := Content;
               return;
            end if;

         end loop;

         raise ThreadLocalStorageOverflow;

      end Set;
      ------------------------------------------------------------------------

      procedure Get
        (ID      : Task_ID;
         Content : out Content_ClassAccess) is
      begin

         for i in Elements'Range loop

            if Elements(i).TaskID=ID then
               Content:=Elements(i).Content;
               return;
            end if;

         end loop;

         Content:=null;

      end Get;
      ------------------------------------------------------------------------

   end Storage_Type;
   ---------------------------------------------------------------------------

   Storage : Storage_Type;

   procedure Set(Content : Content_ClassAccess) is
   begin
      Storage.Set(Current_Task,Content);
   end Set;
   ---------------------------------------------------------------------------

   procedure Get(Content : out Content_ClassAccess) is
   begin
      Storage.Get(Current_Task,Content);
   end Get;
   ---------------------------------------------------------------------------

end ThreadLocalStorage;
