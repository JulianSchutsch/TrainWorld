package VersionParser is

   InvalidVersionString : Exception;

   type Version_Type is array(Natural range <>) of Natural;

   function Parse
     (StringData : String)
      return Version_Type;

end VersionParser;
