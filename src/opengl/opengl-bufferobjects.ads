package OpenGL.BufferObjects is

   type BufferObject is private;

private
   type BufferObject is
      record
         ID : GLuint_Type:=0;
      end record;

end OpenGL.BufferObjects;
