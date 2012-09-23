package body Graphics is

   procedure CreateConfig
     (Configuration    : in out Config.ConfigNode_Type;
      WindowType       : WindowType_Enum:=WindowTypeWindow;
      BufferKind       : BufferKind_Enum:=BufferKindDefault;
      Height           : Natural:=1024;
      Width            : Natural:=768;
      RedBits          : Natural:=8;
      GreenBits        : Natural:=8;
      BlueBits         : Natural:=8;
      AlphaBits        : Natural:=8;
      DepthBits        : Natural:=24;
      StencilBits      : Natural:=0;
      WindowTitle      : Unbounded_String:=U("Win");
      ApplicationTitle : Unbounded_String:=U("App")) is

   begin
      Configuration.SetConfig(new Context_Config'(
        WindowType       => WindowType,
        BufferKind       => BufferKind,
        Height           => Height,
        Width            => Width,
        RedBits          => RedBits,
        GreenBits        => GreenBits,
        BlueBits         => BlueBits,
        AlphaBits        => AlphaBits,
        DepthBits        => DepthBits,
        StencilBits      => StencilBits,
        WindowTitle      => WindowTitle,
        ApplicationTitle => ApplicationTitle));
   end CreateConfig;

end Graphics;
