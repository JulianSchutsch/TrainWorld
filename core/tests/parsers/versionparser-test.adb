pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

package body VersionParser.Test is

   procedure VersionTest is

      type NumberCase_Enum is
        (NumberCase_SmallLow,
         NumberCase_SmallHigh,
         NumberCase_MediumLow,
         NumberCase_MediumHigh,
         NumberCase_Letter,
         NumberCase_Special,
         NumberCase_Dot,
         NumberCase_Long);

      type DotCase_Enum is
        (DotCase_Dot,
         DotCase_Number,
         DotCase_Letter,
         DotCase_Space);

      procedure Test
        (Length : Integer) is

         LeadingSpace  : array(1..length) of Boolean:=(others => False);
         TrailingSpace : array(1..Length) of Boolean:=(others => False);
         NumberCase    : array(1..Length) of NumberCase_Enum:=(others => NumberCase_Enum'First);
         DotCase       : array(1..Length-1) of DotCase_Enum:=(others => DotCase_Enum'First);
         Result        : Version_Type(1..Length);

         function Increment(Index : Integer:=NumberCase'First)
                            return Boolean;

         function IncrementPos(Index : Integer)
                               return Boolean is
         begin

            pragma Assert(Index<DotCase'Last);

            if DotCase(Index)/=DotCase_Enum'Last then
               DotCase(Index):=DotCase_Enum'Succ(DotCase(Index));
               return True;
            else
               DotCase(Index):=DotCase_Enum'First;
               if Index=DotCase'Last then
                  return False;
               end if;
               return Increment(Index+1);
            end if;

         end IncrementPos;
         ---------------------------------------------------------------------

         function Increment(Index : Integer:=NumberCase'First)
                            return Boolean is
         begin

            if Index>NumberCase'Last then
               return False;
            end if;

            if NumberCase(Index)/=NumberCase_Enum'Last then
               NumberCase(Index):=NumberCase_Enum'Succ(NumberCase(Index));
               return True;
            else
               NumberCase(Index):=NumberCase_Enum'First;
               if Index=NumberCase'Last then
                  return False;
               end if;
               return IncrementPos(Index);
            end if;

         end Increment;
         ---------------------------------------------------------------------

         function IncrementLeadingSpace(Index : Integer:=LeadingSpace'First)
                                        return Boolean is
         begin
            if Index>LeadingSpace'Last then
               return False;
            end if;
            LeadingSpace(Index):=not LeadingSpace(Index);
            if not LeadingSpace(Index) then
               return IncrementLeadingSpace(Index+1);
            else
               return True;
            end if;
         end IncrementLeadingSpace;
         ---------------------------------------------------------------------

         function IncrementTrailingSpace(Index : Integer:=TrailingSpace'First)
                                        return Boolean is
         begin
            if Index>TrailingSpace'Last then
               return False;
            end if;
            TrailingSpace(Index):=not TrailingSpace(Index);
            if not TrailingSpace(Index) then
               return IncrementTrailingSpace(Index+1);
            else
               return True;
            end if;
         end IncrementTrailingSpace;
         ---------------------------------------------------------------------

         function Generate(Index : Integer:=NumberCase'First)
                           return String;

         function GeneratePos(Index : Integer)
                              return String is
         begin

            if Index>DotCase'Last then
               return "";
            end if;

            case DotCase(Index) is
               when DotCase_Dot =>
                  return "."&String'(Generate(Index+1));
               when DotCase_Number =>
                  return " 123"&String'(Generate(Index+1));
               when DotCase_Letter =>
                  return "A"&String'(Generate(Index+1));
               when DotCase_Space =>
                  return "_"&String'(Generate(Index+1));
            end case;

         end GeneratePos;
         ---------------------------------------------------------------------

         function Generate(Index : Integer:=NumberCase'First)
                           return String is

            function Leading return String is
            begin
               if LeadingSpace(Index) then
                  return " ";
               else
                  return "";
               end if;
            end Leading;
            ------------------------------------------------------------------

            function Trailing return String is
            begin
               if TrailingSpace(Index) then
                  return " ";
               else
                  return "";
               end if;
            end Trailing;
            ------------------------------------------------------------------

         begin

            if Index>NumberCase'Last then
               return "";
            end if;

            case NumberCase(Index) is
               when NumberCase_SmallLow =>
                  return Leading&"0"&Trailing&GeneratePos(Index);
               when NumberCase_SmallHigh =>
                  return Leading&"9"&Trailing&GeneratePos(Index);
               when NumberCase_MediumLow =>
                  return Leading&"00"&Trailing&GeneratePos(Index);
               when NumberCase_MediumHigh =>
                  return Leading&"99"&Trailing&GeneratePos(Index);
               when NumberCase_Letter =>
                  return Leading&"A"&Trailing&GeneratePos(Index);
               when NumberCase_Special =>
                  return Leading&"?"&Trailing&GeneratePos(Index);
               when NumberCase_Dot =>
                  return Leading&"."&Trailing&GeneratePos(Index);
               when NumberCase_Long =>
                  return Leading&"123456789"&Trailing&GeneratePos(Index);
            end case;

         end Generate;
         ---------------------------------------------------------------------

         function Compare(Index : Integer:=NumberCase'First)
                          return Boolean is
         begin
            if Index>NumberCase'Last then
               return True;
            end if;
            case NumberCase(Index) is
               when NumberCase_SmallLow =>
                  return (Result(Index)=0) and Compare(Index+1);
               when NumberCase_SmallHigh =>
                  return (Result(Index)=9) and Compare(Index+1);
               when NumberCase_MediumLow =>
                  return (Result(Index)=0) and Compare(Index+1);
               when NumberCase_MediumHigh =>
                  return (Result(Index)=99) and Compare(Index+1);
               when others =>
                  ReportIssue("Missing Exception");
                  return True;
            end case;
         end Compare;
         ---------------------------------------------------------------------

         function ExpectException(Index : Integer:=NumberCase'First)
                                  return Boolean;

         function ExpectExceptionPos(Index : Integer)
                                     return Boolean is
         begin
            if Index>DotCase'Last then
               return False;
            end if;
            if DotCase(Index)/=DotCase_Dot then
               return True;
            else
               return ExpectException(Index+1);
            end if;
         end ExpectExceptionPos;
         ---------------------------------------------------------------------

         function ExpectException(Index : Integer:=NumberCase'First)
                                  return Boolean is
         begin
            if Index>NumberCase'Last then
               return False;
            end if;
            if NumberCase(Index) not in NumberCase_SmallLow
              |NumberCase_SmallHigh
                |NumberCase_MediumLow
                  |NumberCase_MediumHigh then
               return True;
            else
               return ExpectExceptionPos(Index);
            end if;
         end ExpectException;
         ---------------------------------------------------------------------

      begin
         loop
            loop
               loop
                  begin
                     Result:=Parse(Generate);
                     if not Compare then
                        Put_Line(""""&Generate&"""");
                        ReportIssue("Mismatch");
                     end if;
                  exception
                     when E:InvalidVersionString =>
                        if not ExpectException then
                           Put_Line(""""&Generate&"""");
                           ReportIssue("Exception not expected:"&Ada.Exceptions.Exception_Message(E));
                        end if;
                  end;
                  exit when not IncrementLeadingSpace;
               end loop;
               exit when not IncrementTrailingSpace;
            end loop;
            exit when not Increment;
         end loop;
      end Test;
      ------------------------------------------------------------------------

   begin
      begin
         declare
            Result : Version_Type:=Parse("");
            pragma Unreferenced(Result);
         begin
            null;
         end;
         ReportIssue("Expected exception for empty string");
      exception
         when InvalidVersionString =>
            null;
      end;
      begin
         declare
            Result : Version_Type:=Parse("  ");
            pragma Unreferenced(Result);
         begin
            null;
         end;
         ReportIssue("Expected exception for space character only string");
      exception
         when InvalidVersionString =>
            null;
      end;

      for Length in 1..3 loop
         Test(Length);
      end loop;

   end VersionTest;
   ---------------------------------------------------------------------------

end VersionParser.Test;
