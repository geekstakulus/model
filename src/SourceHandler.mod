IMPLEMENTATION MODULE SourceHandler;
  FROM Host IMPORT File, FileAccessMode, OpenFile, CloseFile, ReadChar, EOL,
                   WriteChar, WriteString, WriteLine, WriteCardinal;

  CONST MaxErrorsPerLine = 6;

  TYPE  TerminationStatus = (Normal, InputExhausted);

  VAR   SourceFile, ListingFile: File;
        Line: ARRAY CharPosition OF CHAR;
        FirstInLine, LastInLine: CharPosition;
        TotalNumberOfErrors: CARDINAL;
        ErrorsInThisLine: [0..MaxErrorsPerLine]; ErrorOverflow: BOOLEAN;
        ErrorList: ARRAY [1..MaxErrorsPerLine] OF 
                     RECORD
                       ErrorPosition: TextPosition; ErrorCode: CARDINAL
                     END;

  PROCEDURE ListThisLine;
    PROCEDURE ListErrors;
      VAR K: [1..MaxErrorsPerLine]; J: CharPosition;
    BEGIN
      INC(TotalNumberOfErrors, ErrorsInThisLine);
      FOR K := 1 TO ErrorsInThisLine DO
        WITH ErrorList[K] DO
          WriteString(ListingFile, "*****    ");
          IF ErrorPosition.LineNumber <> PositionNow.LineNumber THEN
            WriteString(ListingFile, "ERROR ");
            WriteCardinal(ListingFile, ErrorCode, 1);
            WriteString(ListingFile, " AT CHARACTER ");
            WriteCardinal(ListingFile, ErrorPosition.CharNumber, 1);
            WriteString(ListingFile, " OF LINE ");
            WriteCardinal(ListingFile, ErrorPosition.LineNumber, 1)
          ELSE
            FOR J := 1 TO ErrorPosition.CharNumber-1 DO
              WriteChar(ListingFile, " ")
            END; (* FOR *)
            WriteString(ListingFile, "^ERROR ");
            WriteCardinal(ListingFile, ErrorCode, 1)
          END; (* IF *)
          WriteLine(ListingFile)
        END (* WITH *)
      END; (* FOR *)
      IF ErrorOverflow THEN
        WriteString(ListingFile,
                 "***** FURTHER ERRORS ON THIS LINE SUPPRESSED");
        WriteLine(ListingFile)
      END; (* IF *)
      WriteLine(ListingFile);
      ErrorsInThisLine := 0; ErrorOverflow := FALSE
    END ListErrors;

    VAR I: CharPosition;
  BEGIN (* ListThisLine *)
    WriteCardinal(ListingFile, PositionNow.LineNumber, 5);
    WriteString(ListingFile, "   ");
    FOR I := 1 TO LastInLine DO WriteChar(ListingFile, Line[I]) END;
    WriteLine(ListingFile);
    IF ErrorsInThisLine > 0 THEN ListErrors END
  END ListThisLine;

  PROCEDURE Finalize(Reason: TerminationStatus);
  BEGIN
    IF Reason = Normal THEN ListThisLine END;
    WriteLine(ListingFile); WriteLine(ListingFile);
    IF Reason = Normal THEN WriteString(ListingFile, "COMPILATION COMPLETED.")
    ELSE
      WriteString(ListingFile,
                  "COMPILATION ABORTED - PREMATURE END OF INPUT LINE");
      WriteLine(ListingFile)
    END; (* IF *)
    IF TotalNumberOfErrors = 0 THEN WriteString(ListingFile, "NO")
    ELSE WriteCardinal(ListingFile, TotalNumberOfErrors, 1)
    END; (* IF *)
    WriteString(ListingFile, " ERROR(S) REPORTED");
    WriteLine(ListingFile);
    CloseFile(ListingFile); CloseFile(SourceFile)
  END Finalize;

  PROCEDURE ReadNextLine;
    VAR I: CharPosition; NoInputRemains: BOOLEAN;
  BEGIN
    (* check if there is a next line in the input file *)
    ReadChar(SourceFile, NoInputRemains, Line[1]);
    IF NoInputRemains THEN Finalize(InputExhausted); HALT END;
    I := 1;
    (* read leading spaces and set FirstInLine *)
    WHILE Line[I] = " " DO
      INC(I); ReadChar(SourceFile, NoInputRemains, Line[I]);
    END; (* WHILE *)
    FirstInLine := I;
    (* read remainder of the line *)
    WHILE Line[I] <> EOL DO
      INC(I); ReadChar(SourceFile, NoInputRemains, Line[I])
    END; (* WHILE *)
    Line[I] := " "; LastInLine := I;
    IF FirstInLine <> LastInLine THEN
      (* scan trailing spaces to set LastInLine *)
      REPEAT DEC(I) UNTIL Line[I] <> " ";
      LastInLine := I+1
    END (* IF *)
  END ReadNextLine;

  PROCEDURE GetNextCharacter;
  BEGIN
    WITH PositionNow DO
      IF CharNumber = LastInLine THEN
        ListThisLine; ReadNextLine;
        INC(LineNumber); CharNumber := FirstInLine
      ELSE INC(CharNumber)
      END;
      CurrentCharacter := Line[CharNumber]
    END (* WITH *)
  END GetNextCharacter;

  PROCEDURE Error(Code: CARDINAL; Position: TextPosition);
  BEGIN
    IF ErrorsInThisLine = MaxErrorsPerLine THEN ErrorOverflow := TRUE
    ELSE
      INC(ErrorsInThisLine);
      WITH ErrorList[ErrorsInThisLine] DO
        ErrorCode := Code; ErrorPosition := Position
      END (* WITH *)
    END (* IF *)
  END Error;

  PROCEDURE FinalizeIO;
  BEGIN
    Finalize(Normal)
  END FinalizeIO;

  VAR OK: BOOLEAN;
BEGIN
  OpenFile("Enter name of source text file: ", Input, SourceFile, OK);
  IF OK THEN
    OpenFile("Enter name of listing file: ", Output, ListingFile, OK);
    IF OK THEN
      WriteString(ListingFile, "LISTING PRODUCED BY Model COMPILER 1/93");
      WriteLine(ListingFile); WriteLine(ListingFile);
      ReadNextLine;
      WITH PositionNow DO
        LineNumber := 0; CharNumber := FirstInLine;
        CurrentCharacter := Line[CharNumber]
      END; (* WITH *)
      TotalNumberOfErrors := 0; ErrorsInThisLine := 0; ErrorOverflow := FALSE
    END (* IF *)
  END (* IF *)
END SourceHandler.
