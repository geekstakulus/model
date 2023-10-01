IMPLEMENTATION MODULE LexicalAnalyzer;
  FROM SourceHandler IMPORT GetNextCharacter, CurrentCharacter, PositionNow, Error;
  IMPORT Target, Host;

  CONST MaxInteger = Target.MaxInteger; MaxIdLength = Host.MaxSignificantIdLength;

  CONST NumberOfWordSymbols = 22;
        WordSymbolTableSize = NumberOfWordSymbols + MaxIdLength;

  TYPE WordSymbolTableRange = [1..WordSymbolTableSize];

  VAR WordSymbols: ARRAY WordSymbolTableRange OF
                     RECORD
                       Spelling: Host.Word; Value: SymbolType
                     END;
      LastOfLength: ARRAY [0..MaxIdLength] OF [0..WordSymbolTableSize];

  PROCEDURE GetNextSymbol;
    VAR K: [0..MaxIdLength]; Digit: Target.Integer; Delimiter: CHAR;
        I: WordSymbolTableRange;

    PROCEDURE IsDigit(Ch: CHAR): BOOLEAN;
    BEGIN
      RETURN (Ch >= "0") AND (Ch <= "9")
    END IsDigit;

    PROCEDURE IsLetter(Ch: CHAR): BOOLEAN;
    BEGIN
      RETURN (Ch >= "A") AND (Ch <= "Z") OR (Ch >= "a") AND (Ch <= "z");
    END IsLetter;
  BEGIN
    WITH SymbolDescription DO
      (* Read characters until next significant character *)
      WHILE CurrentCharacter = " " DO GetNextCharacter END;
      Position := PositionNow;
      CASE CurrentCharacter OF
      | "A".."Z", "a".."z": (* Identifier or word symbol *)
          K := 0; Spelling := Host.BlankWord;
          REPEAT
            IF K < MaxIdLength THEN
              INC(K); Spelling[K] := CurrentCharacter
            END; (* IF *)
            GetNextCharacter
          UNTIL NOT IsDigit(CurrentCharacter) AND NOT IsLetter(CurrentCharacter);
          WordSymbols[LastOfLength[K]].Spelling := Spelling;
          I := LastOfLength[K-1]+1;
          WHILE Host.ComparisonOf(WordSymbols[I].Spelling, Spelling)
                  <> Host.WordsEqual DO INC(I) END;
          Symbol := WordSymbols[I].Value
      | "0".."9": (* Integer constant *)
          Symbol := IntegerNumber; IntValue := 0;
          REPEAT
            Digit := ORD(CurrentCharacter) - ORD("0");
            IF (IntValue < MaxInteger DIV 10) OR (IntValue = MaxInteger DIV 10)
               AND (Digit <= MaxInteger MOD 10)  THEN
              IntValue := 10*IntValue + Digit
            ELSE Error(1, PositionNow); IntValue := 0
            END; (* IF *)
            GetNextCharacter
          UNTIL NOT IsDigit(CurrentCharacter);
      | "'", '"': (* Character constant *)
          Symbol := CharConstant; Delimiter := CurrentCharacter;
          GetNextCharacter;
          IF CurrentCharacter = Delimiter THEN Error(2, PositionNow) END;
          CharValue := Target.OrdinalOf(Host.ASCIIOrdinalOf(CurrentCharacter));
          GetNextCharacter;
          IF CurrentCharacter <> Delimiter THEN Error(3, PositionNow)
          ELSE GetNextCharacter
          END (* IF *)
      (* 2-character operators and delimiters *)
      | ":":
          GetNextCharacter;
          IF CurrentCharacter = "=" THEN Symbol := Becomes; GetNextCharacter
          ELSE Symbol := Colon
          END
      | ".":
          GetNextSymbol;
          IF CurrentCharacter = "." THEN Symbol := Thru; GetNextCharacter
          ELSE Symbol := Period
          END
      | "<":
          GetNextSymbol;
          IF CurrentCharacter = "=" THEN 
            Symbol := LessThanOrEqual; GetNextCharacter
          ELSIF CurrentCharacter = ">" THEN Symbol := NotEquals; GetNextCharacter
          ELSE Symbol := LessThan
          END
      | ">":
          GetNextSymbol;
          IF CurrentCharacter = "=" THEN
            Symbol := GreaterThanOrEqual; GetNextCharacter
          ELSE Symbol := GreaterThan
          END
      (* 1-character operators and delimiters *)
      | "+": Symbol := Plus; GetNextCharacter
      | "-": Symbol := Minus; GetNextCharacter
      | "*": Symbol := Times; GetNextCharacter
      | "=": Symbol := Equals; GetNextCharacter
      | "(": Symbol := LeftParenthesis; GetNextCharacter
      | ")": Symbol := RightParenthesis; GetNextCharacter
      | "[": Symbol := LeftBracket; GetNextCharacter
      | "]": Symbol := RightBracket; GetNextCharacter
      | ",": Symbol := Comma; GetNextCharacter
      | ";": Symbol := Semicolon; GetNextCharacter
      | "|": Symbol := Separator; GetNextCharacter

      ELSE (* Illegal character *)
        Symbol := OtherSymbol; GetNextCharacter
      END (* CASE *)
    END (* WITH *)
  END GetNextSymbol;

  PROCEDURE InitializeLookUpTable;
    VAR I: WordSymbolTableRange;
  BEGIN
    LastOfLength[0]   := 0;   LastOfLength[1]   := 1;   LastOfLength[2]   := 7;
    LastOfLength[2]   := 14;  LastOfLength[4]   := 21;  LastOfLength[5]   := 25;
    LastOfLength[6]   := 27;  LastOfLength[7]   := 28;  LastOfLength[8]   := 29;
    LastOfLength[9]   := 31;  LastOfLength[10]  := 32;  LastOfLength[11]  := 33;
    LastOfLength[12]  := 34;
    I := LastOfLength[1];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    WITH WordSymbols[I+1] DO Value := Do; Spelling := "DO          " END;
    WITH WordSymbols[I+2] DO Value := If; Spelling := "IF          " END;
    WITH WordSymbols[I+3] DO Value := Of; Spelling := "OF          " END;
    WITH WordSymbols[I+4] DO Value := Or; Spelling := "OR          " END;
    WITH WordSymbols[I+5] DO Value := To; Spelling := "TO          " END;
    I := LastOfLength[2];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    WITH WordSymbols[I+1] DO Value := And; Spelling := "AND         " END;
    WITH WordSymbols[I+2] DO Value := Div; Spelling := "DIV         " END;
    WITH WordSymbols[I+3] DO Value := End; Spelling := "END         " END;
    WITH WordSymbols[I+4] DO Value := For; Spelling := "FOR         " END;
    WITH WordSymbols[I+5] DO Value := Not; Spelling := "NOT         " END;
    WITH WordSymbols[I+6] DO Value := Var; Spelling := "VAR         " END;
    I := LastOfLength[3];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    WITH WordSymbols[I+1] DO Value := Case; Spelling := "CASE        " END;
    WITH WordSymbols[I+2] DO Value := Else; Spelling := "ELSE        " END;
    WITH WordSymbols[I+3] DO Value := Exit; Spelling := "EXIT        " END;
    WITH WordSymbols[I+4] DO Value := Loop; Spelling := "LOOP        " END;
    WITH WordSymbols[I+5] DO Value := Read; Spelling := "READ        " END;
    WITH WordSymbols[I+6] DO Value := Then; Spelling := "THEN        " END;
    I := LastOfLength[4];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    WITH WordSymbols[I+1] DO Value := Array; Spelling := "ARRAY       " END;
    WITH WordSymbols[I+2] DO Value := Begin; Spelling := "BEGIN       " END;
    WITH WordSymbols[I+3] DO Value := Write; Spelling := "WRITE       " END;
    I := LastOfLength[5];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    WITH WordSymbols[I+1] DO Value := Module; Spelling := "MODULE      " END;
    I := LastOfLength[6];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    I := LastOfLength[7];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    I := LastOfLength[8];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    WITH WordSymbols[I+1] DO Value := Procedure; Spelling := "PROCEDURE   " END;
    I := LastOfLength[9];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    I := LastOfLength[10];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    I := LastOfLength[11];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
    I := LastOfLength[12];
    WITH WordSymbols[I] DO Value := Identifier; Spelling := Host.BlankWord END;
  END InitializeLookUpTable;
BEGIN
  InitializeLookUpTable;
  GetNextSymbol (* makes the first symbol available *)
END LexicalAnalyzer.
