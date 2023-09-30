IMPLEMENTATION MODULE Host;
  IMPORT SYSTEM, Storage, InOut;

  (* This environment is provided by the MetroWerks Modula-2 implementation for *)
  (* the MacIntosh, and uses the ASCII character set. *)

  PROCEDURE ASCIIOrdinalOf(C: CHAR): ASCIIOrdinal;
  BEGIN
    RETURN ORD(C)
  END ASCIIOrdinalOf;

  (* The operations for controlling dynamic allocation and deallocation of *)
  (* storage are performed using operators defined by the MetroWerks library *)
  (* module Storage. *)

  PROCEDURE New(VAR Pointer: AnyPointerType; ObjectSize: CARDINAL);
  BEGIN
    Storage.ALLOCATE(Pointer, ObjectSize)
  END New;

  PROCEDURE Dispose(VAR Pointer: AnyPointerType; ObjectSize: CARDINAL);
  BEGIN
    Storage.DEALLOCATE(Pointer, ObjectSize); Pointer := NIL
  END Dispose;

  (* The operations for performing input and output of text files are *)
  (* implemented using the standard InOut module *)

  TYPE File = POINTER TO FileDesc;
       FileDesc = RECORD AccessMode: FileAccessMode END;

  PROCEDURE OpenFile(OpenPrompt: ARRAY OF CHAR; Mode: FileAccessMode;
                     VAR F: File; VAR Opened: BOOLEAN);
  BEGIN
    InOut.WriteString(OpenPrompt);
    New(F, SIZE(FileDesc)); F^.AccessMode := Mode;
    CASE Mode OF
    | Input: InOut.OpenInput("")
    | Output: InOut.OpenOutput("")
    END;
    Opened := InOut.Done
  END OpenFile;

  PROCEDURE CloseFile(VAR F: File);
  BEGIN
    CASE F^.AccessMode OF
    | Input: InOut.CloseInput
    | Output: InOut.CloseOutput
    END;
    Dispose(F, SIZE(FileDesc))
  END CloseFile;

  PROCEDURE ReadChar(F: File; VAR EndOfInput: BOOLEAN; VAR C: CHAR);
  BEGIN
    F^.AccessMode := F^.AccessMode; (* Hack to avoid compiler error *)
    InOut.Read(C); EndOfInput := NOT InOut.Done
  END ReadChar;

  PROCEDURE WriteChar(F: File; C: CHAR);
  BEGIN
    F^.AccessMode := F^.AccessMode; (* Hack to avoid compiler error *)
    InOut.Write(C)
  END WriteChar;

  PROCEDURE WriteCardinal(F: File; Number, FieldWidth: CARDINAL);
  BEGIN
    F^.AccessMode := F^.AccessMode; (* Hack to avoid compiler error *)
    InOut.WriteCard(Number, FieldWidth)
  END WriteCardinal;

  PROCEDURE WriteInteger(F: File; Number: INTEGER; FieldWidth: CARDINAL);
  BEGIN
    F^.AccessMode := F^.AccessMode; (* Hack to avoid compiler error *)
    InOut.WriteInt(Number, FieldWidth)
  END WriteInteger;

  PROCEDURE WriteString(F: File; String: ARRAY OF CHAR);
  BEGIN
    F^.AccessMode := F^.AccessMode; (* Hack to avoid compiler error *)
    InOut.WriteString(String)
  END WriteString;

  PROCEDURE WriteLine(F: File);
  BEGIN
    F^.AccessMode := F^.AccessMode; (* Hack to avoid compiler error *)
    InOut.WriteLn
  END WriteLine;

  PROCEDURE ComparisonOf(W1, W2: Word): WordRelation;
    VAR I: [1..MaxSignificantIdLength];
  BEGIN
    I := 1;
    LOOP
      IF W1[I] < W2[I] THEN RETURN FirstIsLess
      ELSIF W1[I] > W2[I] THEN RETURN SecondIsLess
      ELSIF I = MaxSignificantIdLength THEN RETURN WordsEqual
      ELSE INC(I)
      END
    END
  END ComparisonOf;

  VAR StandardIdSpelling: ARRAY [Integer..True] OF Word;

  PROCEDURE GetSpelling(S: StandardIdentifiers; VAR Spelling: Word);
  BEGIN
    Spelling := StandardIdSpelling[S]
  END GetSpelling;

BEGIN
  BlankWord := "        ";
  StandardIdSpelling[Integer] := "INTEGER";
  StandardIdSpelling[Char] := "CHAR";
  StandardIdSpelling[Boolean] := "BOOLEAN";
  StandardIdSpelling[False] := "FALSE";
  StandardIdSpelling[True] := "TRUE";
END Host.
