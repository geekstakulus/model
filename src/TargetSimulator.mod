MODULE TargetSimulator;
  IMPORT SYSTEM;
  FROM Target IMPORT WordLength, (* AddressRange, *) Instruction, OrderCode,
                     Integer, Register, Level, C, V, E, H, M, R, I;
  FROM InOut IMPORT Read, Write, WriteString, WriteLn;
                    (* Use standard I/O channels *)

  TYPE Word = Integer;
       AddressRange = [0..8191]; (* should be imported *)

  VAR Memory: ARRAY AddressRange OF Word;

  VAR Reg: ARRAY [BP..PC] OF AddressRange;
      PSReg: BITSET;
      IR: Instruction; (* Instruction Register *)

  PROCEDURE DisplayDiagnostics;
    VAR FatalError: BOOLEAN; Bit: [V..I];
  BEGIN
    FatalError := (PSReg*{I, R, M} <> {}) OR
                  (PSReg*{V, E} <> {}) AND (C IN PSReg);
    IF FatalError THEN
      WriteString("PREMATURE END OF PROGRAM EXECUTION ... ");
      FOR Bit := V TO I DO
        IF Bit IN PSReg THEN
          CASE Bit OF
          | V: WriteString("ARITHMETIC OVERFLOW")
          | E: WriteString("RANGE ERROR")
          | R: WriteString("REGISTER VIOLATION")
          | M: WriteString("MEMORY VIOLATION")
          | I: WriteString("ILLEGAL INSTRUCTION")
          ELSE (* do nothing *)
            WriteString("INVALID PSReg, ");
          END (* CASE *)
        END (* IF *)
      END (* FOR *)
    END; (* IF *)
    WriteLn; WriteLn;

    (*...Could dump registers and other useful *)
    (* machine state details at this point... *)
  END DisplayDiagnostics;

  PROCEDURE InvalidAddress(W: Word): BOOLEAN;
  BEGIN
    RETURN (W < 0) OR (W > MAX(AddressRange))
  END InvalidAddress;

  PROCEDURE SetRegister(RegId: Register; Value: Word);
  BEGIN
    IF InvalidAddress(Value) THEN INCL(PSReg, R)
    ELSE Reg[RegId] := Value
    END
  END SetRegister;

  PROCEDURE ModifyRegister(RegId: Register; Increment: Word);
    VAR RegisterViolation: BOOLEAN;
  BEGIN
    IF Increment > 0 THEN
      RegisterViolation := (Reg[RegId] > MAX(AddressRange) - SYSTEM.CAST(AddressRange, Increment))
    ELSE RegisterViolation := (Reg[RegId] + SYSTEM.CAST(AddressRange, Increment)) < 0
    END; (* IF *)
    IF RegisterViolation THEN INCL(PSReg, R)
    ELSE INC(Reg[RegId], Increment)
    END
  END ModifyRegister;

  PROCEDURE AddressOf(N: Integer; RegId: Register; L: Level): AddressRange;
    (* Determines memory address specified by N, [RegId, Level]. *)
    VAR NextAddress: AddressRange; Temporary: Word; I : Level;
        MemoryViolation: BOOLEAN;
  BEGIN
    NextAddress := Reg[RegId];
    FOR I := 1 TO L DO
      Temporary := Memory[NextAddress];
      IF InvalidAddress(Temporary) THEN INCL(PSReg, M)
      ELSE NextAddress := Temporary
      END
    END;
    IF N > 0 THEN
      MemoryViolation := (NextAddress > MAX(AddressRange) - SYSTEM.CAST(AddressRange, N))
    ELSE MemoryViolation := (NextAddress + SYSTEM.CAST(AddressRange, N)) < 0
    END;
    IF MemoryViolation THEN INCL(PSReg, M)
    ELSE INC(NextAddress, N)
    END;
    RETURN NextAddress
  END AddressOf;

  PROCEDURE Transfer(NumberOfWords, Source, Destination: Word);
    (* Moves specified NumberOfWords from memory *)
    (* address Source to the memory address Destination *)
    VAR L: AddressRange;
  BEGIN
    IF (NumberOfWords < 1) OR (NumberOfWords > MAX(AddressRange)) THEN
      INCL(PSReg, I)
    ELSIF InvalidAddress(Source) OR InvalidAddress(Destination) OR 
          (MAX(AddressRange) - Source < NumberOfWords - 1) OR
          (MAX(AddressRange) - Destination < NumberOfWords - 1) THEN
      INCL(PSReg, M)
    ELSE
      FOR L := 0 TO SYSTEM.CAST(AddressRange, NumberOfWords)-1 DO
        Memory[SYSTEM.CAST(AddressRange, Destination) + L] := Memory[SYSTEM.CAST(AddressRange, Source) + L]
      END (* FOR *)
    END (* IF *)
  END Transfer;

  PROCEDURE StackSafe(MinimumSize, ExtraWordsNeeded: CARDINAL): BOOLEAN;
    (* Checks for possible stack underflow and overflow. *)
  BEGIN
    IF (Reg[SP] >= MinimumSize) AND
       (Reg[SP] <= MAX(AddressRange) - ExtraWordsNeeded) THEN
      RETURN TRUE
    ELSE INCL(PSReg, R); RETURN FALSE
    END
  END StackSafe;

  TYPE ArithmeticOperator = (Plus, Minus, Times, Div, Mod);

  PROCEDURE DoArithmetic(VAR Operand1: Word; Operand2: Word;
                         Operator: ArithmeticOperator);
    (* Performs *)
    (*  Operand1 := Operand1 Operator Operand2 *)
    (* having first checked for arithmetic overflow and sets the *)
    (* Overflow bit in the Processor Status Register accordingly. *)
    VAR Overflow: BOOLEAN;
  BEGIN
    Overflow := FALSE;
    CASE Operator OF
    | Plus, Minus:
        IF Operator = Minus THEN Operand2 := -Operand2 END;
        IF (Operand1 > 0) AND (Operand2 > 0)  THEN
          Overflow := (Operand1 > MAX(Word) - Operand2)
        ELSIF (Operand1 < 0) AND (Operand2 < 0) THEN
          Overflow := (Operand1 < MIN(Word) - Operand2)
        END;
        IF NOT Overflow THEN INC(Operand1, Operand2) END
    | Times:
        IF ABS(Operand1) > MAX(Word) DIV ABS(Operand2) THEN
          Overflow := TRUE
        ELSE Operand1 := Operand1 * Operand2
        END
    | Div:
        IF Operand2 = 0 THEN Overflow := TRUE
        ELSE Operand1 := Operand1 DIV Operand2
        END
    | Mod:
        IF Operand2 = 0 THEN Overflow := TRUE
        ELSE Operand1 := Operand1 MOD Operand2
        END
    END;
    IF Overflow THEN INCL(PSReg, V) ELSE EXCL(PSReg, V) END
  END DoArithmetic;

  PROCEDURE ExecuteInstruction;
    VAR Ch: CHAR; TopWordOnStack: BITSET; I, ShiftLength: [0..WordLength];
  BEGIN
    WITH IR DO (* decode instruction *)
      CASE OpCode OF
      | NOOP: (* do nothing *)
      | LOAD:
          IF StackSafe(0, 1) THEN
            Memory[Reg[SP]] := Memory[AddressOf(N, R, L)]; ModifyRegister(SP, 1)
          END
      | LOADL:
          IF StackSafe(0, 1) THEN
            Memory[Reg[SP]] := N; ModifyRegister(SP, 1)
          END
      | LOADR:
          IF StackSafe(0, 1) THEN
            Memory[Reg[SP]] := Reg[R]; ModifyRegister(SP, 1)
          END
      | LOADA:
          IF StackSafe(0, 1) THEN
            Memory[Reg[SP]] := AddressOf(N, R, L); ModifyRegister(SP, 1)
          END
      | LOADI:
          IF StackSafe(0, 1) THEN
            ModifyRegister(SP, -1);
            Transfer(N, Memory[Reg[SP]], Reg[SP]); ModifyRegister(SP, N)
          END
      | STORE:
          IF StackSafe(0, 1) THEN
            ModifyRegister(SP, -1);
            Memory[AddressOf(N, R, L)] := Memory[Reg[SP]]
          END
      | STORER:
          IF StackSafe(0, 1) THEN
            ModifyRegister(SP, -1); SetRegister(R, Memory[Reg[SP]])
          END
      | STOREI:
          IF StackSafe(0, 1) THEN
            ModifyRegister(SP, -1); 
            Transfer(N, Reg[SP]-SYSTEM.CAST(AddressRange, N), Memory[Reg[SP]]);
            ModifyRegister(SP, N) 
          END
      | STZ:
          Memory[AddressOf(N, R, L)] := 0 
      | INCR:
          IF StackSafe(0, 1) THEN
           DoArithmetic(Memory[Reg[SP]-1], N, Plus) 
          END
      | INCREG:
          ModifyRegister(R, N);
      | MOVE:
          IF StackSafe(2, 0) THEN
            Transfer(N, Memory[Reg[SP]-2], Memory[Reg[SP]-1]);
            ModifyRegister(SP, -2); 
          END
      | SLL:
          (* The SLL and SRL implementations assume that the machine *)
          (* on which the simulation takes place has a word length *)
          (* not less than that of the Target computer (16 bits). *)
          (* This is probably so -- however, if not, then they must *)
          (* be rewritten using e.g. arrays of BITSET! *)
          IF StackSafe(1, 0) THEN
            ShiftLength := N MOD WordLength;
            TopWordOnStack := SYSTEM.CAST(BITSET, Memory[Reg[SP]-1]);
            FOR I := WordLength-1-ShiftLength TO 0 BY -1 DO
              IF I IN TopWordOnStack THEN
                INCL(TopWordOnStack, I+ShiftLength)
              ELSE EXCL(TopWordOnStack, I+ShiftLength)
              END (* IF *)
            END; (* FOR *)
            (* zero fill at right *)
            FOR I := 0 TO ShiftLength-1 DO EXCL(TopWordOnStack, I) END;
            Memory[Reg[SP]-1] := SYSTEM.CAST(Word, TopWordOnStack)
          END
      | SRL:
          IF StackSafe(1, 0) THEN
            ShiftLength := N MOD WordLength;
            TopWordOnStack := SYSTEM.CAST(BITSET, Memory[Reg[SP]-1]);
            FOR I := ShiftLength TO WordLength-1 DO
              IF I IN TopWordOnStack THEN
                INCL(TopWordOnStack, I-ShiftLength)
              ELSE EXCL(TopWordOnStack, I-ShiftLength)
              END (* IF *)
            END; (* FOR *)
            (* zero fill at right *)
            FOR I := WordLength-ShiftLength TO WordLength-1 DO 
              EXCL(TopWordOnStack, I) 
            END;
            Memory[Reg[SP]-1] := SYSTEM.CAST(Word, TopWordOnStack)
          END
      | ADD:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            DoArithmetic(Memory[Reg[SP]-1], Memory[Reg[SP]], Plus)
          END
      | SUB:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            DoArithmetic(Memory[Reg[SP]-1], Memory[Reg[SP]], Minus)
          END
      | MUL:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            DoArithmetic(Memory[Reg[SP]-1], Memory[Reg[SP]], Times)
          END
      | DVD:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            DoArithmetic(Memory[Reg[SP]-1], Memory[Reg[SP]], Div)
          END
      | DREM:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            DoArithmetic(Memory[Reg[SP]-1], Memory[Reg[SP]], Mod)
          END
      | LAND:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            Memory[Reg[SP]-1] := ORD(Memory[Reg[SP]-1] * Memory[Reg[SP]] > 0);
          END
      | LOR:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            Memory[Reg[SP]-1] := ORD(Memory[Reg[SP]-1] + Memory[Reg[SP]] > 0);
          END
      | INV:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            Memory[Reg[SP]-1] := ABS(Memory[Reg[SP]-1] - 1)
          END
      | NEG:
          (* Subtract from 0 in case we are negating MIN(Taret.Integer) *)
          IF StackSafe(1, 0) THEN
            Memory[Reg[SP]] := Memory[Reg[SP]-1]; Memory[Reg[SP]-1] := 0;
            DoArithmetic(Memory[Reg[SP]-1], Memory[Reg[SP]], Minus)
          END
      | CLT:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            Memory[Reg[SP]-1] := ORD(Memory[Reg[SP]-1] < Memory[Reg[SP]]);
          END
      | CLE:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            Memory[Reg[SP]-1] := ORD(Memory[Reg[SP]-1] <= Memory[Reg[SP]]);
          END
      | CEQ:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            Memory[Reg[SP]-1] := ORD(Memory[Reg[SP]-1] = Memory[Reg[SP]]);
          END
      | CNE:
          IF StackSafe(2, 0) THEN
            ModifyRegister(SP, -1); 
            Memory[Reg[SP]-1] := ORD(Memory[Reg[SP]-1] <> Memory[Reg[SP]]);
          END
      | BRN:
          Reg[PC] := AddressOf(N, R, L)
      | BIDX:
          IF StackSafe(1, 0) THEN
            ModifyRegister(SP, -1); 
            IF N < 0 THEN INCL(PSReg, I)
            ELSIF (N > MAX(AddressRange) DIV Memory[Reg[SP]]) THEN
              INCL(PSReg, M)
            ELSE ModifyRegister(PC, Memory[Reg[SP]] * N)
            END
          END
      | BZE:
          IF StackSafe(1, 0) THEN
            ModifyRegister(SP, -1); 
            IF Memory[Reg[SP]] = 0 THEN Reg[PC] := AddressOf(N, R, L) END
          END
      | BNZ:
          IF StackSafe(1, 0) THEN
            ModifyRegister(SP, -1); 
            IF Memory[Reg[SP]] <> 0 THEN Reg[PC] := AddressOf(N, R, L) END
          END
      | BNG:
          IF StackSafe(1, 0) THEN
            ModifyRegister(SP, -1); 
            IF Memory[Reg[SP]] < 0 THEN Reg[PC] := AddressOf(N, R, L) END
          END
      | BPZ:
          IF StackSafe(1, 0) THEN
            ModifyRegister(SP, -1); 
            IF Memory[Reg[SP]] >= 0 THEN Reg[PC] := AddressOf(N, R, L) END
          END
      | BVS:
          IF V IN PSReg THEN Reg[PC] := AddressOf(N, R, L) END;
          EXCL(PSReg, V)
      | BES:
          IF E IN PSReg THEN Reg[PC] := AddressOf(N, R, L) END;
          EXCL(PSReg, E)
      | MARK:
          IF StackSafe(0, N) THEN
            Reg[MP] := Reg[SP]; ModifyRegister(SP, N); 
          END
      | CALL:
          Memory[Reg[MP]+1] := Reg[FP]; Memory[Reg[MP]+2] := Reg[PC];
          Reg[FP] := Reg[MP]; Reg[PC] := AddressOf(N, R, L)
      | EXT:
          Reg[SP] := Reg[FP]; SetRegister(FP, Memory[Reg[SP]+1]);
          SetRegister(PC, Memory[Reg[SP]+2]);
      | SETSP:
          Reg[SP] := AddressOf(N, R, L)
      | SETPSR:
          PSReg := SYSTEM.CAST(BITSET, N)
      | CHECK:
          IF StackSafe(3, 0) THEN
            ModifyRegister(SP, -2); 
            IF (Memory[Reg[SP]-1] >= Memory[Reg[SP]]) AND 
               (Memory[Reg[SP]-1] <= Memory[Reg[SP]+1]) THEN
              EXCL(PSReg, E)
            ELSE INCL(PSReg, E)
            END
          END
      | HLT:
          INCL(PSReg, H)
      | CHIN:
          IF StackSafe(0, 1) THEN
            Read(Ch); Memory[Reg[SP]] := ORD(Ch); ModifyRegister(SP, 1)
          END
      | CHOUT:
          IF StackSafe(1, 0) THEN
            ModifyRegister(SP, -1); Write(CHR(Memory[Reg[SP]]))
          END
      ELSE (* do nothing *)
        WriteString("INVALID INSTRUCTION");
        WriteLn
      END (* CASE *)
    END (* WITH *)
  END ExecuteInstruction;

  PROCEDURE FetchInstruction;
    VAR Temporary: CARDINAL;
  BEGIN
    WITH IR DO
      (* set N, R, L from PC^ *)
      Temporary := SYSTEM.CAST(CARDINAL, SYSTEM.CAST(BITSET, Memory[Reg[PC]]));
      L := Temporary MOD 64; Temporary := Temporary DIV 64;
      R := SYSTEM.CAST(Register, Temporary MOD 4);
      OpCode := SYSTEM.CAST(OrderCode, Temporary DIV 4);
      N := Memory[Reg[PC]+1]
    END; (* WITH *)
    ModifyRegister(PC, 2)
  END FetchInstruction;

  PROCEDURE ExaminePSR;
  BEGIN
    IF (PSReg*{I, R, M} <> {}) OR
       (PSReg*{V, E} <> {}) AND (C IN PSReg) THEN (* fatal error *)
      INCL(PSReg, H)
    END
  END ExaminePSR;

  PROCEDURE InitializeRegisters;
    VAR R : Register;
  BEGIN
    FOR R := BP TO PC DO Reg[R] := 0 END;
    PSReg := {}
  END InitializeRegisters;

  PROCEDURE LoadProgram;
  BEGIN
    (* Loads a program from somewhere; depends upon *)
    (* where and how object programs are stored. *)
  END LoadProgram;

BEGIN
  LoadProgram;
  InitializeRegisters;
  REPEAT
    FetchInstruction;
    ExecuteInstruction;
    ExaminePSR
  UNTIL H IN PSReg;
  DisplayDiagnostics
END TargetSimulator.
