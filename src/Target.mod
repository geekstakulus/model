IMPLEMENTATION MODULE Target;
  IMPORT Host;

  PROCEDURE OrdinalOf(C: Host.ASCIIOrdinal): Ordinal;
  BEGIN
    RETURN C
  END OrdinalOf;

  PROCEDURE AddressOf(Routine: SystemRoutines): AddressRange;
  BEGIN
    CASE Routine OF
    | InitializeIO: RETURN 10;
    | FinalizeIO:   RETURN 30;
    | ReadInteger:  RETURN 50;
    | WriteInteger: RETURN 100;
    END
  END AddressOf;

END Target.
