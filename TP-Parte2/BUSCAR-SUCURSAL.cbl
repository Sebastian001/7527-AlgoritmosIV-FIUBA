       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUSCAR-SUCURSAL.
       AUTHOR. "ADRIAN MOULY - SEBASTIAN TORRES".
       DATE-WRITTEN. "2DO CUATRIMESTRE 2015".

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT SUCURSALES
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Sucursales.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS SUC-SUCURSAL OF REG-SUCURSALES
           ALTERNATE RECORD KEY IS SUC-CUIT OF REG-SUCURSALES
           ALTERNATE RECORD KEY IS SUC-RAZON OF REG-SUCURSALES
           WITH DUPLICATES
           FILE STATUS IS FS-SUCURSALES.

       DATA DIVISION.
       FILE SECTION.

       FD SUCURSALES
           LABEL RECORD IS STANDARD.
       01 REG-SUCURSALES.
           03  SUC-SUCURSAL    PIC X(03).
           03  SUC-RAZON       PIC X(25).
           03  SUC-DIRE        PIC X(20).
           03  SUC-TEL         PIC X(20).
           03  SUC-CUIT        PIC 9(11).

       WORKING-STORAGE SECTION.
       01 FS-SUCURSALES        PIC XX.
           88 OK-SUC                   VALUE "00".
           88 NO-SUC                   VALUE "23".
           88 EOF-SUC                  VALUE "10".


       LINKAGE SECTION.

       01 PAR-IN.
           03  IN-OP                   PIC X(06).
           03  IN-CUIT                 PIC 9(11).

       01 PAR-OUT.
           03 OUT-CR                  PIC X(02).
           03 OUT-REG-SUCURSALES.
               05  SUC-SUCURSAL    PIC X(03).
               05  SUC-RAZON       PIC X(25).
               05  SUC-DIRE        PIC X(20).
               05  SUC-TEL         PIC X(20).
               05  SUC-CUIT        PIC 9(11).

       PROCEDURE DIVISION USING PAR-IN, PAR-OUT.

           IF (IN-OP IS EQUAL TO "ABRIR")
               PERFORM ABRIR-SUCURSALES
           END-IF.
           IF (IN-OP IS EQUAL TO "CERRAR")
               PERFORM CERRAR-SUCURSALES
           END-IF.
           IF (IN-OP IS EQUAL TO "BUSCAR")
               PERFORM PROCESAR-SUCURSALES
           END-IF.
           EXIT PROGRAM.


       ABRIR-SUCURSALES.
           OPEN INPUT SUCURSALES.
           IF (NOT OK-SUC)
               DISPLAY "ERROR AL ABRIR ARCHIVO SUCURSALES FS: "
                 FS-SUCURSALES
           END-IF.

       CERRAR-SUCURSALES.
           CLOSE SUCURSALES.

       PROCESAR-SUCURSALES.
           DISPLAY "PROCESAR SUCURSALES".
           MOVE IN-CUIT TO SUC-CUIT OF REG-SUCURSALES.

           READ SUCURSALES RECORD
               KEY IS SUC-CUIT OF REG-SUCURSALES.

           IF (OK-SUC)
               MOVE REG-SUCURSALES TO OUT-REG-SUCURSALES
           ELSE IF (NO-SUC)
               DISPLAY "SUCURSAL NO ENCONTRADA."
           ELSE IF (EOF-SUC)
               DISPLAY "FIN DE ARCHIVO SUCURSALES."

           END-IF.

           MOVE FS-SUCURSALES TO OUT-CR.
