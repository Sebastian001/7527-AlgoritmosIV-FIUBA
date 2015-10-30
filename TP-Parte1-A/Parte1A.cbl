       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. "TP_PARTE_1A".
       AUTHOR. "Adrian Mouly - Sebastian Torres".
       DATE-WRITTEN. "2do cuatrimestre 2015".

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT NOVTIMES1_FILE
           ASSIGN TO "../files/in/NovTimes1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES1.

           SELECT NOVTIMES2_FILE
           ASSIGN TO "../files/in/NovTimes2.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES2.

           SELECT NOVTIMES3_FILE
           ASSIGN TO "../files/in/NovTimes3.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES3.

           SELECT PROFESORES_FILE
           ASSIGN TO "../files/in/Profesores.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-PROFESORES.

           SELECT SUCURSALES_FILE
           ASSIGN TO "../files/in/Sucursales.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSALES.

           SELECT TIPOSCLASE_FILE
           ASSIGN TO "../files/in/TiposClase.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TIPOSCLASE.

           SELECT TIMES_FILE
           ASSIGN TO "../files/out/Times.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TIMES.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.

      *-------------------------------*
      *- SUCURSALES FILE DESCRIPTION -*
      *-------------------------------*
       FD SUCURSALES_FILE LABEL RECORD STANDARD.
       01 REG-SUCURSALES.
              03 SUC-SUCURSAL      PIC X(03).
              03 SUC-RAZON         PIC X(25).
              03 SUC-DIRE          PIC X(20).
              03 SUC-TEL           PIC X(20).
              03 SUC-CUIT          PIC 9(11).

      *-------------------------------*
      *- TIPOSCLASE FILE DESCRIPTION -*
      *-------------------------------*
       FD TIPOSCLASE_FILE LABEL RECORD STANDARD.
       01 REG-TIPOSCLASE.
              03 TIP-CLASE  PIC X(04).
              03 TIP-DESC   PIC X(20).
              03 TIP-TARIFA PIC 9(5)V99.

      *-----------------------
       WORKING-STORAGE SECTION.

       77 SUCURSALES-EOF    PIC X(2)      VALUE "NO".
          88 EOF                          VALUE "SI".

       77 TIPOSCLASE-EOF    PIC X(2)      VALUE "NO".
          88 EOF                          VALUE "SI".

       77 NOVTIMES1-EOF     PIC X(2)      VALUE "NO".
          88 EOF                          VALUE "SI".

       77 NOVTIMES2-EOF     PIC X(2)      VALUE "NO".
          88 EOF                          VALUE "SI".

       77 NOVTIMES3-EOF     PIC X(2)      VALUE "NO".
          88 EOF                          VALUE "SI".

       77 PROFESORES-EOF    PIC X(2)      VALUE "NO".
          88 EOF                          VALUE "SI".

       77 FS-NOVTIMES1      PIC X(2).
       77 FS-NOVTIMES2      PIC X(2).
       77 FS-NOVTIMES3      PIC X(2).
       77 FS-PROFESORES     PIC X(2).
       77 FS-SUCURSALES     PIC X(2).
       77 FS-TIPOSCLASE     PIC X(2).
       77 FS-TIMES          PIC X(2).

       01 FECHA-DE-HOY.
           03  FECHA-AAAA      pic 9(4).
           03  FECHA-MM        pic 9(2).
           03  FECHA-DD        pic 9(2).

       01 ENCABEZADO1.
           03  FILLER      PIC X(9)    VALUE "Fecha: ".
           03  FECHA-DD    PIC 9(2).
           03  FILLER      PIC X       VALUE "/".
           03  FECHA-MM    PIC 9(2).
           03  FILLER      PIC X       VALUE "/".
           03  FECHA-AAAA  PIC 9(4).
           03  FILLER      PIC X(50)   VALUE SPACES.
           03  FILLER      PIC X(6)    VALUE "Hoja: ".
           03  E1-HOJA     PIC 9(3).

       01 ENCABEZADO2.
           03 FILLER PIC x(26) VALUE SPACES.
           03 FILLER PIC X(38) VALUE "Listado de horas aplicadas".
           03 FILLER PIC x(26) VALUE SPACES.

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      *- INICIO LLAMADO A PROCEDIMIENTOS

           PERFORM INICIALIZAR.
           PERFORM PRINT-ENCABEZADO.
           PERFORM ABRIR-ARCHIVOS.

           PERFORM LEER-TIPOSCLASE.
           PERFORM LEER-NOVTIMES1.
           PERFORM LEER-NOVTIMES2.
           PERFORM LEER-NOVTIMES3.

           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

      *- FIN LLAMADO A PROCEDIMIENTOS

       INICIALIZAR.
           DISPLAY "Inicializar Variables".

       PRINT-ENCABEZADO.
           MOVE FUNCTION CURRENT-DATE TO FECHA-DE-HOY.
           MOVE CORRESPONDING FECHA-DE-HOY TO ENCABEZADO1.
           DISPLAY ENCABEZADO1.
           DISPLAY ENCABEZADO2.

       ABRIR-ARCHIVOS.
           OPEN INPUT NOVTIMES1_FILE.
           IF FS-NOVTIMES1 IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR NOVTIMES1 FS: " FS-NOVTIMES1
               STOP RUN
           END-IF.

           OPEN INPUT NOVTIMES2_FILE.
           IF FS-NOVTIMES2 IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR NOVTIMES2 FS: " FS-NOVTIMES2
               STOP RUN
           END-IF.

           OPEN INPUT NOVTIMES3_FILE.
           IF FS-NOVTIMES3 IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR NOVTIMES3 FS: " FS-NOVTIMES3
               STOP RUN
           END-IF.

           OPEN INPUT PROFESORES_FILE.
           IF FS-PROFESORES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR PROFESORES FS: " FS-PROFESORES
               STOP RUN
           END-IF.

           OPEN INPUT SUCURSALES_FILE.
           IF FS-SUCURSALES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR SUCURSALES FS: " FS-SUCURSALES
               STOP RUN
           END-IF.

           OPEN INPUT TIPOSCLASE_FILE.
           IF FS-TIPOSCLASE IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR TIPOSCLASE FS: " FS-TIPOSCLASE
               STOP RUN
           END-IF.

           OPEN INPUT TIMES_FILE.
           IF FS-TIMES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIMES
               STOP RUN
           END-IF.

       LEER-SUCURSALES.
           READ SUCURSALES_FILE
           RECORD AT END MOVE "SI" TO SUCURSALES-EOF.
           IF FS-SUCURSALES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER SUCURSALES FS: " FS-SUCURSALES
           END-IF.

       LEER-NOVTIMES1.
           READ NOVTIMES1_FILE
           RECORD AT END MOVE "SI" TO NOVTIMES1-EOF.
           IF FS-NOVTIMES1 IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER NOVTIMES1 FS: " FS-NOVTIMES1
           END-IF.

       LEER-NOVTIMES2.
           READ NOVTIMES2_FILE
           RECORD AT END MOVE "SI" TO NOVTIMES2-EOF.
           IF FS-NOVTIMES2 IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER NOVTIMES2 FS: " FS-NOVTIMES2
           END-IF.

       LEER-NOVTIMES3.
           READ NOVTIMES3_FILE
           RECORD AT END MOVE "SI" TO NOVTIMES3-EOF.
           IF FS-NOVTIMES3 IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER NOVTIMES3 FS: " FS-NOVTIMES3
           END-IF.

       LEER-PROFESORES.
           READ PROFESORES_FILE
           RECORD AT END MOVE "SI" TO PROFESORES-EOF.
           IF FS-PROFESORES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER PROFESORES FS: " FS-PROFESORES
           END-IF.

       LEER-TIPOSCLASE.
           READ TIPOSCLASE_FILE
           RECORD AT END MOVE "SI" TO TIPOSCLASE-EOF.
           IF FS-TIPOSCLASE IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER TIPOS-CLASE FS: " FS-TIPOSCLASE
           END-IF.

       CERRAR-ARCHIVOS.
           CLOSE NOVTIMES1_FILE.
           CLOSE NOVTIMES2_FILE.
           CLOSE NOVTIMES3_FILE.
           CLOSE PROFESORES_FILE.
           CLOSE SUCURSALES_FILE.
           CLOSE TIPOSCLASE_FILE.
           CLOSE TIMES_FILE.

       END PROGRAM "TP_PARTE_1A".
