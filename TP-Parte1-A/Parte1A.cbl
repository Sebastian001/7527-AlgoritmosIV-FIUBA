       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. "TP_PARTE_1A".
       AUTHOR. "Adrian Mouly - Sebastian Torres".
       DATE-WRITTEN. "1er cuatrimestre 2014".

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT NOVTIMES1_FILE
           ASSIGN TO "./files/in/NovTimes1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES1.

           SELECT NOVTIMES2_FILE
           ASSIGN TO "./files/in/NovTimes2.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES2.

           SELECT NOVTIMES3_FILE
           ASSIGN TO "./files/in/NovTimes3.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES3.

           SELECT PROFESORES_FILE
           ASSIGN TO "./files/in/Profesores.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-PROFESORES.

           SELECT SUCURSALES_FILE
           ASSIGN TO "./files/in/Sucursales.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSALES.

           SELECT TIPOSCLASE_FILE
           ASSIGN TO "./files/in/TiposClase.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TIPOSCLASE.

           SELECT TIMES_FILE
           ASSIGN TO "./files/out/Times.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TIMES.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.

       77 FS-NOVTIMES1      PIC X(2).
       77 FS-NOVTIMES2      PIC X(2).
       77 FS-NOVTIMES3      PIC X(2).
       77 FS-PROFESORES     PIC X(2).
       77 FS-SUCURSALES     PIC X(2).
       77 FS-TIPOSCLASE     PIC X(2).
       77 FS-TIMES          PIC X(2).

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

       INICIALIZAR.
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

       CERRAR-ARCHIVOS.
           CLOSE NOVTIMES1_FILE.
           CLOSE NOVTIMES2_FILE.
           CLOSE NOVTIMES3_FILE.
           CLOSE PROFESORES_FILE.
           CLOSE SUCURSALES_FILE.
           CLOSE TIPOSCLASE_FILE.
           CLOSE TIMES_FILE.

       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            DISPLAY "Hello world"
            STOP RUN.
      ** add other procedures here
       END PROGRAM "TP_PARTE_1A".
