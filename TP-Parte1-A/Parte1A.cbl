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
           SELECT NOVTIMES1
           ASSIGN TO "./files/in/NovTimes1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES1.

           SELECT NOVTIMES2
           ASSIGN TO "./files/in/NovTimes2.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES2.

           SELECT NOVTIMES3
           ASSIGN TO "./files/in/NovTimes3.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVTIMES3.

           SELECT PROFESORES
           ASSIGN TO "./files/in/Profesores.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-PROFESORES.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.

       77 FS-NOVTIMES1      PIC X(2).
       77 FS-NOVTIMES2      PIC X(2).
       77 FS-NOVTIMES3      PIC X(2).
       77 FS-PROFESORES     PIC X(2).

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            DISPLAY "Hello world"
            STOP RUN.
      ** add other procedures here
       END PROGRAM "TP_PARTE_1A".
