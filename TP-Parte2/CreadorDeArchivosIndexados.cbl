      ******************************************************************
      * Author: Sebastian Torres y Adrian Mouly
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREADOR-DE-ARCHIVOS-INDEXADOS.
       AUTHOR. "ADRIAN MOULY - SEBASTIAN TORRES".
       DATE-WRITTEN. "2DO CUATRIMESTRE 2015".

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT IN-TIMES
           ASSIGN TO DISK "../files/in/ArchivosTexto/Times.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-IN-TIMES.

       SELECT OUT-TIMES-SEQ
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Times.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS TIM-CLAVE OF REG-OUT-TIMES-SEQ
           ALTERNATE RECORD KEY IS TIM-CUIT OF REG-OUT-TIMES-SEQ
           WITH DUPLICATES
           FILE STATUS IS FS-OUT-TIMES.

       SELECT OUT-TIMES-RND
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Times.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS TIM-CLAVE OF REG-OUT-TIMES-RND
           ALTERNATE RECORD KEY IS TIM-CUIT OF REG-OUT-TIMES-RND
           WITH DUPLICATES
           FILE STATUS IS FS-OUT-TIMES.

       SELECT IN-PROFESORES
           ASSIGN TO DISK "../files/in/ArchivosTexto/Profesores.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-IN-PROFESORES.

       SELECT OUT-PROFESORES-SEQ
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Profesores.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS PROF-NUMERO OF REG-OUT-PROFESORES-SEQ
           FILE STATUS IS FS-OUT-PROFESORES.

       SELECT OUT-PROFESORES-RND
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Profesores.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS PROF-NUMERO OF REG-OUT-PROFESORES-RND
           FILE STATUS IS FS-OUT-PROFESORES.

       SELECT IN-SUCURSALES
           ASSIGN TO DISK "../files/in/ArchivosTexto/Sucursales.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-IN-SUCURSALES.

       SELECT OUT-SUCURSALES-SEQ
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Sucursales.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS SUC-SUCURSAL OF REG-OUT-SUCURSALES-SEQ
           ALTERNATE RECORD KEY IS SUC-CUIT OF REG-OUT-SUCURSALES-SEQ
           ALTERNATE RECORD KEY IS SUC-RAZON OF REG-OUT-SUCURSALES-SEQ
           WITH DUPLICATES
           FILE STATUS IS FS-OUT-SUCURSALES.

       SELECT OUT-SUCURSALES-RND
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Sucursales.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS SUC-SUCURSAL OF REG-OUT-SUCURSALES-RND
           ALTERNATE RECORD KEY IS SUC-CUIT OF REG-OUT-SUCURSALES-RND
           ALTERNATE RECORD KEY IS SUC-RAZON OF REG-OUT-SUCURSALES-RND
           WITH DUPLICATES
           FILE STATUS IS FS-OUT-SUCURSALES.

       SELECT IN-TARIFAS
           ASSIGN TO DISK "../files/in/ArchivosTexto/Tarifas.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-IN-TARIFAS.

       SELECT OUT-TARIFAS-SEQ
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Tarifas.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS TAR-CLAVE OF REG-OUT-TARIFAS-SEQ
           FILE STATUS IS FS-OUT-TARIFAS.

       SELECT OUT-TARIFAS-RND
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Tarifas.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS TAR-CLAVE OF REG-OUT-TARIFAS-RND
           FILE STATUS IS FS-OUT-TARIFAS.

       DATA DIVISION.
       FILE SECTION.

       FD IN-TIMES
           LABEL RECORD IS STANDARD.
       01 REG-IN-TIMES.
           03 TIM-CLAVE.
               05 TIM-NUMERO   PIC X(05).
               05 TIM-FECHA    PIC 9(08).
               05 TIM-CUIT     PIC 9(11).
               05 TIM-SEC      PIC 9(04).
           03 TIM-TIP-CLASE    PIC X(04).
           03 TIM-HORAS        PIC 9(2)V99.


       FD OUT-TIMES-RND
           LABEL RECORD IS STANDARD.
       01 REG-OUT-TIMES-RND.
           03 TIM-CLAVE.
               05 TIM-NUMERO   PIC X(05).
               05 TIM-FECHA    PIC 9(08).
               05 TIM-CUIT     PIC 9(11).
               05 TIM-SEC      PIC 9(04).
           03 TIM-TIP-CLASE    PIC X(04).
           03 TIM-HORAS        PIC 9(2)V99.

       FD OUT-TIMES-SEQ
           LABEL RECORD IS STANDARD.
       01 REG-OUT-TIMES-SEQ.
           03 TIM-CLAVE.
               05 TIM-NUMERO   PIC X(05).
               05 TIM-FECHA    PIC 9(08).
               05 TIM-CUIT     PIC 9(11).
               05 TIM-SEC      PIC 9(04).
           03 TIM-TIP-CLASE    PIC X(04).
           03 TIM-HORAS        PIC 9(02)V99.

       FD IN-PROFESORES
           LABEL RECORD IS STANDARD.
       01 REG-IN-PROFESORES.
           03  PROF-NUMERO     PIC X(05).
           03  PROF-DNI        PIC 9(08).
           03  PROF-NOMBRE     PIC X(25).
           03  PROF-DIRE       PIC X(20).
           03  PROF-TEL        PIC X(20).

       FD OUT-PROFESORES-RND
           LABEL RECORD IS STANDARD.
       01 REG-OUT-PROFESORES-RND.
           03  PROF-NUMERO     PIC X(05).
           03  PROF-DNI        PIC 9(08).
           03  PROF-NOMBRE     PIC X(25).
           03  PROF-DIRE       PIC X(20).
           03  PROF-TEL        PIC X(20).

       FD OUT-PROFESORES-SEQ
           LABEL RECORD IS STANDARD.
       01 REG-OUT-PROFESORES-SEQ.
           03  PROF-NUMERO     PIC X(05).
           03  PROF-DNI        PIC 9(08).
           03  PROF-NOMBRE     PIC X(25).
           03  PROF-DIRE       PIC X(20).
           03  PROF-TEL        PIC X(20).

       FD IN-SUCURSALES
           LABEL RECORD IS STANDARD.
       01 REG-IN-SUCURSALES.
           03  SUC-SUCURSAL    PIC X(03).
           03  SUC-RAZON       PIC X(25).
           03  SUC-DIRE        PIC X(20).
           03  SUC-TEL         PIC X(20).
           03  SUC-CUIT        PIC 9(11).

       FD OUT-SUCURSALES-RND
           LABEL RECORD IS STANDARD.
       01 REG-OUT-SUCURSALES-RND.
           03  SUC-SUCURSAL    PIC X(03).
           03  SUC-RAZON       PIC X(25).
           03  SUC-DIRE        PIC X(20).
           03  SUC-TEL         PIC X(20).
           03  SUC-CUIT        PIC 9(11).

       FD OUT-SUCURSALES-SEQ
           LABEL RECORD IS STANDARD.
       01 REG-OUT-SUCURSALES-SEQ.
           03  SUC-SUCURSAL    PIC X(03).
           03  SUC-RAZON       PIC X(25).
           03  SUC-DIRE        PIC X(20).
           03  SUC-TEL         PIC X(20).
           03  SUC-CUIT        PIC 9(11).

       FD IN-TARIFAS
           LABEL RECORD IS STANDARD.
       01 REG-IN-TARIFAS.
           03  TAR-CLAVE.
               05 TAR-TIP-CLASE PIC X(04).
               05 TAR-VIG-DES   PIC 9(08).
           03  TAR-TARIFA       PIC 9(05)V99.

       FD OUT-TARIFAS-RND
           LABEL RECORD IS STANDARD.
       01 REG-OUT-TARIFAS-RND.
           03  TAR-CLAVE.
               05 TAR-TIP-CLASE PIC X(04).
               05 TAR-VIG-DES   PIC 9(08).
           03  TAR-TARIFA       PIC 9(05)V99.

       FD OUT-TARIFAS-SEQ
           LABEL RECORD IS STANDARD.
       01 REG-OUT-TARIFAS-SEQ.
           03  TAR-CLAVE.
               05 TAR-TIP-CLASE PIC X(04).
               05 TAR-VIG-DES   PIC 9(08).
           03  TAR-TARIFA       PIC 9(05)V99.

       WORKING-STORAGE SECTION.
       01 FS-IN-TIMES       PIC XX.
       01 FS-OUT-TIMES      PIC XX.

       01 FS-IN-PROFESORES  PIC XX.
       01 FS-OUT-PROFESORES PIC XX.

       01 FS-IN-SUCURSALES  PIC XX.
       01 FS-OUT-SUCURSALES PIC XX.

       01 FS-IN-TARIFAS     PIC XX.
       01 FS-OUT-TARIFAS    PIC XX.

       01 WS-EXIT           PIC X.
       01 CANT-TIMES        PIC 9(10) VALUE ZEROES.
       01 CANT-PROFESORES   PIC 9(10) VALUE ZEROES.
       01 CANT-SUCURSALES   PIC 9(10) VALUE ZEROES.
       01 CANT-TARIFAS      PIC 9(10) VALUE ZEROES.


       PROCEDURE DIVISION.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM CREAR-TIMES.
           PERFORM IMPRIMIR-TIMES.
           PERFORM CREAR-PROFESORES.
           PERFORM IMPRIMIR-PROFESORES.
           PERFORM CREAR-SUCURSALES.
           PERFORM IMPRIMIR-SUCURSALES.
           PERFORM CREAR-TARIFAS.
           PERFORM IMPRIMIR-TARIFAS.
           PERFORM CERRAR-ARCHIVOS.
           ACCEPT WS-EXIT.
           STOP RUN.

       ABRIR-ARCHIVOS.
           OPEN INPUT IN-TIMES.
           OPEN INPUT IN-PROFESORES.
           OPEN INPUT IN-SUCURSALES.
           OPEN INPUT IN-TARIFAS.

           OPEN OUTPUT OUT-TIMES-RND.
           OPEN OUTPUT OUT-PROFESORES-RND.
           OPEN OUTPUT OUT-SUCURSALES-RND.
           OPEN OUTPUT OUT-TARIFAS-RND.

       *>
       *> ARCHIVO TIMES
       *>

       CREAR-TIMES.
           READ IN-TIMES.
           PERFORM CARGAR-TIMES UNTIL FS-IN-TIMES <> 00.
           CLOSE OUT-TIMES-RND.

       CARGAR-TIMES.
           MOVE CORRESPONDING REG-IN-TIMES TO REG-OUT-TIMES-RND.
           MOVE CORRESPONDING REG-IN-TIMES TO
           TIM-CLAVE OF REG-OUT-TIMES-RND.
           WRITE REG-OUT-TIMES-RND.
           IF (FS-OUT-TIMES <> 00)
               DISPLAY "ERROR AL ESCRIBIR EL ARCHIVO TIMES: "
               FS-OUT-TIMES.
           READ IN-TIMES.

       IMPRIMIR-TIMES.
           OPEN INPUT OUT-TIMES-SEQ.
           READ OUT-TIMES-SEQ.
           PERFORM CHEQUEO-TIMES UNTIL FS-OUT-TIMES <> 00.
           DISPLAY "--------------------".
           DISPLAY "TOTAL REGISTROS TIMES: " CANT-TIMES.
           CLOSE OUT-TIMES-SEQ.

       CHEQUEO-TIMES.
           DISPLAY "------TIMES-------".
           DISPLAY "NUMERO: " TIM-NUMERO OF REG-OUT-TIMES-SEQ.
           DISPLAY "FECHA: " TIM-FECHA OF REG-OUT-TIMES-SEQ.
           DISPLAY "CUIT: " TIM-CUIT OF REG-OUT-TIMES-SEQ.
           DISPLAY "NRO. SEC: " TIM-SEC OF REG-OUT-TIMES-SEQ.
           DISPLAY "TIP. CLASE: " TIM-TIP-CLASE OF REG-OUT-TIMES-SEQ.
           DISPLAY "HORAS: " TIM-HORAS OF REG-OUT-TIMES-SEQ.
           ADD 1 TO CANT-TIMES.
           READ OUT-TIMES-SEQ.

       *>
       *> ARCHIVO DE PROFESORES
       *>

       CREAR-PROFESORES.
           READ IN-PROFESORES.
           PERFORM CARGAR-PROFESORES UNTIL FS-IN-PROFESORES <> 00.
           CLOSE OUT-PROFESORES-RND.

       CARGAR-PROFESORES.
           MOVE CORRESPONDING REG-IN-PROFESORES TO
           REG-OUT-PROFESORES-RND.
           WRITE REG-OUT-PROFESORES-RND.
           IF (FS-OUT-PROFESORES <> 00)
               DISPLAY "ERROR AL ESCRIBIR EL ARCHIVO DE PROFESORES: "
               FS-OUT-PROFESORES.
           READ IN-PROFESORES.

       IMPRIMIR-PROFESORES.
           OPEN INPUT OUT-PROFESORES-SEQ.
           READ OUT-PROFESORES-SEQ.
           PERFORM CHEQUEO-PROFESORES UNTIL FS-OUT-PROFESORES <> 00.
           DISPLAY "--------------------".
           DISPLAY "TOTAL REGISTROS PROFESORES: " CANT-PROFESORES.
           CLOSE OUT-PROFESORES-SEQ.

       CHEQUEO-PROFESORES.
           DISPLAY "------PROFESORES-------".
           DISPLAY "NUMERO: " PROF-NUMERO OF REG-OUT-PROFESORES-SEQ.
           DISPLAY "DNI: " PROF-DNI OF REG-OUT-PROFESORES-SEQ.
           DISPLAY "NOMBRE: " PROF-NOMBRE OF REG-OUT-PROFESORES-SEQ.
           DISPLAY "DIRECCION: " PROF-DIRE OF REG-OUT-PROFESORES-SEQ.
           DISPLAY "TELEFONO: " PROF-TEL OF REG-OUT-PROFESORES-SEQ.
           ADD 1 TO CANT-PROFESORES.
           READ OUT-PROFESORES-SEQ.

       *>
       *> ARCHIVO DE SUCURSALES
       *>

       CREAR-SUCURSALES.
           READ IN-SUCURSALES.
           PERFORM CARGAR-SUCURSALES UNTIL FS-IN-SUCURSALES <> 00.
           CLOSE OUT-SUCURSALES-RND.

       CARGAR-SUCURSALES.
           MOVE CORRESPONDING REG-IN-SUCURSALES TO
           REG-OUT-SUCURSALES-RND.
           WRITE REG-OUT-SUCURSALES-RND.
           IF (FS-OUT-SUCURSALES <> 00)
               DISPLAY "ERROR AL ESCRIBIR EL ARCHIVO DE SUCURSALES: "
               FS-OUT-SUCURSALES.
           READ IN-SUCURSALES.

       IMPRIMIR-SUCURSALES.
           OPEN INPUT OUT-SUCURSALES-SEQ.
           READ OUT-SUCURSALES-SEQ.
           PERFORM CHEQUEO-SUCURSALES UNTIL FS-OUT-SUCURSALES <> 00.
           DISPLAY "--------------------".
           DISPLAY "TOTAL REGISTROS SUCURSALES: " CANT-SUCURSALES.
           CLOSE OUT-SUCURSALES-SEQ.

       CHEQUEO-SUCURSALES.
           DISPLAY "------SUCURSALES-------".
           DISPLAY "SUCURSAL: " SUC-SUCURSAL OF
           REG-OUT-SUCURSALES-SEQ.
           DISPLAY "RAZON: " SUC-RAZON OF
           REG-OUT-SUCURSALES-SEQ.
           DISPLAY "DIRECCION: " SUC-DIRE OF
           REG-OUT-SUCURSALES-SEQ.
           DISPLAY "TELEFONO: " SUC-TEL OF REG-OUT-SUCURSALES-SEQ.
           ADD 1 TO CANT-SUCURSALES.
           READ OUT-SUCURSALES-SEQ.

       *>
       *> ARCHIVO DE TARIFAS
       *>

       CREAR-TARIFAS.
           READ IN-TARIFAS.
           PERFORM CARGAR-TARIFAS UNTIL FS-IN-TARIFAS <> 00.
           CLOSE OUT-TARIFAS-RND.

       CARGAR-TARIFAS.
           MOVE CORRESPONDING REG-IN-TARIFAS TO
           REG-OUT-TARIFAS-RND.
           MOVE CORRESPONDING REG-IN-TARIFAS TO
           TAR-CLAVE OF REG-OUT-TARIFAS-RND.
           WRITE REG-OUT-TARIFAS-RND.
           IF (FS-OUT-TARIFAS <> 00)
               DISPLAY "ERROR AL ESCRIBIR EL ARCHIVO DE TARIFAS: "
               FS-OUT-TARIFAS.
           READ IN-TARIFAS.

       IMPRIMIR-TARIFAS.
           OPEN INPUT OUT-TARIFAS-SEQ.
           READ OUT-TARIFAS-SEQ.
           PERFORM CHEQUEO-TARIFAS UNTIL FS-OUT-TARIFAS <> 00.
           DISPLAY "--------------------".
           DISPLAY "TOTAL REGISTROS TARIFAS: " CANT-TARIFAS.
           CLOSE OUT-TARIFAS-SEQ.

       CHEQUEO-TARIFAS.
           DISPLAY "------TARIFAS-------".
           DISPLAY "TIP. CLASE: " TAR-TIP-CLASE OF
           REG-OUT-TARIFAS-SEQ.
           DISPLAY "VIG. DES: " TAR-VIG-DES OF
           REG-OUT-TARIFAS-SEQ.
           DISPLAY "TARIFA: " TAR-TARIFA OF
           REG-OUT-TARIFAS-SEQ.
           ADD 1 TO CANT-TARIFAS.
           READ OUT-TARIFAS-SEQ.

       CERRAR-ARCHIVOS.
           CLOSE IN-TIMES.
           CLOSE IN-PROFESORES.
           CLOSE IN-SUCURSALES.
           CLOSE IN-TARIFAS.
