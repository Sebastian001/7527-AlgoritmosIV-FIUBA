       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. "TP_PARTE_1B".
       AUTHOR. "Adrian Mouly - Sebastian Torres".
       DATE-WRITTEN. "2do cuatrimestre 2015".

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT SUCURSALES_FILE
           ASSIGN TO "../files/in/Sucursales.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSALES.

           SELECT TIMES_FILE
           ASSIGN TO "../files/out/Times.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TIMES.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------

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

      *--------------------------*
      *- TIMES FILE DESCRIPTION -*
      *--------------------------*
       FD TIMES_FILE LABEL RECORD STANDARD.
       01 REG-TIMES.
           03 CLAVE-TIMES.
              05 CLAVE-TIMES-SUC.
                  07 CLAVE-TIMES-FECHA.
                      09 TIM-NUMERO        PIC X(5).
                      09 TIM-FECHA         PIC 9(8).
                  07 TIM-SUCURSAL          PIC X(03).
           03 TIM-TIPCLASE                 PIC X(04).
           03 TIM-HORAS                    PIC 9(2)V99.

       WORKING-STORAGE SECTION.
      *-----------------------

       77 FS-TIMES          PIC X(2).
       77 FS-SUCURSALES     PIC X(2).

       77 TOT-GRAL          PIC 9999999999V99.

       01 FECHA-ACTUAL.
           03  FECHA-ACTUAL-AAAA      PIC 9(4).
           03  FECHA-ACTUAL-MM        PIC 9(2).
           03  FECHA-ACTUAL-DD        PIC 9(2).

       01 VEC.
           03 VEC-SUCURSALES
               OCCURS 3 TIMES
               INDEXED BY INDICE.
               05  VEC-SUCURSALES-SUCURSAL        PIC X(03).
               05  VEC-SUCURSALES-RAZON           PIC X(25).

       01 VEC-ANIOS.
           03  VEC-ANIOS-ELEM
               OCCURS 5 TIMES PIC 9(4).

       01 VEC-TOT-MENSUAL.
           03  VEC-TOT-MENSUAL-ELM
               OCCURS 12 TIMES PIC 9(4).


       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

      *- INICIO LLAMADO A PROCEDIMIENTOS

           PERFORM INICIALIZAR.
           PERFORM ABRIR-ARCHIVOS.

           PERFORM LEER-SUCURSALES.
           PERFORM CARGAR-SUCURSALES.

           PERFORM IMPRIMIR-ENCABEZADO-1.
           PERFORM GENERAR-ANIOS.
           PERFORM LEER-TIMES.

           PERFORM PROCESO1.
           PERFORM ESCRIBIR-ESTADISTICAS.

           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

      *- FIN LLAMADO A PROCEDIMIENTOS

       INICIALIZAR.
           DISPLAY "Inicializar Variables".

           MOVE 0 TO TOT-GRAL.

       ABRIR-ARCHIVOS.
           OPEN INPUT SUCURSALES_FILE.
           IF FS-SUCURSALES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR SUCURSALES FS: " FS-SUCURSALES
               STOP RUN
           END-IF.

           OPEN INPUT TIMES_FILE.
           IF FS-TIMES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIMES
               STOP RUN
           END-IF.

       LEER-SUCURSALES.
           READ SUCURSALES_FILE.
           IF FS-SUCURSALES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER SUCURSALES FS: " FS-SUCURSALES
           END-IF.

       CARGAR-SUCURSALES.
           PERFORM GUARDAR-SUCURSAL
                  VARYING INDICE FROM 1 BY 1
                  UNTIL INDICE > 3
                  OR FS-SUCURSALES IS EQUAL TO 10.

       GUARDAR-SUCURSAL.
           DISPLAY "Guardar Sucursal - " SUC-SUCURSAL.

           MOVE SUC-SUCURSAL TO VEC-SUCURSALES-SUCURSAL(INDICE).
           MOVE SUC-RAZON TO VEC-SUCURSALES-RAZON(INDICE).

           PERFORM LEER-SUCURSALES.

       IMPRIMIR-ENCABEZADO-1.
           DISPLAY "Encabezado 1".

       GENERAR-ANIOS.
           DISPLAY "Generar anios".

       LEER-TIMES.
           READ TIMES_FILE.
           IF FS-TIMES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER TIMES FS: " FS-SUCURSALES
           END-IF.

       PROCESO1.
           DISPLAY "Proceso 1".

       ESCRIBIR-ESTADISTICAS.
           DISPLAY "Escribir en Estadisticas".

       CERRAR-ARCHIVOS.
           CLOSE SUCURSALES_FILE.
           CLOSE TIMES_FILE.

       END PROGRAM "TP_PARTE_1B".
