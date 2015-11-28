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

       77 FS-TIMES             PIC X(2).
       77 FS-SUCURSALES        PIC X(2).
       77 TOT-GRAL             PIC 9999999999V99.

       78 CON-EOF                          VALUE 10.
       78 CON-CANT-ANIOS                   VALUE 5.
       78 CON-CANT-SUC                     VALUE 3.
       78 CON-TOT-MENSUAL                  VALUE 12.

       01 WS-HOJA              PIC 9(3)    VALUE 001.
       01 WS-I                 PIC 9(1)    VALUE 1.
       01 WS-J                 PIC 9(1)    VALUE 4.
       01 WS-IND-ANIO          PIC 9(1).
       01 WS-IND-SUC           PIC 9(1).
       01 WS-TIM-ANIO          PIC 9(4).
       01 WS-TIM-SUC           PIC X(03).

       01 FECHA-ACTUAL.
           03  FECHA-ACTUAL-AAAA      PIC 9(4).
           03  FECHA-ACTUAL-MM        PIC 9(2).
           03  FECHA-ACTUAL-DD        PIC 9(2).

       01 VEC-SUCURSALES.
           03 VEC-SUCURSALES-ELM
               OCCURS CON-CANT-SUC TIMES
               INDEXED BY INDICE.
               05  VEC-SUCURSALES-SUCURSAL        PIC X(03).
               05  VEC-SUCURSALES-RAZON           PIC X(25).

       01 VEC-ANIOS.
           03 VEC-ANIOS-ELEM
              OCCURS CON-CANT-ANIOS TIMES PIC 9(4).

       01 VEC-TOT-MENSUAL.
           03 VEC-TOT-MENSUAL-ELM
              OCCURS CON-TOT-MENSUAL TIMES PIC 9(4).

       01 ENCABEZADO1.
           03 FILLER           PIC X(5)    VALUE "Fecha".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-FECHA-DD     PIC 9(2).
           03 FILLER           PIC X(1)    VALUE "/".
           03 ENC-FECHA-MM     PIC 9(2).
           03 FILLER           PIC X(1)    VALUE "/".
           03 ENC-FECHA-AAAA   PIC 9(4).
           03 FILLER           PIC X(56)   VALUE SPACES.
           03 FILLER           PIC X(4)    VALUE "Hoja".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-HOJA         PIC 9(3).

       01 ENCABEZADO2.
           03 FILLER           PIC X(10)    VALUE SPACES.
           03 ENC-TITULO       PIC X(56)    VALUE
           "Listado de Estadistico de Horas aplicadas por anio y mes".
           03 FILLER           PIC X(10)    VALUE SPACES.

       01 ENCABEZADO3          PIC X(80)    VALUE ALL SPACES.

       01 LINEA-DETALLES.
           03 FILLER           PIC X(80)   VALUE ALL "-".

       01 ENCABEZAD-DETALLES.
           03 FILLER           PIC X(19)   VALUE "Sucursal".
           03 FILLER           PIC X(2)    VALUE SPACES.
           03 ENC-ANIO         PIC X(4)    VALUE SPACES.
           03 FILLER           PIC X(2)    VALUE SPACES.
           03 ENC-ENE          PIC X(3)    VALUE "Ene".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-FEB          PIC X(3)    VALUE "Feb".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-MAR          PIC X(3)    VALUE "Mar".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-ABR          PIC X(3)    VALUE "Abr".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-MAY          PIC X(3)    VALUE "May".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-JUN          PIC X(3)    VALUE "Jun".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-JUL          PIC X(3)    VALUE "Jul".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-AGO          PIC X(3)    VALUE "Ago".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-SEP          PIC X(3)    VALUE "Sep".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-OCT          PIC X(3)    VALUE "Oct".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-NOV          PIC X(3)    VALUE "Nov".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-DIC          PIC X(3)    VALUE "Dic".
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 ENC-TOTAL        PIC X(5)    VALUE "Total".

       01 FILA-DETALLES.
           03 DET-SUCURSAL     PIC X(19).
           03 FILLER           PIC X(2)    VALUE SPACES.
           03 DET-ANIO         PIC 9(4).
           03 FILLER           PIC X(2)    VALUE SPACES.
           03 DET-ENE          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-FEB          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-MAR          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-ABR          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-MAY          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-JUN          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-JUL          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-AGO          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-SEP          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-OCT          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-NOV          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-DIC          PIC 9(3).
           03 FILLER           PIC X(1)    VALUE SPACES.
           03 DET-TOTAL        PIC 9(4).

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

      *- INICIO LLAMADO A PROCEDIMIENTOS

           PERFORM INICIALIZAR.
           PERFORM ABRIR-ARCHIVOS.

           PERFORM LEER-SUCURSALES.
           PERFORM CARGAR-SUCURSALES.

           PERFORM IMPRIMIR-ENCABEZADO-1.
           PERFORM IMPRIMIR-ENCABEZADO-2.
           PERFORM IMPRIMIR-ENCABEZADO-3.
           PERFORM IMPRIMIR-ENCABEZADO-DETALLES.

           PERFORM GENERAR-ANIOS.
           PERFORM LEER-TIMES.

           PERFORM PROCESO1 UNTIL FS-TIMES = CON-EOF.
           PERFORM ESCRIBIR-ARCHIVO.

           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

      *- FIN LLAMADO A PROCEDIMIENTOS

       INICIALIZAR.
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
                  UNTIL INDICE > CON-CANT-SUC
                  OR FS-SUCURSALES IS EQUAL TO 10.

       GUARDAR-SUCURSAL.
           MOVE SUC-SUCURSAL TO VEC-SUCURSALES-SUCURSAL(INDICE).
           MOVE SUC-RAZON TO VEC-SUCURSALES-RAZON(INDICE).

           PERFORM LEER-SUCURSALES.

       IMPRIMIR-ENCABEZADO-1.
           MOVE FUNCTION CURRENT-DATE TO FECHA-ACTUAL.

           MOVE FECHA-ACTUAL-AAAA TO ENC-FECHA-AAAA.
           MOVE FECHA-ACTUAL-MM TO ENC-FECHA-MM.
           MOVE FECHA-ACTUAL-DD TO ENC-FECHA-DD.
           MOVE WS-HOJA TO ENC-HOJA.

           DISPLAY ENCABEZADO1.

       IMPRIMIR-ENCABEZADO-2.
           DISPLAY ENCABEZADO2.

       IMPRIMIR-ENCABEZADO-3.
           DISPLAY ENCABEZADO3.

       IMPRIMIR-ENCABEZADO-DETALLES.
           DISPLAY LINEA-DETALLES.
           DISPLAY ENCABEZAD-DETALLES.
           DISPLAY LINEA-DETALLES.

       GENERAR-ANIOS.
           PERFORM CARGAR-ANIO UNTIL (WS-I > CON-CANT-SUC).

       CARGAR-ANIO.
           SUBTRACT WS-J FROM FECHA-ACTUAL-AAAA.
           MOVE WS-J TO VEC-ANIOS-ELEM(WS-I).

           ADD 1 TO WS-I.
           SUBTRACT 1 FROM WS-J.

       LEER-TIMES.
           READ TIMES_FILE.
           IF FS-TIMES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER TIMES FS: " FS-TIMES
           END-IF.

       PROCESO1.
           PERFORM GUARDAR-ANIO-REGISTRO.
           PERFORM BUSCAR-ANIO.

           PERFORM GUARDAR-SUC-REGISTRO.
           PERFORM BUSCAR-SUCURSAL.

           IF (VEC-ANIOS-ELEM(WS-IND-ANIO) = WS-TIM-ANIO)
                  AND (VEC-SUCURSALES-ELM(WS-IND-SUC) = WS-TIM-SUC)

               PERFORM GUARDAR-VALORES

           END-IF.

           PERFORM LEER-TIMES.

       GUARDAR-ANIO-REGISTRO.
           MOVE TIM-FECHA(1:4) TO WS-TIM-ANIO.

           DISPLAY WS-TIM-ANIO.

       GUARDAR-SUC-REGISTRO.
           MOVE TIM-SUCURSAL TO WS-TIM-SUC.

           DISPLAY WS-TIM-SUC.

       BUSCAR-ANIO.
           DISPLAY "BUSCAR ANIO".

           MOVE 1 TO WS-IND-ANIO.

           PERFORM INC-IND-ANIO
           UNTIL (WS-IND-ANIO > CON-CANT-ANIOS
                  AND VEC-ANIOS-ELEM(WS-IND-ANIO) = WS-TIM-ANIO).

       BUSCAR-SUCURSAL.
           DISPLAY "BUSCAR SUCURSAL".

           MOVE 1 TO WS-IND-SUC.

           PERFORM INC-IND-SUC
           UNTIL (WS-IND-SUC > CON-CANT-SUC
                  AND VEC-SUCURSALES-ELM(WS-IND-SUC) = WS-TIM-SUC).

       INC-IND-ANIO.
           ADD 1 TO WS-IND-ANIO.

       INC-IND-SUC.
           ADD 1 TO WS-IND-SUC.

       GUARDAR-VALORES.
           DISPLAY "Guardar valores en la matriz".

       IMPRIMIR-FILA-DETALLES.
           MOVE "San Jose" TO DET-SUCURSAL.
           MOVE "2014" TO DET-ANIO.
           MOVE "123" TO DET-ENE.
           MOVE "456" TO DET-FEB.
           MOVE "789" TO DET-MAR.

           DISPLAY FILA-DETALLES.

       ESCRIBIR-ARCHIVO.
      *    DISPLAY "Escribir en Estadisticas".

       CERRAR-ARCHIVOS.
           CLOSE SUCURSALES_FILE.
           CLOSE TIMES_FILE.

       END PROGRAM "TP_PARTE_1B".
