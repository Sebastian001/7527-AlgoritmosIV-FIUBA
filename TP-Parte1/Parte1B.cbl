       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. "TP_PARTE_1B".
       AUTHOR. "Adrian Mouly - Sebastian Torres".
       DATE-WRITTEN. "2do cuatrimestre 2015".

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT SUCURSALES-FILE
           ASSIGN TO "../files/in/Sucursales.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSALES.

           SELECT TIMES-FILE
           ASSIGN TO "../files/out/Times.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TIMES.

           SELECT ESTADISTICAS-FILE
           ASSIGN TO "../files/out/Estadisticas.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ESTADISTICAS.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------

      *-------------------------------*
      *- SUCURSALES FILE DESCRIPTION -*
      *-------------------------------*
       FD SUCURSALES-FILE LABEL RECORD STANDARD.
       01 REG-SUCURSALES.
           03 SUC-SUCURSAL      PIC X(03).
           03 SUC-RAZON         PIC X(25).
           03 SUC-DIRE          PIC X(20).
           03 SUC-TEL           PIC X(20).
           03 SUC-CUIT          PIC 9(11).

      *--------------------------*
      *- TIMES FILE DESCRIPTION -*
      *--------------------------*
       FD TIMES-FILE LABEL RECORD STANDARD.
       01 REG-TIMES.
           03 CLAVE-TIMES.
              05 CLAVE-TIMES-SUC.
                  07 CLAVE-TIMES-FECHA.
                      09 TIM-NUMERO        PIC X(5).
                      09 TIM-FECHA         PIC 9(8).
                  07 TIM-SUCURSAL          PIC X(03).
           03 TIM-TIPCLASE                 PIC X(04).
           03 TIM-HORAS                    PIC 9(2)V99.

      *---------------------------------*
      *- ESTADISTICAS FILE DESCRIPTION -*
      *---------------------------------*
       FD ESTADISTICAS-FILE LABEL RECORD OMITTED.
       01 REG-ESTADISTICAS                 PIC X(80).

       WORKING-STORAGE SECTION.
      *-----------------------

       77 FS-TIMES             PIC X(2).
       77 FS-SUCURSALES        PIC X(2).
       77 FS-ESTADISTICAS      PIC X(2).

       78 CON-EOF                          VALUE 10.
       78 CON-CANT-ANIOS                   VALUE 5.
       78 CON-CANT-SUC                     VALUE 3.
       78 CON-CANT-MESES                   VALUE 12.
       78 CON-TOT-MENSUAL                  VALUE 12.

       01 WS-TOT-GRAL          PIC 9999999999V99.
       01 WS-ANIO-ACTUAL       PIC 9(4).
       01 WS-HOJA              PIC 9(3)    VALUE 001.
       01 WS-I                 PIC 9(1).
       01 WS-J                 PIC 9(1).
       01 WS-I2                PIC 9(1).
       01 WS-J2                PIC 9(1).
       01 WS-K                 PIC 9(2).
       01 WS-IND-ANIO          PIC 9(1).
       01 WS-IND-SUC           PIC 9(1).
       01 WS-TIM-ANIO          PIC 9(4).
       01 WS-TIM-MES           PIC 9(2).
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
              OCCURS CON-CANT-ANIOS TIMES
              INDEXED BY INDICE2                  PIC 9(4).

       01 VEC-TOT-MENSUAL.
           03 VEC-TOT-MENSUAL-ELM
              OCCURS CON-TOT-MENSUAL TIMES        PIC 9(4).

       01 MAT-DATOS.
           03 MAT-DATOS-SUC OCCURS CON-CANT-SUC TIMES.
              05 MAT-DATOS-ANIO OCCURS CON-CANT-ANIOS TIMES.
                 07 MAT-DATOS-MES OCCURS CON-TOT-MENSUAL TIMES.
                    09 MAT-DATOS-HORAS            PIC 9(2)V99.

       01 MAT-TOT-SUC.
           03 MAT-TOT-SUC-SUC OCCURS CON-CANT-SUC TIMES.
              05 MAT-TOT-SUC-ANIO OCCURS CON-CANT-ANIOS TIMES.
                 07 MAT-TOT-SUC-HORAS             PIC 9(2)V99.

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
           03 FILLER           PIC X(10)   VALUE SPACES.
           03 ENC-TITULO       PIC X(56)   VALUE
           "Listado de Estadistico de Horas aplicadas por anio y mes".
           03 FILLER           PIC X(10)   VALUE SPACES.

       01 ENCABEZADO3          PIC X(80)   VALUE ALL SPACES.

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

           PERFORM CARGAR-FECHA.

           PERFORM GENERAR-ANIOS.
           PERFORM LEER-TIMES.

           PERFORM PROCESO1 UNTIL FS-TIMES = CON-EOF.

           PERFORM IMPRIMIR-ENCABEZADO-1.
           PERFORM IMPRIMIR-ENCABEZADO-2.
           PERFORM IMPRIMIR-ENCABEZADO-DETALLES.
           PERFORM ESCRIBIR-ARCHIVO.
           PERFORM IMPRIMIR-TOTALES.

           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

      *- FIN LLAMADO A PROCEDIMIENTOS

       INICIALIZAR.
           MOVE 0 TO WS-TOT-GRAL.

       ABRIR-ARCHIVOS.
           OPEN INPUT SUCURSALES-FILE.
           IF FS-SUCURSALES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR SUCURSALES FS: " FS-SUCURSALES
               STOP RUN
           END-IF.

           OPEN INPUT TIMES-FILE.
           IF FS-TIMES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIMES
               STOP RUN
           END-IF.

           OPEN OUTPUT ESTADISTICAS-FILE.
           IF FS-ESTADISTICAS IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR ESTADISTICAS FS: "
                       FS-ESTADISTICAS
               STOP RUN
           END-IF.

       LEER-SUCURSALES.
           READ SUCURSALES-FILE.
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

       CARGAR-FECHA.
           MOVE FUNCTION CURRENT-DATE TO FECHA-ACTUAL.

           MOVE FECHA-ACTUAL-AAAA TO ENC-FECHA-AAAA.
           MOVE FECHA-ACTUAL-MM TO ENC-FECHA-MM.
           MOVE FECHA-ACTUAL-DD TO ENC-FECHA-DD.

       IMPRIMIR-ENCABEZADO-1.
           MOVE WS-HOJA TO ENC-HOJA.

           DISPLAY LINEA-DETALLES.
           DISPLAY ENCABEZADO1.

           WRITE REG-ESTADISTICAS FROM LINEA-DETALLES.
           WRITE REG-ESTADISTICAS FROM ENCABEZADO1.

       IMPRIMIR-ENCABEZADO-2.
           DISPLAY ENCABEZADO3.
           DISPLAY ENCABEZADO2.
           DISPLAY ENCABEZADO3.

           WRITE REG-ESTADISTICAS FROM ENCABEZADO3.
           WRITE REG-ESTADISTICAS FROM ENCABEZADO2.
           WRITE REG-ESTADISTICAS FROM ENCABEZADO3.

       IMPRIMIR-ENCABEZADO-DETALLES.
           DISPLAY LINEA-DETALLES.
           DISPLAY ENCABEZAD-DETALLES.
           DISPLAY LINEA-DETALLES.

           WRITE REG-ESTADISTICAS FROM LINEA-DETALLES.
           WRITE REG-ESTADISTICAS FROM ENCABEZAD-DETALLES.
           WRITE REG-ESTADISTICAS FROM LINEA-DETALLES.

       GENERAR-ANIOS.
           MOVE 1 TO WS-I.
           MOVE 4 TO WS-J.

           PERFORM CARGAR-ANIO UNTIL (WS-I > CON-CANT-ANIOS).

       CARGAR-ANIO.
           MOVE FECHA-ACTUAL-AAAA TO WS-ANIO-ACTUAL.

           SUBTRACT WS-J FROM WS-ANIO-ACTUAL.
           MOVE WS-ANIO-ACTUAL TO VEC-ANIOS-ELEM(WS-I).

           ADD 1 TO WS-I.
           SUBTRACT 1 FROM WS-J.

       LEER-TIMES.
           READ TIMES-FILE.
           IF FS-TIMES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER TIMES FS: " FS-TIMES
           END-IF.

       PROCESO1.
           PERFORM GUARDAR-ANIO-REGISTRO.
           PERFORM BUSCAR-ANIO.

           PERFORM GUARDAR-SUC-REGISTRO.
           PERFORM BUSCAR-SUCURSAL.

           PERFORM GUARDAR-MES-REGISTRO.

           IF (VEC-ANIOS-ELEM(WS-IND-ANIO) = WS-TIM-ANIO)
               AND (VEC-SUCURSALES-SUCURSAL(WS-IND-SUC) = WS-TIM-SUC)
               PERFORM GUARDAR-VALORES
           ELSE
               PERFORM NO-RESULTADOS
           END-IF.

           PERFORM LEER-TIMES.

       GUARDAR-ANIO-REGISTRO.
           MOVE TIM-FECHA(1:4) TO WS-TIM-ANIO.

       GUARDAR-MES-REGISTRO.
           MOVE TIM-FECHA(5:2) TO WS-TIM-MES.

       GUARDAR-SUC-REGISTRO.
           MOVE TIM-SUCURSAL TO WS-TIM-SUC.

       BUSCAR-ANIO.
           DISPLAY "[ Buscar anio - " WS-TIM-ANIO " ]".

           SET INDICE2 TO 1.
           SEARCH VEC-ANIOS-ELEM
           AT END PERFORM ANIO-NO-ENCONTRADO
           WHEN VEC-ANIOS-ELEM(INDICE2) IS EQUAL TO WS-TIM-ANIO

               DISPLAY "- Anio encontrado"

               MOVE INDICE2 TO WS-IND-ANIO
           END-SEARCH.

       BUSCAR-SUCURSAL.
           DISPLAY "[ Buscar sucursal: " WS-TIM-SUC " ]".

           SET INDICE TO 1.
           SEARCH VEC-SUCURSALES-ELM
           AT END PERFORM SUCURSAL-NO-ENCONTRADA
           WHEN VEC-SUCURSALES-SUCURSAL(INDICE) IS EQUAL TO WS-TIM-SUC

               DISPLAY "- Sucursal encontrada"

               MOVE INDICE TO WS-IND-SUC
           END-SEARCH.

       GUARDAR-VALORES.
           DISPLAY "[ Guardar valores en la matriz ]".

           ADD TIM-HORAS
           TO MAT-DATOS-HORAS(WS-IND-SUC, WS-IND-SUC, WS-TIM-MES).

           ADD TIM-HORAS TO VEC-TOT-MENSUAL-ELM(WS-TIM-MES).

           ADD VEC-TOT-MENSUAL-ELM(WS-TIM-MES)
           TO MAT-TOT-SUC-HORAS(WS-IND-SUC, WS-IND-ANIO).

           ADD VEC-TOT-MENSUAL-ELM(WS-TIM-MES) TO WS-TOT-GRAL.

       SUCURSAL-NO-ENCONTRADA.
           DISPLAY "- Sucursal no enconetrada".

       ANIO-NO-ENCONTRADO.
           DISPLAY "- Anio no encontrado".

       NO-RESULTADOS.
           DISPLAY "-- No hay resultados".

       ESCRIBIR-ARCHIVO.
           MOVE 1 TO WS-I2.

           PERFORM IMPRIMIR-FILAS-SUCURSAL UNTIL (WS-I2 > CON-CANT-SUC).

       IMPRIMIR-FILAS-SUCURSAL.
           MOVE VEC-SUCURSALES-RAZON(WS-I2) TO DET-SUCURSAL.

           MOVE 1 TO WS-J2.
           PERFORM IMRPIMIR-COLUMNAS-SUCURSAL
                  UNTIL (WS-J2 > CON-CANT-ANIOS).

           ADD 1 TO WS-I2.

       IMRPIMIR-COLUMNAS-SUCURSAL.
           *> Popular tabla de detalles
           MOVE VEC-ANIOS-ELEM(WS-I2) TO DET-ANIO.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 1)  TO DET-ENE.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 2)  TO DET-FEB.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 3)  TO DET-MAR.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 4)  TO DET-ABR.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 5)  TO DET-MAY.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 6)  TO DET-JUN.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 7)  TO DET-JUL.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 8)  TO DET-AGO.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 9)  TO DET-SEP.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 10) TO DET-OCT.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 11) TO DET-NOV.
           MOVE MAT-DATOS-HORAS(WS-I2, WS-J2, 12) TO DET-DIC.
           MOVE MAT-TOT-SUC-HORAS(WS-I2, WS-J2)   TO DET-TOTAL.

           *> Mostrar fila por pantalla
           DISPLAY FILA-DETALLES.

           *> Imprimir fila en archivo
           WRITE REG-ESTADISTICAS FROM FILA-DETALLES.

           ADD 1 TO WS-J2.

       IMPRIMIR-TOTALES.
           *> Popular tabla de detalles
           MOVE "TOTALES" TO DET-SUCURSAL.
           MOVE "    " TO DET-ANIO.
           MOVE VEC-TOT-MENSUAL-ELM(1)  TO DET-ENE.
           MOVE VEC-TOT-MENSUAL-ELM(2)  TO DET-FEB.
           MOVE VEC-TOT-MENSUAL-ELM(3)  TO DET-MAR.
           MOVE VEC-TOT-MENSUAL-ELM(4)  TO DET-ABR.
           MOVE VEC-TOT-MENSUAL-ELM(5)  TO DET-MAY.
           MOVE VEC-TOT-MENSUAL-ELM(6)  TO DET-JUN.
           MOVE VEC-TOT-MENSUAL-ELM(7)  TO DET-JUL.
           MOVE VEC-TOT-MENSUAL-ELM(8)  TO DET-AGO.
           MOVE VEC-TOT-MENSUAL-ELM(9)  TO DET-SEP.
           MOVE VEC-TOT-MENSUAL-ELM(10) TO DET-OCT.
           MOVE VEC-TOT-MENSUAL-ELM(11) TO DET-NOV.
           MOVE VEC-TOT-MENSUAL-ELM(12) TO DET-DIC.
           MOVE WS-TOT-GRAL TO DET-TOTAL.

           *> Mostrar tabla en pantalla
           DISPLAY LINEA-DETALLES.
           DISPLAY FILA-DETALLES.
           DISPLAY LINEA-DETALLES.

           *> Imprimir tabla en archivo
           WRITE REG-ESTADISTICAS FROM LINEA-DETALLES.
           WRITE REG-ESTADISTICAS FROM FILA-DETALLES.
           WRITE REG-ESTADISTICAS FROM LINEA-DETALLES.

       CERRAR-ARCHIVOS.
           CLOSE SUCURSALES-FILE.
           CLOSE TIMES-FILE.
           CLOSE ESTADISTICAS-FILE.

       END PROGRAM "TP_PARTE_1B".
