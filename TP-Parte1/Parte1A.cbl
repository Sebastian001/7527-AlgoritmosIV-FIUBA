       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. "TP_PARTE_1A".
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

           SELECT LISTADO_FILE
           ASSIGN TO "../files/out/Listado.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-LISTADO.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.

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

      *-------------------------------*
      *- PROFESORES FILE DESCRIPTION -*
      *-------------------------------*
       FD PROFESORES_FILE LABEL RECORD STANDARD.
       01 REG-PROFESORES.
              03 CLAVE-PROF.
                   05 PROF-NUMERO       PIC X(5).
              03 PROF-DNI               PIC 9(8).
              03 PROF-NOMBRE            PIC X(25).
              03 PROF-DIRE              PIC X(20).
              03 PROF-TEL               PIC X(20).

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
              03 TIP-CLASE         PIC X(04).
              03 TIP-DESC          PIC X(20).
              03 TIP-TARIFA        PIC 9(5)V99.

      *------------------------------*
      *- NOVTIMES1 FILE DESCRIPTION -*
      *------------------------------*
       FD NOVTIMES1_FILE LABEL RECORD STANDARD.
       01 REG-NOVTIMES1.
           03 CLAVE-NOV1.
               05 CLAVE-SUC1.
                   07 CLAVE-FECHA1.
                       09 NOV1-NUMERO        PIC X(5).
                       09 NOV1-FECHA         PIC 9(8).
                   07 NOV1-SUCURSAL          PIC X(03).
           03 NOV1-TIPCLASE                  PIC X(04).
           03 NOV1-HORAS                     PIC 9(2)V99.

      *------------------------------*
      *- NOVTIMES2 FILE DESCRIPTION -*
      *------------------------------*
       FD NOVTIMES2_FILE LABEL RECORD STANDARD.
       01 REG-NOVTIMES2.
           03 CLAVE-NOV2.
               05 CLAVE-SUC2.
                   07 CLAVE-FECHA2.
                       09 NOV2-NUMERO        PIC X(5).
                       09 NOV2-FECHA         PIC 9(8).
                   07 NOV2-SUCURSAL          PIC X(03).
           03 NOV2-TIPCLASE                  PIC X(04).
           03 NOV2-HORAS                     PIC 9(2)V99.

      *------------------------------*
      *- NOVTIMES3 FILE DESCRIPTION -*
      *------------------------------*
       FD NOVTIMES3_FILE LABEL RECORD STANDARD.
       01 REG-NOVTIMES3.
           03 CLAVE-NOV3.
               05 CLAVE-SUC3.
                   07 CLAVE-FECHA3.
                       09 NOV3-NUMERO        PIC X(5).
                       09 NOV3-FECHA         PIC 9(8).
                   07 NOV3-SUCURSAL          PIC X(03).
           03 NOV3-TIPCLASE                  PIC X(04).
           03 NOV3-HORAS                     PIC 9(2)V99.

      *----------------------------------*
      *- LISTADO FINAL FILE DESCRIPTION -*
      *----------------------------------*
       FD LISTADO_FILE LABEL RECORD OMITTED.
       01 REG-LISTADO PIC X(80).
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
       77 FS-LISTADO        PIC X(2).

       01 FECHA-DE-HOY.
           03  FECHA-AAAA      pic 9(4).
           03  FECHA-MM        pic 9(2).
           03  FECHA-DD        pic 9(2).

       01 ENCABEZADO1.
           03  FILLER      PIC X(6)    VALUE "Fecha ".
           03  FECHA-DD    PIC 9(2).
           03  FILLER      PIC X       VALUE "/".
           03  FECHA-MM    PIC 9(2).
           03  FILLER      PIC X       VALUE "/".
           03  FECHA-AAAA  PIC 9(4).
           03  FILLER      PIC X(45)   VALUE SPACES.
           03  FILLER      PIC X(5)    VALUE "Hoja ".
           03  E1-HOJA     PIC 9(3).

       01 ENCABEZADO2.
           03 FILLER PIC x(16) VALUE SPACES.
           03 FILLER PIC X(38) VALUE "Listado de horas aplicadas".
           03 FILLER PIC x(26) VALUE SPACES.

       01 LINEA-EN-BLANCO.
           03 FILLER PIC X(80) VALUE SPACES.

       01 ENCABEZADO3-PROF.
           03  FILLER      PIC X(10)   VALUE "Profesor: ".
           03  E3-PROF-NUM PIC X(5).
           03  FILLER      PIC X(5)    VALUE SPACES.
           03  FILLER      PIC X(8)    VALUE "Nombre: ".
           03  E3-PROF-NOM PIC X(25).
           03  FILLER      PIC X(27)   VALUE SPACES.

       01 ENCABEZADO4-TABLA.
           03 FILLER       PIC X(10)   VALUE "   Fecha  ".
           03 FILLER       PIC X(10)   VALUE " Sucursal ".
           03 FILLER       PIC X(22)   VALUE " Tipo de clase       ".
           03 FILLER       PIC X(10)   VALUE "    Tarifa".
           03 FILLER       PIC X(10)   VALUE "  Horas   ".
           03 FILLER       PIC X(20)   VALUE "       Importe ".

       01 LINEA-TABLA.
           03 FILLER       PIC X(80)   VALUE ALL "_".

       01 LINEA-SUBTOTAL.
           03 FILLER       PIC X(50)   VALUE ALL " ".
           03 FILLER       PIC X(10)   VALUE "   ------ ".
           03 FILLER       PIC X(20)   VALUE "      -----------  ".

       01 DATOS-TABLA.
           03 DT-FECHA-DD     PIC 9(2).
           03 FILLER          PIC X       VALUE "/".
           03 DT-FECHA-MM     PIC 9(2).
           03 FILLER          PIC X       VALUE "/".
           03 DT-FECHA-AAAA   PIC 9(4).
           03 FILLER          PIC X(3)    VALUE ALL " ".
           03 DT-SUC          PIC X(3).
           03 FILLER          PIC X(6)    VALUE ALL " ".
           03 DT-TIPO         PIC X(20).
           03 FILLER          PIC X       VALUE " ".
           03 DT-TARIFA       PIC ZZZZ9,99.
           03 FILLER          PIC X(3)    VALUE ALL " ".
           03 DT-HORAS        PIC Z9,99.
           03 FILLER          PIC X(8)    VALUE ALL " ".
           03 DT-IMPORTE      PIC ZZZZZZ9,99.
           03 FILLER          PIC X(3)    VALUE ALL " ".

       01 FECHA-DATO.
           03  FECHAD-AAAA      pic 9(4).
           03  FECHAD-MM        pic 9(2).
           03  FECHAD-DD        pic 9(2).

       01 ENCABEZADO5-SUBTOT-FECHA.
           03 FILLER           PIC X(17)    VALUE "Totales por fecha".
           03 FILLER           PIC X(36)    VALUE ALL " ".
           03 E5-TOT-HORAS     PIC ZZ9,99.
           03 FILLER           PIC X(7)     VALUE ALL " ".
           03 E5-TOT-IMPORTE   PIC ZZZZZZZ9,99.
           03 FILLER           PIC X(2)     VALUE ALL " ".

       01 ENCABEZADO6-SUBTOT-PROFESOR.
           03 FILLER          PIC X(20)    VALUE "Totales por Profesor".
           03 FILLER          PIC X(32)    VALUE ALL " ".
           03 E6-TOT-HORAS    PIC ZZZ9,99.
           03 FILLER          PIC X(6)     VALUE ALL " ".
           03 E6-TOT-IMPORTE  PIC ZZZZZZZZ9,99.
           03 FILLER          PIC X(2)     VALUE ALL " ".

       01 ENCABEZADO7-TOT-GENERAL.
           03 FILLER          PIC X(13)    VALUE "Total general".
           03 FILLER          PIC X(51)    VALUE ALL " ".
           03 E7-TOT-IMPORTE  PIC ZZZZZZZZZ9,99.
           03 FILLER          PIC X        VALUE " ".

       01 CLAVE-MENOR.
           03 CLAVE-MENOR-SUC.
               05 CLAVE-MENOR-FECHA.
                   07 MENOR-NUMERO        PIC X(5).
                   07 MENOR-FECHA         PIC 9(8).
               05 MENOR-SUCURSAL          PIC X(03).

       01 REG-NOVTIMES.
           03 CLAVE-NOV.
               05 CLAVE-SUC.
                   07 CLAVE-FECHA.
                       09 NOV-NUMERO        PIC X(5).
                       09 NOV-FECHA         PIC 9(8).
                   07 NOV-SUCURSAL          PIC X(03).
           03 NOV-TIPCLASE                  PIC X(04).
           03 NOV-HORAS                     PIC 9(2)V99.

       01 VEC.
           03 VEC-TIPOSCLASE
               OCCURS 50 TIMES
               INDEXED BY INDICE.
               05  VEC-TIPOSCLASE-TIPO        PIC X(04).
               05  VEC-TIPOSCLASE-DESC        PIC X(20).
               05  VEC-TIPOSCLASE-TARIFA      PIC 9(5)V99.

       77 LINEA           PIC 99.
       77 HOJA            PIC 999.
       77 TOT-GRAL        PIC 9999999999V99.
       77 TOT-IMP-PROF    PIC 99999999V99.
       77 TOT-HORAS-PROF  PIC 999V99.
       77 TOT-IMP-FECHA   PIC 9999999V99.
       77 TOT-HORAS-FECHA PIC 99V99.
       77 IMPORTE         PIC 9999999V99.
       77 HORAS           PIC 99V99.
       77 DESCRIPCION     PIC X(20).
       77 TARIFA          PIC 9(5)V99.
       77 RESTO-LINEAS    PIC 99.
       77 I               PIC 99.

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      *- INICIO LLAMADO A PROCEDIMIENTOS

           PERFORM INICIALIZAR.
           PERFORM ABRIR-ARCHIVOS.

           PERFORM LEER-TIPOSCLASE.
           PERFORM CARGAR-TIPOSCLASE.

           PERFORM LEER-NOVTIMES1.
           PERFORM LEER-NOVTIMES2.
           PERFORM LEER-NOVTIMES3.
           PERFORM LEER-PROFESORES.

           PERFORM PROCESO1 UNTIL FS-NOVTIMES1 = 10
               AND FS-NOVTIMES2 = 10 AND FS-NOVTIMES3 = 10
               AND FS-PROFESORES = 10.

           PERFORM PRINT-ENCABEZADO.
           PERFORM PRINT-TOTAL-GRAL.

           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

      *- FIN LLAMADO A PROCEDIMIENTOS

       INICIALIZAR.
           DISPLAY "Inicializar Variables".
           MOVE 0 TO LINEA.
           MOVE 1 TO HOJA.
           MOVE 0 TO TOT-GRAL.

       PRINT-ENCABEZADO.
           MOVE FUNCTION CURRENT-DATE TO FECHA-DE-HOY.
           MOVE CORRESPONDING FECHA-DE-HOY TO ENCABEZADO1.
           DISPLAY ENCABEZADO1.
           DISPLAY ENCABEZADO2.
           MOVE HOJA TO E1-HOJA.
           WRITE REG-LISTADO FROM ENCABEZADO1.
           WRITE REG-LISTADO FROM ENCABEZADO2.
           WRITE REG-LISTADO FROM LINEA-EN-BLANCO.
           ADD 3 TO LINEA.

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

           OPEN OUTPUT TIMES_FILE.
           IF FS-TIMES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIMES
               STOP RUN
           END-IF.

           OPEN OUTPUT LISTADO_FILE.
           IF FS-LISTADO IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR LISTADO FS: " FS-LISTADO
               STOP RUN
           END-IF.

       LEER-SUCURSALES.
           READ SUCURSALES_FILE.
           IF FS-SUCURSALES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER SUCURSALES FS: " FS-SUCURSALES
           END-IF.

       LEER-NOVTIMES1.
           READ NOVTIMES1_FILE RECORD AT END MOVE HIGH-VALUE TO
           CLAVE-NOV1.
           IF FS-NOVTIMES1 IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER NOVTIMES1 FS: " FS-NOVTIMES1
           END-IF.

       LEER-NOVTIMES2.
           READ NOVTIMES2_FILE RECORD AT END MOVE HIGH-VALUE TO
           CLAVE-NOV2.
           IF FS-NOVTIMES2 IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER NOVTIMES2 FS: " FS-NOVTIMES2
           END-IF.

       LEER-NOVTIMES3.
           READ NOVTIMES3_FILE RECORD AT END MOVE HIGH-VALUE TO
           CLAVE-NOV3.
           IF FS-NOVTIMES3 IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER NOVTIMES3 FS: " FS-NOVTIMES3
           END-IF.

       LEER-PROFESORES.
           READ PROFESORES_FILE.
           IF FS-PROFESORES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER PROFESORES FS: " FS-PROFESORES
           END-IF.

       LEER-TIPOSCLASE.
           READ TIPOSCLASE_FILE.
           IF FS-TIPOSCLASE IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER TIPOS-CLASE FS: " FS-TIPOSCLASE
           END-IF.

       CARGAR-TIPOSCLASE.
           PERFORM GUARDAR-TIPOCLASE
                  VARYING INDICE FROM 1 BY 1
                  UNTIL INDICE > 50
                  OR FS-TIPOSCLASE IS EQUAL TO 10.

       GUARDAR-TIPOCLASE.
           DISPLAY "Guardar TC: " TIP-CLASE " Desc: " TIP-DESC.

           MOVE TIP-CLASE   TO VEC-TIPOSCLASE-TIPO(INDICE).
           MOVE TIP-DESC    TO VEC-TIPOSCLASE-DESC(INDICE).
           MOVE TIP-TARIFA  TO VEC-TIPOSCLASE-TARIFA(INDICE).

           PERFORM LEER-TIPOSCLASE.

       DETERMINAR-CLAVE-MENOR.
           DISPLAY "Determinar clave Menor".
           MOVE CLAVE-NOV1 TO CLAVE-MENOR.
           IF CLAVE-SUC2 < CLAVE-MENOR
               MOVE CLAVE-SUC2 TO CLAVE-MENOR
           END-IF.
           IF CLAVE-SUC3 < CLAVE-MENOR
               MOVE CLAVE-SUC3 TO CLAVE-MENOR
           END-IF.
           DISPLAY "CLAVE MENOR PROFESOR " MENOR-NUMERO.

       PRINT-ENCABEZADO-PROF.
           DISPLAY "Imprimir encabezado profesor".
           PERFORM PRINT-ENCABEZADO.
           PERFORM LEER-PROFESORES UNTIL FS-PROFESORES = 10
               OR MENOR-NUMERO <= CLAVE-PROF.
           IF MENOR-NUMERO = CLAVE-PROF
               PERFORM PRINT-DATOS-PROF
           ELSE
               WRITE REG-LISTADO FROM "PROFESOR INEXISTENTE"
               WRITE REG-LISTADO FROM MENOR-NUMERO
               DISPLAY "PROFESOR " MENOR-NUMERO " NO ENCONTRADO"
           END-IF.

       PRINT-DATOS-PROF.
           DISPLAY "Imprimir datos profesor".
           DISPLAY PROF-NOMBRE.
           DISPLAY PROF-DNI.
           MOVE PROF-NUMERO TO E3-PROF-NUM.
           MOVE PROF-NOMBRE TO E3-PROF-NOM.
           WRITE REG-LISTADO FROM ENCABEZADO3-PROF.
           ADD 1 TO LINEA.

       PRINT-ENCABEZADO-TABLA.
           DISPLAY "Fecha   Sucursal   Tipo  Tarifa   Horas   Importe".
           WRITE REG-LISTADO FROM ENCABEZADO4-TABLA.
           WRITE REG-LISTADO FROM LINEA-TABLA.
           ADD 2 TO LINEA.

       PROCESO1.
           DISPLAY "Ejecutar Proceso1".

           PERFORM DETERMINAR-CLAVE-MENOR.
           MOVE 0 TO TOT-IMP-PROF.
           MOVE 0 TO TOT-HORAS-PROF.
           PERFORM PRINT-ENCABEZADO-PROF.
           PERFORM PROCESO2 UNTIL (FS-NOVTIMES1 = 10
               AND FS-NOVTIMES2 = 10 AND FS-NOVTIMES3 = 10
               AND FS-PROFESORES = 10) OR (MENOR-NUMERO <> NOV1-NUMERO
               AND MENOR-NUMERO <> NOV2-NUMERO
               AND MENOR-NUMERO <> NOV3-NUMERO).
           PERFORM PRINT-TOT-POR-PROFESOR.
           PERFORM PRINT-SALTO-DE-PAGINA.

       PRINT-TOT-POR-PROFESOR.
           MOVE TOT-HORAS-PROF TO E6-TOT-HORAS.
           MOVE TOT-IMP-PROF TO E6-TOT-IMPORTE.
           WRITE REG-LISTADO FROM ENCABEZADO6-SUBTOT-PROFESOR.

       PRINT-SALTO-DE-PAGINA.
           SUBTRACT LINEA FROM 60 GIVING RESTO-LINEAS.
           MOVE 1 TO I.
           PERFORM PRINT-LINEAS-EN-BLANCO UNTIL I > RESTO-LINEAS.
           MOVE 0 TO LINEA.
           ADD 1 TO HOJA.

       PRINT-LINEAS-EN-BLANCO.
           WRITE REG-LISTADO FROM LINEA-EN-BLANCO.
           ADD 1 TO I.

       PROCESO2.
           DISPLAY "Ejecutar Proceso2".
           PERFORM DETERMINAR-CLAVE-MENOR.
           MOVE 0 TO TOT-IMP-FECHA.
           MOVE 0 TO TOT-HORAS-FECHA.
           WRITE REG-LISTADO FROM LINEA-EN-BLANCO.
           PERFORM PRINT-ENCABEZADO-TABLA.
           PERFORM PROCESO3 UNTIL (FS-NOVTIMES1 = 10
               AND FS-NOVTIMES2 = 10 AND FS-NOVTIMES3 = 10
               AND FS-PROFESORES = 10)
               OR (CLAVE-MENOR-FECHA <> CLAVE-FECHA1
               AND CLAVE-MENOR-FECHA <> CLAVE-FECHA2
               AND CLAVE-MENOR-FECHA <> CLAVE-FECHA3).
           PERFORM PRINT-LINEA-SUBTOTAL.
           PERFORM PRINT-TOT-POR-FECHA.
           ADD 2 TO LINEA.
           IF LINEA > 60
               MOVE 0 TO LINEA
               ADD 1 TO HOJA
               PERFORM PRINT-ENCABEZADO
               ADD 3 TO LINEA
           END-IF.

       PRINT-LINEA-SUBTOTAL.
           DISPLAY"                      ------   --------------".
           WRITE REG-LISTADO FROM LINEA-SUBTOTAL.
           ADD 1 TO LINEA.

       PRINT-TOT-POR-FECHA.
           MOVE TOT-HORAS-FECHA TO E5-TOT-HORAS.
           MOVE TOT-IMP-FECHA TO E5-TOT-IMPORTE.
           WRITE REG-LISTADO FROM ENCABEZADO5-SUBTOT-FECHA.

       PROCESO3.
           DISPLAY "EJECUTAR Proceso3".
           PERFORM DETERMINAR-CLAVE-MENOR.
           PERFORM POS-SUC1 UNTIL FS-NOVTIMES1 = 10
               OR CLAVE-MENOR-SUC <> CLAVE-SUC1.
           PERFORM POS-SUC2 UNTIL FS-NOVTIMES2 = 10
               OR CLAVE-MENOR-SUC <> CLAVE-SUC2.
           PERFORM POS-SUC3 UNTIL FS-NOVTIMES3 = 10
               OR CLAVE-MENOR-SUC <> CLAVE-SUC3.

       POS-SUC1.
           MOVE REG-NOVTIMES1 TO REG-NOVTIMES.
           PERFORM PROCESO-NOV.
           PERFORM LEER-NOVTIMES1.

       POS-SUC2.
           MOVE REG-NOVTIMES2 TO REG-NOVTIMES.
           PERFORM PROCESO-NOV.
           PERFORM LEER-NOVTIMES2.

       POS-SUC3.
           MOVE REG-NOVTIMES3 TO REG-NOVTIMES.
           PERFORM PROCESO-NOV.
           PERFORM LEER-NOVTIMES3.

       PROCESO-NOV.
           PERFORM GUARDAR-EN-TIMES.
           PERFORM BUSCAR-TIPO-CLASE.
           PERFORM PRINT-DATOS-E-IMPORTE.
           PERFORM CALCULAR-TOTALES.

       GUARDAR-EN-TIMES.
           MOVE REG-NOVTIMES TO REG-TIMES.
           WRITE REG-TIMES.

       BUSCAR-TIPO-CLASE.
           SET INDICE TO 1.
           SEARCH VEC-TIPOSCLASE
           AT END PERFORM NO-ENCONTRADO
           WHEN VEC-TIPOSCLASE-TIPO(INDICE) IS EQUAL TO NOV-TIPCLASE
               MOVE NOV-HORAS TO HORAS
               PERFORM CALCULAR-IMPORTE
               MOVE VEC-TIPOSCLASE-DESC(INDICE) TO DESCRIPCION
               MOVE VEC-TIPOSCLASE-TARIFA(INDICE) TO TARIFA
           END-SEARCH.

       NO-ENCONTRADO.
           DISPLAY "NO ENCONTRADO".

       CALCULAR-IMPORTE.
           MULTIPLY VEC-TIPOSCLASE-TARIFA(INDICE) BY HORAS
           GIVING IMPORTE.

       PRINT-DATOS-E-IMPORTE.
           DISPLAY IMPORTE.
           MOVE NOV-FECHA TO FECHA-DATO.
           MOVE FECHAD-DD TO DT-FECHA-DD.
           MOVE FECHAD-MM TO DT-FECHA-MM.
           MOVE FECHAD-AAAA TO DT-FECHA-AAAA.
           MOVE NOV-SUCURSAL TO DT-SUC.
           MOVE DESCRIPCION TO DT-TIPO.
           MOVE TARIFA TO DT-TARIFA.
           MOVE NOV-HORAS TO DT-HORAS.
           MOVE IMPORTE TO DT-IMPORTE.
           WRITE REG-LISTADO FROM DATOS-TABLA.

       CALCULAR-TOTALES.
           DISPLAY "Calcula totales".
           ADD IMPORTE TO TOT-IMP-FECHA.
           ADD HORAS TO TOT-HORAS-FECHA.
           ADD IMPORTE TO TOT-IMP-PROF.
           ADD HORAS TO TOT-HORAS-PROF.
           ADD IMPORTE TO TOT-GRAL.

       PRINT-TOTAL-GRAL.
           DISPLAY "Imprimir totales".
           DISPLAY TOT-GRAL.
           MOVE TOT-GRAL TO E7-TOT-IMPORTE.
           WRITE REG-LISTADO FROM ENCABEZADO7-TOT-GENERAL.

       CERRAR-ARCHIVOS.
           CLOSE NOVTIMES1_FILE.
           CLOSE NOVTIMES2_FILE.
           CLOSE NOVTIMES3_FILE.
           CLOSE PROFESORES_FILE.
           CLOSE SUCURSALES_FILE.
           CLOSE TIPOSCLASE_FILE.
           CLOSE TIMES_FILE.
           CLOSE LISTADO_FILE.

       END PROGRAM "TP_PARTE_1A".
