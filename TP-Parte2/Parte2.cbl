       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP-PARTE2.
       AUTHOR. "ADRIAN MOULY - SEBASTIAN TORRES".
       DATE-WRITTEN. "2DO CUATRIMESTRE 2015".

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT TIMES-FILE
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Times.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TIM-CLAVE OF REG-TIMES
           ALTERNATE RECORD KEY IS TIM-CUIT OF REG-TIMES
           WITH DUPLICATES
           FILE STATUS IS FS-TIMES.

       SELECT PROFESORES-FILE
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Profesores.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS PROF-NUMERO OF REG-PROFESORES
           FILE STATUS IS FS-PROFESORES.


       SELECT TARIFAS-FILE
           ASSIGN TO DISK "../files/in/ArchivosIndexados/Tarifas.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TAR-CLAVE OF REG-TARIFAS
           FILE STATUS IS FS-TARIFAS.

       SELECT PARAMETROS-FILE
           ASSIGN TO DISK "../files/in/ArchivosTexto/Parametros.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-PARAMETROS.

       SELECT LISTADO
           ASSIGN TO DISK "../files/out/ListadoSucursal.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-LISTADO.

       SELECT ARCH-ORDEN
           ASSIGN TO DISK "../files/out/ListadoSucursal-Temporal.tmp"
           SORT STATUS IS SS-ORDEN.

       DATA DIVISION.
       FILE SECTION.

       FD TIMES-FILE
           LABEL RECORD IS STANDARD.
       01 REG-TIMES.
           03 TIM-CLAVE.
               05 TIM-NUMERO   PIC X(05).
               05 TIM-FECHA    PIC 9(08).
               05 TIM-CUIT     PIC 9(11).
               05 TIM-SEC      PIC 9(04).
           03 TIM-TIP-CLASE    PIC X(04).
           03 TIM-HORAS        PIC 9(2)V99.


       FD PROFESORES-FILE
           LABEL RECORD IS STANDARD.
       01 REG-PROFESORES.
           03  PROF-NUMERO     PIC X(05).
           03  PROF-DNI        PIC 9(08).
           03  PROF-NOMBRE     PIC X(25).
           03  PROF-DIRE       PIC X(20).
           03  PROF-TEL        PIC X(20).


       FD TARIFAS-FILE
           LABEL RECORD IS STANDARD.
       01 REG-TARIFAS.
           03  TAR-CLAVE.
               05 TAR-TIP-CLASE PIC X(04).
               05 TAR-VIG-DES   PIC 9(08).
           03  TAR-TARIFA       PIC 9(05)V99.

       FD PARAMETROS-FILE
           LABEL RECORD IS STANDARD.
       01 REG-PARAMETROS.
           03 PAR-CUIT-DESDE    PIC 9(11).
           03 PAR-CUIT-HASTA    PIC 9(11).

       FD LISTADO
           LABEL RECORD IS STANDARD.
       01  REG-LISTADO         PIC X(80).


       SD ARCH-ORDEN
           DATA RECORD IS REG-ORD.
       01  REG-ORD.
           03 ORD-CLAVE.
               05 REG-ORD-RAZON             PIC X(25).
               05 REG-ORD-CUIT              PIC 9(11).
               05 REG-ORD-FECHA.
                   07  ORD-FECHA-AAAA       PIC 9999.
                   07  ORD-FECHA-MM         PIC 99.
                   07  ORD-FECHA-DD         PIC 99.
               05  REG-ORD-PROF-NUMERO      PIC X(05).
           03  REG-ORD-PROF-NOMBRE          PIC X(25).
           03  REG-ORD-HORAS                PIC 9(02)V99.
           03  REG-ORD-IMPORTE              PIC 9(07)V99.

       WORKING-STORAGE SECTION.

       77 FS-TIMES       PIC XX.
       77 FS-PROFESORES  PIC XX.
       77 FS-TARIFAS     PIC XX.
       77 FS-LISTADO     PIC XX.
       77 FS-PARAMETROS  PIC XX.
       77 SS-ORDEN       PIC XX.

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
           03  FILLER      PIC X(50)   VALUE SPACES.
           03  FILLER      PIC X(5)    VALUE "Hoja ".
           03  E1-HOJA     PIC 9(3).

       01 ENCABEZADO2.
           03 FILLER PIC x(20) VALUE SPACES.
           03 FILLER PIC X(38) VALUE "Horas Aplicadas por Sucursal".
           03 FILLER PIC x(26) VALUE SPACES.

       01 LINEA-EN-BLANCO.
           03 FILLER PIC X(80) VALUE SPACES.

       01 ENCABEZADO3-SUCURSAL.
           03  FILLER      PIC X(10)   VALUE "Sucursal: ".
           03  E3-SUCURSAL PIC X(25).
           03  FILLER      PIC X(45)   VALUE SPACES.

       01 ENCABEZADO4-CUIT.
           03  FILLER      PIC X(6)    VALUE "Cuit: ".
           03  E4-CUIT     PIC 9(11).
           03  FILLER      PIC X(63)   VALUE SPACES.

       01 ENCABEZADO4-TABLA.
           03 FILLER       PIC X(10)   VALUE "   Fecha  ".
           03 FILLER       PIC X(15)   VALUE "       Profesor".
           03 FILLER       PIC X(27)   VALUE "        Nombre".
           03 FILLER       PIC X(10)   VALUE "   Horas  ".
           03 FILLER       PIC X(18)   VALUE "      Importe".

       01 LINEA-TABLA.
           03 FILLER       PIC X(80)   VALUE ALL "_".

       01 LINEA-SUBTOTAL.
           03 FILLER       PIC X(50)   VALUE ALL " ".
           03 FILLER       PIC X(10)   VALUE "    ------".
           03 FILLER       PIC X(20)   VALUE "       ----------- ".

       01 DATOS-TABLA1.
           03 DT1-FECHA-DD     PIC 9(2).
           03 FILLER           PIC X       VALUE "/".
           03 DT1-FECHA-MM     PIC 9(2).
           03 FILLER           PIC X       VALUE "/".
           03 DT1-FECHA-AAAA   PIC 9(4).
           03 FILLER           PIC X(8)    VALUE ALL " ".
           03 DT1-PROF         PIC X(5).
           03 FILLER           PIC X(5)    VALUE ALL " ".
           03 DT1-NOMBRE       PIC X(25).
           03 FILLER           PIC X(2)    VALUE ALL " ".
           03 DT1-HORAS        PIC Z9,99.
           03 FILLER           PIC X(8)    VALUE ALL " ".
           03 DT1-IMPORTE      PIC ZZZZZZ9,99.
           03 FILLER           PIC X(2)    VALUE ALL " ".

       01 DATOS-TABLA2.
           03 FILLER           PIC X(18)   VALUE ALL " ".
           03 DT2-PROF         PIC X(5).
           03 FILLER           PIC X(5)    VALUE ALL " ".
           03 DT2-NOMBRE       PIC X(25).
           03 FILLER           PIC X(2)    VALUE ALL " ".
           03 DT2-HORAS        PIC Z9,99.
           03 FILLER           PIC X(8)    VALUE ALL " ".
           03 DT2-IMPORTE      PIC ZZZZZZ9,99.
           03 FILLER           PIC X(2)    VALUE ALL " ".


       01 ENCABEZADO5-SUBTOT-FECHA.
           03 FILLER           PIC X(17)    VALUE "Totales por fecha".
           03 FILLER           PIC X(37)    VALUE ALL " ".
           03 E5-TOT-HORAS     PIC ZZ9,99.
           03 FILLER           PIC X(7)     VALUE ALL " ".
           03 E5-TOT-IMPORTE   PIC ZZZZZZZ9,99.
           03 FILLER           PIC X(1)     VALUE ALL " ".


       01 ENCABEZADO7-TOT-GENERAL.
           03 FILLER          PIC X(13)    VALUE "Total general".
           03 FILLER          PIC X(51)    VALUE ALL " ".
           03 E7-TOT-IMPORTE  PIC ZZZZZZZZZ9,99.
           03 FILLER          PIC X        VALUE " ".

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

       77 TOT-GRAL          PIC 9(10)V99   VALUE ZEROES.
       77 TOT-FECHA-HORAS   PIC 9(03)V99   VALUE ZEROES.
       77 TOT-FECHA-IMP     PIC 9(08)V99   VALUE ZEROES.
       77 ANT-FECHA         PIC 9(08)      VALUE ZEROES.
       77 ANT-RAZON         PIC X(25)      VALUE SPACES.
       77 LINEA             PIC 99.
       77 HOJA              PIC 999.
       77 IMPORTE           PIC 9999999V99.
       77 RESTO-LINEAS      PIC 99.
       77 I                 PIC 99.
       77 FECHA-ANT         PIC 9(8).
       77 TARIFA-VIG        PIC 9(5)V99.
       77 OP                PIC X.

       PROCEDURE DIVISION.

           PERFORM INICIALIZAR.
           PERFORM SORT-SECTION.
           STOP RUN.

       INICIALIZAR.
           DISPLAY "Inicializar Variables".
           MOVE 0 TO LINEA.
           MOVE 1 TO HOJA.

      *
      *     ENTRADA-SECTION

       SORT-SECTION.
           DISPLAY "SORT SECTION".
           SORT ARCH-ORDEN ASCENDING KEY ORD-CLAVE OF REG-ORD
           INPUT PROCEDURE ENTRADA
           OUTPUT PROCEDURE SALIDA.

       ENTRADA.
           DISPLAY "ENTRADA".
           PERFORM ABRIR-ARCHIVOS.
           MOVE "ABRIR" TO IN-OP.
           CALL "BUSCAR-SUCURSAL" USING PAR-IN, PAR-OUT.
           PERFORM LEER-PARAMETROS.
           MOVE PAR-CUIT-DESDE TO TIM-CUIT.
           START TIMES-FILE KEY IS >= TIM-CUIT.
           IF FS-TIMES EQUAL TO 00
               PERFORM LEER-TIMES
               PERFORM PROCESAR-POR-SUCURSAL UNTIL FS-TIMES = 10
                   OR TIM-CUIT > PAR-CUIT-HASTA
           END-IF.
           MOVE "CERRAR" TO IN-OP.
           CALL "BUSCAR-SUCURSAL" USING PAR-IN, PAR-OUT.
           PERFORM CERRAR-ARCHIVOS.

       ABRIR-ARCHIVOS.
           DISPLAY "ABRIR ARCHIVOS".
           OPEN INPUT TIMES-FILE.
           IF FS-TIMES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIMES
               STOP RUN
           END-IF.

           OPEN INPUT PROFESORES-FILE.
           IF FS-PROFESORES IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR PROFESORES FS: " FS-PROFESORES
               STOP RUN
           END-IF.

           OPEN INPUT TARIFAS-FILE.
           IF FS-TARIFAS IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR TARIFAS FS: " FS-TARIFAS
               STOP RUN
           END-IF.

           OPEN INPUT PARAMETROS-FILE.
           IF FS-PARAMETROS IS NOT EQUAL TO 00
               DISPLAY "ERROR AL ABRIR PARAMETROS FS: " FS-PARAMETROS
               STOP RUN
           END-IF.

       LEER-PARAMETROS.
           DISPLAY "LEER PARAMETROS".
           READ PARAMETROS-FILE.
           IF FS-PARAMETROS IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER PARAMETROS FS: " FS-PARAMETROS
           END-IF.
           DISPLAY PAR-CUIT-DESDE PAR-CUIT-HASTA.

       LEER-PROFESORES.
           DISPLAY "LEER PROFESORES".
           READ PROFESORES-FILE RECORD
           KEY IS PROF-NUMERO.
           IF FS-PROFESORES IS NOT EQUAL TO 00 AND 23
               DISPLAY "ERROR AL LEER PROFESORES FS: " FS-PROFESORES
           END-IF.
           IF FS-PROFESORES IS EQUAL TO 23
               DISPLAY "PROFESOR NO ENCONTRADO"
           END-IF.

       LEER-TARIFAS.
           DISPLAY "LEER TARIFAS".
           READ TARIFAS-FILE NEXT RECORD.
           IF FS-TARIFAS IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER TARIFAS FS: " FS-TARIFAS
           END-IF.

       LEER-TIMES.
           DISPLAY "LEER TIMES".
           READ TIMES-FILE NEXT RECORD
           AT END
           IF FS-TIMES IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER TIMES FS: " FS-TIMES
           END-IF.

       CERRAR-ARCHIVOS.
           CLOSE TIMES-FILE.
           CLOSE PROFESORES-FILE.
           CLOSE TARIFAS-FILE.
           CLOSE PARAMETROS-FILE.

       PROCESAR-POR-SUCURSAL.
           DISPLAY "PROCESAR POR SUCURSAL"
           PERFORM BUSCAR-PROFESOR.
           PERFORM BUSCAR-TARIFA.
           PERFORM CALCULAR-IMPORTE.
           MOVE "BUSCAR" TO IN-OP.
           MOVE TIM-CUIT TO IN-CUIT.
           CALL "BUSCAR-SUCURSAL" USING PAR-IN, PAR-OUT.
           IF OUT-CR <> 00
               DISPLAY "ERROR AL BUSCAR SUCURSAL"
           END-IF.
           PERFORM ARMAR-REG-ORD.
           RELEASE REG-ORD.
           PERFORM LEER-TIMES.


       BUSCAR-PROFESOR.
           DISPLAY "BUSCAR PROFESOR".
           MOVE TIM-NUMERO TO PROF-NUMERO.
           PERFORM LEER-PROFESORES.

       BUSCAR-TARIFA.
           DISPLAY "BUSCAR TARIFA".
           MOVE TIM-TIP-CLASE TO TAR-TIP-CLASE.
           MOVE 0 TO TAR-VIG-DES.
           START TARIFAS-FILE KEY IS >= TAR-CLAVE.
           IF FS-TARIFAS = 00
               PERFORM LEER-TARIFAS
               PERFORM PROCESAR-TARIFA UNTIL FS-TARIFAS = 10
                   OR TAR-TIP-CLASE <> TIM-TIP-CLASE
                   OR TAR-VIG-DES > TIM-FECHA
           END-IF.

       PROCESAR-TARIFA.
           MOVE TAR-TARIFA TO TARIFA-VIG.
           PERFORM LEER-TARIFAS.

       CALCULAR-IMPORTE.
           DISPLAY "CALCULAR IMPORTE".
           MULTIPLY TARIFA-VIG BY TIM-HORAS
           GIVING IMPORTE.

       ARMAR-REG-ORD.
           DISPLAY "ARMAR REGISTRO ORDENADO".
           MOVE SUC-RAZON OF OUT-REG-SUCURSALES TO REG-ORD-RAZON.
           MOVE TIM-CUIT TO REG-ORD-CUIT.
           MOVE TIM-FECHA TO REG-ORD-FECHA.
           MOVE PROF-NUMERO TO REG-ORD-PROF-NUMERO.
           MOVE PROF-NOMBRE TO REG-ORD-PROF-NOMBRE.
           MOVE TIM-HORAS TO REG-ORD-HORAS.
           MOVE IMPORTE TO REG-ORD-IMPORTE.

      *
      *     SALIDA-SECTION

       SALIDA.
           DISPLAY "SALIDA".
           PERFORM ABRIR-LISTADO.
           MOVE 0 TO TOT-GRAL.
           PERFORM LEER-ARCH-ORDEN.
           PERFORM PROCESO1 UNTIL SS-ORDEN = 10
           PERFORM PRINT-ENCABEZADO.
           PERFORM PRINT-TOTAL-GRAL.
           PERFORM PRINT-SALTO-DE-PAGINA.

           PERFORM CERRAR-LISTADO.


       ABRIR-LISTADO.
           OPEN OUTPUT LISTADO.
           IF FS-LISTADO <> 00
               DISPLAY "ERROR AL ABRIR ARCHIVO LISTADO FS: "
                 FS-LISTADO
               ACCEPT OP
               STOP RUN
           END-IF.

       LEER-ARCH-ORDEN.
           RETURN ARCH-ORDEN RECORD
           AT END
           IF SS-ORDEN IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR AL LEER ARCH-ORDEN SS: " SS-ORDEN
           END-IF.

       CERRAR-LISTADO.
           CLOSE LISTADO.

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

       PRINT-SALTO-DE-PAGINA.
           SUBTRACT LINEA FROM 60 GIVING RESTO-LINEAS.
           MOVE 1 TO I.
           PERFORM PRINT-LINEAS-EN-BLANCO UNTIL I > RESTO-LINEAS.
           MOVE 0 TO LINEA.
           ADD 1 TO HOJA.

       PRINT-LINEAS-EN-BLANCO.
           WRITE REG-LISTADO FROM LINEA-EN-BLANCO.
           ADD 1 TO I.

       PRINT-TOTAL-GRAL.
           DISPLAY "Imprimir totales".
           DISPLAY TOT-GRAL.
           MOVE TOT-GRAL TO E7-TOT-IMPORTE.
           WRITE REG-LISTADO FROM ENCABEZADO7-TOT-GENERAL.

       PROCESO1.
           MOVE REG-ORD-RAZON TO ANT-RAZON.
           PERFORM PRINT-ENCABEZADO-SUCURSAL.
           PERFORM PROCESO2 UNTIL SS-ORDEN = 10
               OR ANT-RAZON <> REG-ORD-RAZON.

           PERFORM PRINT-SALTO-DE-PAGINA.

       PRINT-ENCABEZADO-SUCURSAL.
           DISPLAY "Imprimir encabezado sucursal".
           PERFORM PRINT-ENCABEZADO.
           PERFORM PRINT-DATOS-SUCURSAL.

       PRINT-DATOS-SUCURSAL.
           DISPLAY "Imprimir datos sucursal".
           DISPLAY REG-ORD-RAZON.
           DISPLAY SUC-RAZON.
           DISPLAY REG-ORD-CUIT.
           MOVE REG-ORD-RAZON TO E3-SUCURSAL.
           MOVE REG-ORD-CUIT TO E4-CUIT.
           WRITE REG-LISTADO FROM ENCABEZADO3-SUCURSAL.
           WRITE REG-LISTADO FROM ENCABEZADO4-CUIT.
           WRITE REG-LISTADO FROM LINEA-EN-BLANCO.
           ADD 3 TO LINEA.

       PRINT-ENCABEZADO-TABLA.
           DISPLAY "Fecha   Profesor   Nombre  Horas   Importe".
           WRITE REG-LISTADO FROM ENCABEZADO4-TABLA.
           WRITE REG-LISTADO FROM LINEA-TABLA.
           ADD 2 TO LINEA.

       PROCESO2.
           MOVE 0 TO TOT-FECHA-HORAS.
           MOVE 0 TO TOT-FECHA-IMP.
           PERFORM PRINT-ENCABEZADO-TABLA.
           MOVE REG-ORD-FECHA TO ANT-FECHA.
           MOVE HIGH-VALUE TO FECHA-ANT.
           PERFORM PROCESO3 UNTIL SS-ORDEN = 10
               OR ANT-RAZON <> REG-ORD-RAZON
               OR ANT-FECHA <> REG-ORD-FECHA.

           PERFORM PRINT-LINEA-SUBTOTAL.
           PERFORM PRINT-TOT-POR-FECHA.
           ADD 3 TO LINEA.
           PERFORM CHECK-NUEVA-PAGINA.

       PRINT-LINEA-SUBTOTAL.
           DISPLAY"                      ------   --------------".
           WRITE REG-LISTADO FROM LINEA-SUBTOTAL.
           ADD 1 TO LINEA.

       PRINT-TOT-POR-FECHA.
           MOVE TOT-FECHA-HORAS TO E5-TOT-HORAS.
           MOVE TOT-FECHA-IMP TO E5-TOT-IMPORTE.
           WRITE REG-LISTADO FROM ENCABEZADO5-SUBTOT-FECHA.
           WRITE REG-LISTADO FROM LINEA-EN-BLANCO.

       CHECK-NUEVA-PAGINA.
           IF LINEA > 60
               MOVE 0 TO LINEA
               ADD 1 TO HOJA
               PERFORM PRINT-ENCABEZADO
               ADD 3 TO LINEA
           END-IF.


       PROCESO3.
           PERFORM PRINT-DATOS-FECHA.

           ADD REG-ORD-IMPORTE TO TOT-GRAL.
           ADD REG-ORD-HORAS TO TOT-FECHA-HORAS.
           ADD REG-ORD-IMPORTE TO TOT-FECHA-IMP.

           PERFORM CHECK-NUEVA-PAGINA.
           PERFORM LEER-ARCH-ORDEN.

       PRINT-DATOS-FECHA.
           DISPLAY IMPORTE.
           IF FECHA-ANT <> ANT-FECHA
               MOVE ORD-FECHA-DD TO DT1-FECHA-DD
               MOVE ORD-FECHA-MM TO DT1-FECHA-MM
               MOVE ORD-FECHA-AAAA TO DT1-FECHA-AAAA
               MOVE REG-ORD-PROF-NUMERO TO DT1-PROF
               MOVE REG-ORD-PROF-NOMBRE TO DT1-NOMBRE
               MOVE REG-ORD-HORAS TO DT1-HORAS
               MOVE REG-ORD-IMPORTE TO DT1-IMPORTE
               WRITE REG-LISTADO FROM DATOS-TABLA1
               MOVE REG-ORD-FECHA TO FECHA-ANT
            ELSE
               MOVE REG-ORD-PROF-NUMERO TO DT2-PROF
               MOVE REG-ORD-PROF-NOMBRE TO DT2-NOMBRE
               MOVE REG-ORD-HORAS TO DT2-HORAS
               MOVE REG-ORD-IMPORTE TO DT2-IMPORTE
               WRITE REG-LISTADO FROM DATOS-TABLA2
            END-IF.
