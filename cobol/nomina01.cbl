       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOMINA01.
       AUTHOR. Javier J. Tapia.
       DATE-WRITTEN. 2024.
      *---------------------------------------------------------------*
      * Programa batch para procesar archivo VSAM y actualizar DB2    *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOMINA-VSAM ASSIGN TO NOMVSAM
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS NV-EMP-ID
                  FILE STATUS IS VSAM-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  NOMINA-VSAM
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS NOMINA-RECORD.
       01  NOMINA-RECORD.
           05 NV-EMP-ID           PIC X(06).
           05 NV-EMP-NOMBRE       PIC X(30).
           05 NV-EMP-DEPTO        PIC X(03).
           05 NV-SALARIO-BASE     PIC 9(08)V99.
           05 NV-BONOS            PIC 9(08)V99.
           05 NV-DEDUCCIONES      PIC 9(08)V99.
           05 NV-FECHA-PAGO       PIC X(10).
           05 FILLER              PIC X(23).

       WORKING-STORAGE SECTION.
       01  WS-CONTROLES.
           05 VSAM-STATUS         PIC XX.
           05 WS-EOF-SW           PIC X VALUE 'N'.
              88 WS-EOF            VALUE 'Y'.
              88 WS-NOT-EOF        VALUE 'N'.
           05 WS-CONTADORES.
              10 WS-REG-LEIDOS    PIC 9(05) VALUE ZEROS.
              10 WS-REG-PROCESADOS PIC 9(05) VALUE ZEROS.
              10 WS-REG-ERROR     PIC 9(05) VALUE ZEROS.
           05 WS-SQLCODE          PIC S9(09) COMP.
           05 WS-SQLSTATE         PIC X(05).

       01  WS-FECHA-ACTUAL.
           05 WS-ANIO             PIC 9(04).
           05 WS-MES              PIC 9(02).
           05 WS-DIA              PIC 9(02).

       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE DCLNOMINA END-EXEC.

       EXEC SQL DECLARE NOMINA TABLE
         (EMP_ID         CHAR(6)     NOT NULL,
          EMP_NOMBRE     VARCHAR(30) NOT NULL,
          EMP_DEPTO      CHAR(3)     NOT NULL,
          SALARIO_BASE   DECIMAL(10,2),
          BONOS          DECIMAL(10,2),
          DEDUCCIONES    DECIMAL(10,2),
          SALARIO_NETO   DECIMAL(10,2),
          FECHA_PAGO     DATE,
          FECHA_PROCESO  TIMESTAMP,
          STATUS         CHAR(1),
          PRIMARY KEY (EMP_ID))
       END-EXEC.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESAR-ARCHIVO UNTIL WS-EOF
           PERFORM 3000-FINALIZAR
           STOP RUN.

       1000-INICIO.
           OPEN INPUT NOMINA-VSAM
           IF VSAM-STATUS NOT = '00'
              DISPLAY 'ERROR AL ABRIR VSAM: ' VSAM-STATUS
              MOVE 16 TO RETURN-CODE
              STOP RUN
           END-IF

           EXEC SQL
              CONNECT TO PRODDB
           END-EXEC
           IF SQLCODE NOT = 0
              DISPLAY 'ERROR CONEXION DB2: ' SQLCODE
              MOVE 16 TO RETURN-CODE
              STOP RUN
           END-IF

           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-FECHA-ACTUAL
           DISPLAY 'INICIO PROCESO: ' WS-FECHA-ACTUAL.

       2000-PROCESAR-ARCHIVO.
           READ NOMINA-VSAM
              AT END SET WS-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-REG-LEIDOS
                 PERFORM 2100-PROCESAR-REGISTRO
           END-READ.

       2100-PROCESAR-REGISTRO.
           MOVE SPACES TO DCLNOMINA
           MOVE NV-EMP-ID      TO EMP-ID
           MOVE NV-EMP-NOMBRE  TO EMP-NOMBRE
           MOVE NV-EMP-DEPTO   TO EMP-DEPTO
           MOVE NV-SALARIO-BASE TO SALARIO-BASE
           MOVE NV-BONOS       TO BONOS
           MOVE NV-DEDUCCIONES TO DEDUCCIONES
           
           COMPUTE SALARIO-NETO = 
              (SALARIO-BASE + BONOS) - DEDUCCIONES
           
           MOVE NV-FECHA-PAGO  TO FECHA-PAGO
           MOVE FUNCTION CURRENT-TIMESTAMP TO FECHA-PROCESO
           MOVE 'A' TO STATUS

           EXEC SQL
              INSERT INTO NOMINA 
                 (EMP_ID, EMP_NOMBRE, EMP_DEPTO, 
                  SALARIO_BASE, BONOS, DEDUCCIONES, 
                  SALARIO_NETO, FECHA_PAGO, 
                  FECHA_PROCESO, STATUS)
              VALUES 
                 (:EMP-ID, :EMP-NOMBRE, :EMP-DEPTO,
                  :SALARIO-BASE, :BONOS, :DEDUCCIONES,
                  :SALARIO-NETO, :FECHA-PAGO,
                  :FECHA-PROCESO, :STATUS)
           END-EXEC

           IF SQLCODE = 0
              ADD 1 TO WS-REG-PROCESADOS
           ELSE
              ADD 1 TO WS-REG-ERROR
              DISPLAY 'ERROR INSERT EMP: ' EMP-ID 
                      ' SQLCODE: ' SQLCODE
           END-IF.

       3000-FINALIZAR.
           CLOSE NOMINA-VSAM
           
           EXEC SQL
              COMMIT WORK
           END-EXEC
           
           EXEC SQL
              CONNECT RESET
           END-EXEC
           
           DISPLAY 'REGISTROS LEIDOS: ' WS-REG-LEIDOS
           DISPLAY 'REGISTROS PROCESADOS: ' WS-REG-PROCESADOS
           DISPLAY 'REGISTROS CON ERROR: ' WS-REG-ERROR
           DISPLAY 'FIN DEL PROCESO'.

       END PROGRAM NOMINA01.