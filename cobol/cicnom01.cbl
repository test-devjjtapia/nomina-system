       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICNOM01.
       AUTHOR. Javier J. Tapia
       DATE-WRITTEN. 2024.
      *---------------------------------------------------------------*
      * Programa CICS para consultar información de nóminas en DB2    *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CAMPOS-CONTROL.
           05 WS-TRAN-ID          PIC X(04) VALUE 'NOM1'.
           05 WS-MAPSET           PIC X(08) VALUE 'NOMIMAP'.
           05 WS-MAP              PIC X(08) VALUE 'NOMIMAP'.
           05 WS-RESP             PIC S9(08) COMP.
           05 WS-RESP2            PIC S9(08) COMP.
           05 WS-SQLCODE          PIC S9(09) COMP.
           05 WS-SQLSTATE         PIC X(05).

       01  WS-DATOS-EMPLEADO.
           05 WS-EMP-ID           PIC X(06).
           05 WS-EMP-NOMBRE       PIC X(30).
           05 WS-EMP-DEPTO        PIC X(03).
           05 WS-SALARIO-BASE     PIC S9(08)V99 COMP-3.
           05 WS-BONOS           PIC S9(08)V99 COMP-3.
           05 WS-DEDUCCIONES     PIC S9(08)V99 COMP-3.
           05 WS-SALARIO-NETO    PIC S9(08)V99 COMP-3.
           05 WS-FECHA-PAGO      PIC X(10).

       01  WS-MENSAJES.
           05 WS-MSG-TEXTO        PIC X(60).
           05 WS-MSG-LONG         PIC S9(04) COMP.

       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE DCLNOMINA END-EXEC.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 CA-EMP-ID           PIC X(06).
          05 CA-ACCION           PIC X(01).
             88 CA-CONSULTA      VALUE 'C'.
             88 CA-FIN           VALUE 'F'.
          05 CA-RESPUESTA        PIC X(01).
             88 CA-OK            VALUE '0'.
             88 CA-ERROR         VALUE '1'.
          05 CA-MENSAJE          PIC X(60).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS HANDLE AID
                PF3(9000-FIN)
                PF12(9000-FIN)
                ANYKEY(9100-KEY-ERROR)
           END-EXEC

           EXEC CICS HANDLE CONDITION
                MAPFAIL(8000-PRIMERA-VEZ)
                NOTFND(8200-EMP-NO-ENCONTRADO)
                ERROR(8300-ERROR-CICS)
           END-EXEC

           IF EIBCALEN > 0
              MOVE DFHCOMMAREA TO CA-EMP-ID
              PERFORM 2000-PROCESAR-CONSULTA
           ELSE
              PERFORM 8000-PRIMERA-VEZ
           END-IF.

       2000-PROCESAR-CONSULTA.
           EXEC CICS RECEIVE MAP(WS-MAP)
                MAPSET(WS-MAPSET)
                RESP(WS-RESP)
                RESP2(WS-RESP2)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
              PERFORM 8300-ERROR-CICS
           END-IF

           MOVE EMIEMPIDI TO WS-EMP-ID
           PERFORM 3000-CONSULTAR-DB2
           
           IF CA-OK
              PERFORM 4000-MOSTRAR-RESULTADO
           ELSE
              PERFORM 8500-MOSTRAR-ERROR
           END-IF.

       3000-CONSULTAR-DB2.
           MOVE WS-EMP-ID TO EMP-ID
           
           EXEC SQL
              SELECT EMP_NOMBRE, EMP_DEPTO, SALARIO_BASE,
                     BONOS, DEDUCCIONES, SALARIO_NETO,
                     FECHA_PAGO
              INTO :EMP-NOMBRE, :EMP-DEPTO, :SALARIO-BASE,
                   :BONOS, :DEDUCCIONES, :SALARIO-NETO,
                   :FECHA-PAGO
              FROM NOMINA
              WHERE EMP_ID = :EMP-ID
                AND STATUS = 'A'
           END-EXEC

           EVALUATE SQLCODE
              WHEN 0
                 SET CA-OK TO TRUE
                 MOVE EMP-NOMBRE   TO WS-EMP-NOMBRE
                 MOVE EMP-DEPTO    TO WS-EMP-DEPTO
                 MOVE SALARIO-BASE TO WS-SALARIO-BASE
                 MOVE BONOS        TO WS-BONOS
                 MOVE DEDUCCIONES  TO WS-DEDUCCIONES
                 MOVE SALARIO-NETO TO WS-SALARIO-NETO
                 MOVE FECHA-PAGO   TO WS-FECHA-PAGO
                 MOVE 'CONSULTA EXITOSA' TO CA-MENSAJE
                 
              WHEN 100
                 SET CA-ERROR TO TRUE
                 MOVE 'EMPLEADO NO ENCONTRADO' TO CA-MENSAJE
                 
              WHEN OTHER
                 SET CA-ERROR TO TRUE
                 MOVE 'ERROR EN CONSULTA DB2' TO CA-MENSAJE
                 MOVE SQLCODE TO WS-SQLCODE
                 DISPLAY 'ERROR DB2: ' WS-SQLCODE
           END-EVALUATE.

       4000-MOSTRAR-RESULTADO.
           MOVE WS-EMP-NOMBRE   TO EMNOMBREO
           MOVE WS-EMP-DEPTO    TO EMDEPTOO
           MOVE WS-SALARIO-BASE TO EMSALBASEO
           MOVE WS-BONOS        TO EMBONOSO
           MOVE WS-DEDUCCIONES  TO EMDEDUCCO
           MOVE WS-SALARIO-NETO TO EMSALNETO
           MOVE WS-FECHA-PAGO   TO EMFECHAPO
           MOVE CA-MENSAJE      TO EMMENSAJEO

           EXEC CICS SEND MAP(WS-MAP)
                MAPSET(WS-MAPSET)
                ERASE
                RESP(WS-RESP)
           END-EXEC

           EXEC CICS RETURN
                TRANSID(WS-TRAN-ID)
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.

       8000-PRIMERA-VEZ.
           EXEC CICS SEND MAP(WS-MAP)
                MAPSET(WS-MAPSET)
                ERASE
                RESP(WS-RESP)
           END-EXEC

           EXEC CICS RETURN
                TRANSID(WS-TRAN-ID)
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.

       8200-EMP-NO-ENCONTRADO.
           MOVE 'EMPLEADO NO ENCONTRADO' TO EMMENSAJEO
           PERFORM 8500-MOSTRAR-ERROR.

       8300-ERROR-CICS.
           MOVE 'ERROR EN SISTEMA CICS' TO EMMENSAJEO
           PERFORM 8500-MOSTRAR-ERROR.

       8500-MOSTRAR-ERROR.
           EXEC CICS SEND MAP(WS-MAP)
                MAPSET(WS-MAPSET)
                ERASE
                RESP(WS-RESP)
           END-EXEC

           EXEC CICS RETURN
                TRANSID(WS-TRAN-ID)
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.

       9000-FIN.
           EXEC CICS SEND TEXT
                FROM('PROGRAMA FINALIZADO')
                ERASE
                FREEKB
           END-EXEC
           EXEC CICS RETURN END-EXEC.

       9100-KEY-ERROR.
           MOVE 'TECLA NO VALIDA' TO EMMENSAJEO
           PERFORM 8500-MOSTRAR-ERROR.

       END PROGRAM CICNOM01.