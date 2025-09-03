       01 DCLNOMINA.
           05 EMP-ID           PIC X(06).
           05 EMP-NOMBRE       PIC X(30).
           05 EMP-DEPTO        PIC X(03).
           05 SALARIO-BASE     PIC S9(08)V99 COMP-3.
           05 BONOS           PIC S9(08)V99 COMP-3.
           05 DEDUCCIONES     PIC S9(08)V99 COMP-3.
           05 SALARIO-NETO    PIC S9(08)V99 COMP-3.
           05 FECHA-PAGO      PIC X(10).
           05 FECHA-PROCESO   PIC X(26).
           05 STATUS          PIC X(01).