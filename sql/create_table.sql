-- Script para crear tabla de nóminas en DB2
CREATE TABLE NOMINA (
    EMP_ID         CHAR(6)     NOT NULL,
    EMP_NOMBRE     VARCHAR(30) NOT NULL,
    EMP_DEPTO      CHAR(3)     NOT NULL,
    SALARIO_BASE   DECIMAL(10,2),
    BONOS          DECIMAL(10,2),
    DEDUCCIONES    DECIMAL(10,2),
    SALARIO_NETO   DECIMAL(10,2),
    FECHA_PAGO     DATE,
    FECHA_PROCESO  TIMESTAMP,
    STATUS         CHAR(1),
    PRIMARY KEY (EMP_ID)
);

COMMENT ON TABLE NOMINA IS 'Tabla de información de nóminas de empleados';

-- Índices para mejorar el performance
CREATE INDEX IDX_NOMINA_DEPTO ON NOMINA(EMP_DEPTO);
CREATE INDEX IDX_NOMINA_STATUS ON NOMINA(STATUS);