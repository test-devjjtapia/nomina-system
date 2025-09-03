# Sistema de Procesamiento de Nóminas

Este proyecto contiene programas para procesar información de nóminas de empleados utilizando VSAM, DB2 y CICS en entorno mainframe.

## Componentes

- **nomina01.cbl**: Programa COBOL batch para procesar archivo VSAM y actualizar DB2
- **cicnom01.cbl**: Programa CICS para consultar información de nóminas en DB2
- **nomimap.bms**: Mapa BMS para la interfaz CICS
- **dclnomina.cpy**: Copybook para estructuras de datos
- **create_table.sql**: Script para crear tabla en DB2

## Requisitos

- COBOL para z/OS
- DB2
- CICS
- VSAM

## Compilación

Utilizar el JCL proporcionado en la carpeta jcl/ para compilar los programas.

## Ejecución

Ejecutar el JCL run_nomina.jcl para procesar el archivo VSAM y actualizar DB2.