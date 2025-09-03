# Sistema de Procesamiento de Nóminas
Sistema completo para gestionar nóminas de empleados en entorno mainframe, utilizando COBOL, VSAM, DB2 y CICS.

## 📋 Descripción
Este proyecto consiste en un sistema batch para procesar información de nóminas desde un archivo VSAM hacia una base de datos DB2, junto con un programa transaccional CICS para consultar la información de manera interactiva.

## 📁 Estructura del Proyecto
```
nomina-system/
├── README.md
├── jcl/
│   ├── run_nomina.jcl          # JCL para ejecutar el proceso batch
│   └── compile.jcl             # JCL para compilar los programas
├── cobol/
│   ├── nomina01.cbl            # Programa principal batch
│   ├── dclnomina.cpy           # Copybook para estructuras de datos
│   ├── cicnom01.cbl            # Programa CICS transaccional
│   └── bms/
│       └── nomimap.bms         # Mapa BMS para interfaz CICS
├── sql/
│   └── create_table.sql        # Script de creación de tabla DB2
└── docs/
    └── instructions.md         # Instrucciones detalladas
```

## 🚀 Características Principales
### Programa Batch (nomina01.cbl)

+ Procesamiento secuencial de archivo VSAM

+ Inserción de datos en DB2 con control de transacciones

+ Manejo robusto de errores y contadores de procesamiento

+ Validación de datos y cálculos de nómina

### Programa CICS (cicnom01.cbl)

+ Interfaz interactiva para consultas de nóminas

+ Manejo completo de errores (CICS y DB2)

+ Comunicación segura con base de datos

+ Mensajes descriptivos al usuario

### Base de Datos
+ Tabla normalizada con información de nóminas

+ Índices para optimizar consultas

+ Campos de auditoría (fechas y estado)

## 🛠️ Tecnologías Utilizadas
+ COBOL - Lenguaje de programación principal

+ VSAM - Almacenamiento de datos secuencial indexado

+ DB2 - Base de datos relacional

+ CICS - Sistema de procesamiento transaccional

+ JCL - Language Control de Jobs para ejecución

+ BMS - Basic Mapping Support para interfaces

## 📊 Estructura de Datos
### Archivo VSAM
+ Longitud de registro: 100 bytes

+ Organización: KSDS (Key Sequenced Data Set)

+ Llave primaria: ID de empleado (6 bytes)

+ Campos: nombre, departamento, salarios, bonos, deducciones, fecha

## Tabla DB2 (NOMINA)
```
CREATE TABLE NOMINA (
    EMP_ID CHAR(6) NOT NULL,
    EMP_NOMBRE VARCHAR(30) NOT NULL,
    EMP_DEPTO CHAR(3) NOT NULL,
    SALARIO_BASE DECIMAL(10,2),
    BONOS DECIMAL(10,2),
    DEDUCCIONES DECIMAL(10,2),
    SALARIO_NETO DECIMAL(10,2),
    FECHA_PAGO DATE,
    FECHA_PROCESO TIMESTAMP,
    STATUS CHAR(1)
);
```
## 🚀 Instalación y Configuración
### Prerrequisitos
+ DB2 instalado y configurado

+ CICS ejecutándose

+ Dataset VSAM creado con la estructura definida

### Pasos de Implementación
1. Crear tabla en DB2

```
db2 -tf sql/create_table.sql
```

2. Compilar programas
```
submit jcl/compile.jcl
```
3. Configurar transacción CICS

+ Definir transacción NOM1 en CICS

+ Asociar al programa CICNOM01

+ Configurar plan de DB2 PLANOM01

4. Procesar datos batch
```
submit jcl/run_nomina.jcl
```

5. Probar consulta interactiva

+ En CICS, ejecutar transacción NOM1

+ Ingresar ID de empleado para consultar

## 📝 Uso del Sistema
### Proceso Batch
El programa batch procesa el archivo VSAM y:

1. Lee registros secuencialmente

2. Calcula salario neto (base + bonos - deducciones)

3. Inserta registros en DB2

4. Genera reporte de estadísticas

### Consulta CICS
+ La transacción NOM1 permite:

+ Consultar por ID de empleado

+ Visualizar información completa de nómina

+ Manejar errores con mensajes al usuario

+ Navegación con PF3/PF12 para salir

## 🐛 Manejo de Errores
### Códigos de Error VSAM
+ 00 - Operación exitosa

+ 10 - Fin de archivo

+ Otros - Error de lectura/escritura

### Códigos SQLCODE
+ 0 - Operación exitosa

+ 100 - Registro no encontrado

+ Negativos - Error de DB2

### Mensajes al Usuario
+ "EMPLEADO NO ENCONTRADO"

+ "ERROR EN CONSULTA DB2"

+ "ERROR EN SISTEMA CICS"

+ "TECLA NO VALIDA"

## 📈 Métricas y Monitoreo
### El sistema genera:

+ Conteo de registros leídos

+ Conteo de registros procesados exitosamente

+ Conteo de registros con errores

+ Timestamps de procesamiento

## 🔒 Consideraciones de Seguridad
### Validación de permisos en datasets

+ Control de acceso a DB2 desde CICS y batch

+ Considerar encriptación de datos sensibles

+ Auditoría de transacciones

## 📞 Soporte
### Para issues y preguntas:

+ Revisar documentación en /docs/

+ Verificar logs de ejecución

+ Contactar al equipo de sistemas

## 🤝 Contribuciones
### Las contribuciones son bienvenidas. Por favor:

+ Fork el proyecto

+ Crea una branch para tu feature

+ Commit tus cambios

+ Push a la branch

+ Abre un Pull Request

## 📄 Licencia
Este proyecto es propiedad de la empresa y está sujeto a los términos de licencia interna.

## 🏷️ Versión
* Versión Actual: 1.0.0
+ Última Actualización: 2025
+ Estado: Production Ready

### Nota: Este sistema está diseñado para procesar aproximadamente 1000 registros de empleados con información confidencial de nóminas.
