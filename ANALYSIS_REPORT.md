# smartReports - Análisis de Código y Detección de Problemas

**Fecha:** 2025-10-27
**Versión analizada:** 0.3.3

## Resumen Ejecutivo

Se detectaron varias inconsistencias entre el README, la documentación y el código real del paquete, incluyendo alucinaciones sobre funcionalidades inexistentes y código obsoleto sin documentar.

---

## 1. Alucinaciones Detectadas

### 1.1 AI Capabilities (CRÍTICO)

**Ubicación:** `DESCRIPTION` línea 4, README

**Descripción del problema:**
El archivo DESCRIPTION menciona:
> "Additionally, it integrates AI capabilities to translate and enhance report text using OpenAI, Anthropic and Google APIs"

**Realidad:**
- ✗ No existe código que implemente integración con OpenAI
- ✗ No existe código que implemente integración con Anthropic
- ✗ No existe código que implemente integración con Google APIs
- ✗ No hay dependencias de paquetes relacionados con LLMs en DESCRIPTION
- ✗ No hay funciones exportadas relacionadas con AI/traducción

**Impacto:** Los usuarios podrían intentar usar funcionalidades que no existen.

**Recomendación:** Eliminar toda referencia a AI capabilities del DESCRIPTION y README.

### 1.2 Soporte de ggplot2 y plotly en buildPlot

**Ubicación:** README línea 172

**Descripción del problema:**
El README menciona:
```r
library = "ggplot2"  # o "plotly"
```

**Realidad:**
- ✗ `buildPlot.R` NO implementa soporte para ggplot2
- ✗ `buildPlot.R` NO implementa soporte para plotly
- ✓ Solo highcharter está implementado
- Los parámetros `library` y `plot.type` están marcados como DEPRECATED en el código (líneas 11-12, 66-67, 110-114)

**Recomendación:** Eliminar referencias a ggplot2 y plotly en buildPlot del README.

---

## 2. Código Obsoleto (Fósiles)

### 2.1 buildHist2D.R

**Estado:** NO exportada en NAMESPACE

**Problemas:**
1. Documentación incompleta: `@title "Title"` (línea 1)
2. Función `get_decimal_places()` duplicada (también en buildHist3D.R)
3. Código comentado sin eliminar:
   ```r
   # colorscale = list(
   #   cbind(seq(0, 1, length.out = length(color.scale)), color.scale)
   # ),
   ```
4. No aparece en NAMESPACE pero se menciona en README línea 357

**Recomendación:**
- Documentar completamente o eliminar del paquete
- Si se mantiene, exportar en NAMESPACE
- Extraer `get_decimal_places()` a un archivo de utilidades compartidas

### 2.2 buildHist3D.R

**Estado:** NO exportada en NAMESPACE

**Problemas:**
1. Documentación incompleta: `@title "Title"` (línea 2)
2. Función `get_decimal_places()` duplicada
3. Variables declaradas pero no utilizadas (línea 57-58):
   ```r
   # X_bin <- Y_bin <- Z_bin <- Z <- NULL
   ```
4. No aparece en NAMESPACE pero se menciona en README línea 358

**Recomendación:**
- Documentar completamente o eliminar del paquete
- Si se mantiene, exportar en NAMESPACE

### 2.3 buildPlot.Model.R

**Estado:** Exportada en NAMESPACE

**Problemas:**
1. Documentación muy básica (líneas 1-14)
2. No hay ejemplos de uso
3. Parámetros con descripciones genéricas ("A data.table...", "The width of the line")

**Recomendación:** Mejorar documentación con ejemplos concretos de uso.

---

## 3. Discrepancias entre README y Código

### 3.1 buildPlot - Estructura de datos

**README dice:**
```r
data = DT  # Con columnas ID, X, Y
```

**Código real requiere:**
```r
data.lines = DT  # Con columnas ID, X, Y, opcional style, optional fill
data.points = DT # Con columnas ID, X, Y, opcional style
```

**Impacto:** Los ejemplos del README no funcionarán con la implementación actual.

**Recomendación:** Actualizar todos los ejemplos en README para usar `data.lines` y `data.points`.

### 3.2 buildTable - Caption en gt

**README dice:**
```r
caption = "Iris Dataset"  # Funciona con todas las librerías
```

**Código real:**
La implementación de `gt` (`.buildTable.gt`) NO implementa el parámetro `caption`. Solo está implementado en:
- ✓ flextable (línea 149-151)
- ✗ gt (falta implementación)
- ✓ kable (línea 239)

**Recomendación:** Implementar caption para gt usando `gt::tab_caption()`.

### 3.3 buildReport - Parámetros obsoletos

**README menciona:**
```r
home_dir = "."
```

**Código real NO tiene:**
El parámetro `home_dir` no existe en la firma de `buildReport()` (línea 19-28 de buildReport.R).

**Recomendación:** Eliminar del README o implementar si es necesario.

---

## 4. Inconsistencias en NAMESPACE

### 4.1 Funciones mencionadas pero no exportadas

| Función | README | NAMESPACE | Archivo existe |
|---------|--------|-----------|----------------|
| buildHist2D | ✓ Menciona | ✗ No exportada | ✓ Sí |
| buildHist3D | ✓ Menciona | ✗ No exportada | ✓ Sí |

**Impacto:** Los usuarios intentarán usar `smartReports::buildHist2D()` y obtendrán error.

**Recomendación:** Exportar estas funciones o eliminarlas del README y considerarlas internas.

---

## 5. Problemas Menores

### 5.1 Código comentado

**buildHist2D.R:**
- Líneas 91-93: código comentado
- Líneas 103-105: código comentado

**buildHist3D.R:**
- Línea 57-58: declaraciones comentadas

**Recomendación:** Eliminar código comentado o documentar por qué se mantiene.

### 5.2 Funciones de utilidad duplicadas

`get_decimal_places()` aparece idéntica en:
- buildHist2D.R (líneas 124-130)
- buildHist3D.R (líneas 236-242)

**Recomendación:** Mover a R/local.R o crear R/utils.R

### 5.3 nolint comments

**Archivos con `# nolint start/end`:**
- buildPlot.R (línea 1)
- buildReport.R (línea 1)
- buildPlot.Model.R (línea 1, 78)

**Recomendación:** Revisar y corregir los problemas de linting en lugar de deshabilitarlo.

---

## 6. Verificación de Ejemplos Reales

### 6.1 Análisis de uso en ~/github/psha/

**Patrones de uso confirmados:**

✓ **buildPlot con data.lines:**
```r
buildPlot(
    data.lines = DATA,
    line.type = "spline",
    xAxis.log = TRUE,
    yAxis.log = TRUE,
    xAxis.legend = "Sa(Tn) [g]",
    yAxis.legend = "AEP [1/yr]"
)
```

✓ **buildTable con flextable:**
```r
buildTable(
    library = "flextable",
    align.body = "center",
    font.size.body = 10,
    font.size.header = 12
)
```

**Conclusión:** Los ejemplos reales confirman que:
- Se usa principalmente `data.lines` (no `data`)
- Se usa principalmente `flextable` para tablas
- El uso es consistente con el código actual (no con el README)

---

## 7. Recomendaciones Prioritarias

### Alta Prioridad
1. ✗ **Eliminar referencias a AI capabilities** del DESCRIPTION y README
2. ✗ **Corregir ejemplos de buildPlot** para usar `data.lines`/`data.points`
3. ✗ **Eliminar referencias a ggplot2 y plotly** en buildPlot
4. ✗ **Decidir el futuro de buildHist2D y buildHist3D** (exportar o documentar como internas)

### Media Prioridad
5. Implementar `caption` en buildTable.gt
6. Consolidar funciones de utilidad duplicadas
7. Mejorar documentación de buildPlot.Model
8. Eliminar código comentado

### Baja Prioridad
9. Revisar y corregir problemas de linting
10. Añadir más ejemplos de uso real al README
11. Crear viñetas con casos de uso completos

---

## 8. Funciones Realmente Exportadas

Según NAMESPACE (líneas 3-8):
```r
export(buildPlot)
export(buildPlot.Bar)
export(buildPlot.Histogram)
export(buildPlot.Model)
export(buildReport)
export(buildTable)
```

**Total:** 6 funciones exportadas (no 8+ como sugiere el README)

---

## Conclusiones

El paquete smartReports tiene una implementación funcional y útil para:
- Crear gráficos interactivos con highcharter (`buildPlot`)
- Crear tablas formateadas con flextable/gt/kable (`buildTable`)
- Generar reportes con Quarto (`buildReport`)

Sin embargo, el README contiene **alucinaciones significativas** sobre funcionalidades que no existen (AI capabilities, soporte de ggplot2/plotly) y ejemplos que no coinciden con la API actual del código.

**Acción recomendada:** Actualizar README eliminando alucinaciones y basándolo 100% en el código real y ejemplos de uso verificados.
