# Traceability of reported figures

Generated on 2026-08-24 by R/59_verificar_cifras.R

Every figure quoted in the documentation is read from a versioned results
file rather than transcribed by hand. Each check uses a tolerance derived
from the number of decimal places the source file retains, so that the
margin never exceeds the largest possible rounding error.

| Claim | Value | Expected | Tolerance | Agrees |
|---|---|---|---|---|
| Ganancia de lambda minimo en minoritarias | 0.0395 | 0.0395 | 0.00010 | yes |
| Cobertura conforme, sin crecimiento | 0.9044 | 0.9044 | 0.00005 | yes |
| Cobertura conforme, sangre | 0.8679 | 0.8679 | 0.00005 | yes |
| Cobertura conforme, minimo entre categorias | 0.8679 | 0.8679 | 0.00005 | yes |
| Proporcion resuelta al noventa por ciento | 9.6000 | 9.6000 | 0.05000 | yes |
| Error entre resueltos al noventa por ciento | 0.0280 | 0.0280 | 0.00005 | yes |
| Precision maxima en conclusiones minoritarias | 0.4000 | 0.4000 | 0.00100 | yes |
| Transportabilidad, cobertura minima | 0.8036 | 0.8036 | 0.00005 | yes |
| Transportabilidad, desviacion | 0.0744 | 0.0744 | 0.00005 | yes |
| Transportabilidad, cobertura media | 0.8944 | 0.8944 | 0.00005 | yes |
| Unidad reservada, cobertura sin crecimiento | 0.9911 | 0.9911 | 0.00005 | yes |
| Unidad reservada, casos respiratorios | 13.0000 | 13.0000 | 0.50000 | yes |
| Categorias calibrables con cincuenta casos | 1.0000 | 1.0000 | 0.05000 | yes |
| Tamano medio del conjunto con cincuenta casos | 0.8880 | 0.8880 | 0.00050 | yes |
| Referencia demografica, minoritarias | 0.5421 | 0.5421 | 0.00005 | yes |
| Modelo completo, minoritarias | 0.6735 | 0.6735 | 0.00005 | yes |
| Ganancia del modelo completo sobre tres marcadores | 0.0825 | 0.0825 | 0.00010 | yes |
| Aporte del indicador de solicitud | 0.0134 | 0.0134 | 0.00010 | yes |
| Aporte del valor de lactato | 0.0038 | 0.0038 | 0.00010 | yes |
| Concordancia minima entre reglas de agregacion | 0.8722 | 0.8722 | 0.00005 | yes |
| Beneficio neto maximo sobre politicas triviales | 0.0222 | 0.0222 | 0.00001 | yes |
| Arboles potenciados, ganancia en minoritarias | 0.0002 | 0.0002 | 0.00010 | yes |
| Cobertura de la frecuencia cardiaca | 99.2000 | 99.2000 | 0.05000 | yes |
| Cobertura de la presion sistolica no invasiva | 74.9000 | 74.9000 | 0.05000 | yes |
| Total de valores implausibles marcados | 216.0000 | 216.0000 | 0.50000 | yes |
| Intubacion en la unidad cardiovascular | 72.0000 | 72.0000 | 0.05000 | yes |
| Escala de conciencia dentro del estrato con tubo | 0.4865 | 0.4865 | 0.00005 | yes |
| Escala de conciencia en la cohorte completa | 0.7086 | 0.7086 | 0.00005 | yes |
| Ganancia de la temperatura con spline | 283.0000 | 283.0000 | 0.00500 | yes |
| Ganancia de la saturacion con spline | -47.8500 | -47.8500 | 0.00500 | yes |
| Modelo ampliado, ganancia en minoritarias | 0.0146 | 0.0146 | 0.00010 | yes |
| Modelo ampliado, desviacion de cobertura | 0.0736 | 0.0736 | 0.00005 | yes |
| Modelo ampliado, cobertura minima | 0.7757 | 0.7757 | 0.00005 | yes |

## Source files

- `outputs/fase7/comparacion_lambda.csv`
- `outputs/fase8/cobertura.csv`
- `outputs/fase8/umbrales.csv`
- `outputs/fase8/riesgo_cobertura.csv`
- `outputs/fase9/curva_alfa.csv`
- `outputs/fase9/cobertura_por_alfa.csv`
- `outputs/fase10/cobertura_louo.csv`
- `outputs/fase10/cobertura_clase_louo.csv`
- `outputs/fase10/auc_louo.csv`
- `outputs/fase11/cobertura_sellado.csv`
- `outputs/fase12/recalibracion.csv`
- `outputs/fase13/referencias.csv`
- `outputs/fase15/sensibilidad_lactato.csv`
- `outputs/fase15/comparacion_agregacion.csv`
- `outputs/fase16/curvas_decision.csv`
- `outputs/fase16/gbm_comparacion.csv`
- `outputs/fase17/cobertura_vitales.csv`
- `outputs/fase17/implausibles.csv`
- `outputs/fase17/intubacion_por_unidad.csv`
- `outputs/fase17/circularidad_glasgow.csv`
- `outputs/fase17/vitales_por_estrato.csv`
- `outputs/fase18/comparacion_ampliado.csv`
- `outputs/fase18/ganancia_splines.csv`
- `outputs/fase18/umbrales_ampliado.csv`
- `outputs/fase19/cobertura_louo_ampliado.csv`
- `outputs/fase19/cobertura_clase_louo_ampliado.csv`
- `outputs/fase19/auc_louo_ampliado.csv`
