# RedPsy

Análisis de **redes psicológicas** sencillo. Pocas funciones, una por paso del
análisis; cada una imprime su tabla y una sección **"Cómo se lee"** con la
interpretación de los valores clave.

## Instalación

```r
install.packages("remotes")
remotes::install_github("jventural/RedPsy")
```

## Cada nodo se define con un texto corto

| Texto | Significa |
|---|---|
| `"BP1, BP3, BP6, BP10"` | ítems sueltos |
| `"EG1:EG5, EG7:EG17"` | rangos de ítems |

El puntaje del nodo es la media de sus ítems. Cada nodo pertenece a la
comunidad (instrumento) del prefijo de sus ítems; con `comunidades` les pones nombre.

## El análisis completo

```r
library(RedPsy)

# Paso 1 · Limpiar: rangos, ítems inversos, criterios y respuestas planas
base <- rp_preparar(rp_ejemplo(), escalas = c(SCS = "1-5", BP = "1-3", EG = "1-5"),
                    inversos = "EG5",
                    criterios = c("Consentimiento" = "consentimiento", "Edad 18 o más" = "edad >= 18"))

# Paso 2 · Nodos: puntajes, descriptivos y redundancia
nodos <- rp_nodos(base,
  GRA = "EG1:EG5, EG7:EG10, EG12:EG17",
  AAM = "SCS5, SCS12, SCS19, SCS23, SCS26", AJU = "SCS1, SCS8, SCS11, SCS16, SCS21",
  HUM = "SCS3, SCS7, SCS10, SCS15",         AIS = "SCS4, SCS13, SCS18, SCS25",
  CPL = "SCS9, SCS14, SCS17, SCS22",        SOB = "SCS2, SCS6, SCS20, SCS24",
  PRO = "BP1, BP3, BP6, BP10", ACE = "BP2, BP11, BP13", VIN = "BP5, BP7, BP8", AUT = "BP4, BP9, BP12",
  comunidades = c(EG = "Gratitud", SCS = "Autocompasión", BP = "Bienestar"))

red <- rp_red(nodos)          # Paso 3 · ggmModSelect con Spearman + predictibilidad
plot(red)
rp_centralidad(red)           # Paso 4 · influencia esperada y puentes
rp_estabilidad(red)           # Paso 5 · IC de las aristas y coeficiente CS
rp_grupos(red, "sexo")        # Paso 6 · comparación de redes (si cada grupo tiene 150 o más)
rp_sensibilidad(red)          # Paso 7 · EBICglasso, Pearson, sin respuestas planas
rp_muestra(red)               # N por simulación (va en Participantes)
```

## Funciones

| Función | Paso | Qué hace |
|---|---|---|
| `rp_preparar()` | 1 | limpieza, rangos, inversos, criterios en orden, respuestas planas |
| `rp_items()` | — | porcentaje de cada alternativa por ítem |
| `rp_nodos()` | 2 | puntajes de los nodos, descriptivos y redundancia (goldbricker) |
| `rp_red()` | 3 | red (ggmModSelect o EBICglasso), aristas, densidad, predictibilidad (mgm) |
| `rp_centralidad()` | 4 | influencia esperada y fuerza puente |
| `rp_estabilidad()` | 5 | bootstrap de aristas y coeficiente CS |
| `rp_grupos()` | 6 | Network Comparison Test |
| `rp_sensibilidad()` | 7 | la red con otras decisiones |
| `rp_muestra()` | — | tamaño de muestra por simulación |
| `rp_ejemplo()` | — | ruta de la base simulada incluida |

La base de ejemplo es **simulada** (ficticia): no corresponde a ninguna persona.

## Autor

José Ventura-León
