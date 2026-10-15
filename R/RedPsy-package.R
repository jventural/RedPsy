#' RedPsy: análisis de redes psicológicas sencillo
#'
#' Pocas funciones, una por paso de un análisis de redes. Cada una imprime su
#' tabla y una sección "Cómo se lee" con la interpretación de los valores clave.
#'
#' \itemize{
#'   \item [rp_preparar()]: limpieza, criterios, ítems inversos y respuestas planas (paso 1).
#'   \item [rp_items()]: porcentaje de cada alternativa por ítem.
#'   \item [rp_nodos()]: puntajes de los nodos, descriptivos y redundancia (paso 2).
#'   \item [rp_red()]: estimación de la red y predictibilidad (paso 3).
#'   \item [rp_centralidad()]: influencia esperada y nodos puente (paso 4).
#'   \item [rp_estabilidad()]: IC de las aristas y coeficiente CS (paso 5).
#'   \item [rp_grupos()]: comparación de redes entre grupos (paso 6).
#'   \item [rp_sensibilidad()]: ¿cambia la red con otras decisiones? (paso 7).
#'   \item [rp_muestra()]: tamaño de muestra por simulación (va en Participantes).
#'   \item [rp_ejemplo()]: ruta de la base simulada de ejemplo.
#' }
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"
