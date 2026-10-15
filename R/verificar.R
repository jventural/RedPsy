#' Comparación de la red entre grupos (paso 6)
#'
#' Compara la red de dos grupos con el Network Comparison Test (permutaciones):
#' si la estructura es la misma (M: la mayor diferencia entre aristas) y si la
#' conectividad global es la misma (S: diferencia en la suma de las aristas).
#' Solo se realiza si cada grupo tiene al menos `minimo` participantes: con
#' grupos pequeños la red del grupo menor sale incompleta.
#'
#' @param red El resultado de [rp_red()].
#' @param grupo Columna de la base con los grupos, por ejemplo `"sexo"`. Se
#'   usan los dos grupos más frecuentes.
#' @param minimo Participantes mínimos por grupo.
#' @param permutaciones Número de permutaciones.
#' @return Un resultado `rp_resultado` con la prueba y su lectura (o con los
#'   tamaños de los grupos si no se puede hacer).
#' @examples
#' \donttest{
#' base <- rp_preparar(rp_ejemplo(), escalas = c(SCS = "1-5", BP = "1-3", EG = "1-5"), inversos = "EG5")
#' nodos <- rp_nodos(base, GRA = "EG1:EG5, EG7:EG10, EG12:EG17",
#'                   AAM = "SCS5, SCS12, SCS19, SCS23, SCS26", AJU = "SCS1, SCS8, SCS11, SCS16, SCS21",
#'                   PRO = "BP1, BP3, BP6, BP10", ACE = "BP2, BP11, BP13")
#' rp_grupos(rp_red(nodos), "sexo")
#' }
#' @export
rp_grupos <- function(red, grupo, minimo = 150, permutaciones = 1000) {
  .comprobar_red(red)
  base <- as.data.frame(red$nodos$base)
  if (!grupo %in% names(base)) stop("La columna \"", grupo, "\" no est\u00e1 en la base.", call. = FALSE)
  g <- as.character(base[[grupo]])
  ng <- sort(table(g), decreasing = TRUE)
  dos <- names(ng)[1:2]
  if (ng[2] < minimo) {
    tabla <- data.frame(Grupo = names(ng), n = as.integer(ng), check.names = FALSE)
    return(.resultado(tabla, sprintf("Comparaci\u00f3n de la red por %s (rp_grupos)", grupo), list(.nota(2, "n", sprintf(
      "El grupo %s tiene %d participantes (< %d): su red saldr\u00eda incompleta y la comparaci\u00f3n no ser\u00eda confiable. NO se realiza; se declara como limitaci\u00f3n.",
      dos[2], ng[2], minimo)))))
  }
  if (!requireNamespace("NetworkComparisonTest", quietly = TRUE))
    stop("Instala NetworkComparisonTest: install.packages(\"NetworkComparisonTest\")", call. = FALSE)
  P <- red$nodos$puntajes
  d1 <- stats::na.omit(P[g %in% dos[1], , drop = FALSE]); d2 <- stats::na.omit(P[g %in% dos[2], , drop = FALSE])
  invisible(utils::capture.output(nct <- suppressMessages(suppressWarnings(NetworkComparisonTest::NCT(
    d1, d2, it = permutaciones, gamma = .5, progressbar = FALSE)))))
  tabla <- data.frame(Prueba = c("Estructura (M)", "Conectividad global (S)"), Valor = .f2(c(nct$nwinv.real, nct$glstrinv.real)),
                      p = .fp(c(nct$nwinv.pval, nct$glstrinv.pval)), check.names = FALSE)
  notas <- list(.nota(1, "p", sprintf("Grupos: %s (n = %d) y %s (n = %d). p %s: %s", dos[1], nrow(d1), dos[2], nrow(d2), .fpe(nct$nwinv.pval),
    if (nct$nwinv.pval >= .05) "no hay evidencia de que la estructura de la red difiera entre grupos." else "la estructura difiere: al menos una arista cambia seg\u00fan el grupo.")),
    .nota(2, "p", sprintf("p %s: %s", .fpe(nct$glstrinv.pval), if (nct$glstrinv.pval >= .05)
      "la red est\u00e1 igual de conectada en ambos grupos." else "un grupo tiene la red m\u00e1s conectada que el otro.")))
  .resultado(tabla, sprintf("Comparaci\u00f3n de la red por %s (rp_grupos)", grupo), notas, nct = nct)
}

#' Tamaño de muestra por simulación (va en Participantes)
#'
#' Toma la red estimada como si fuera la verdadera, simula muestras de varios
#' tamaños y comprueba qué tan bien se recupera: sensibilidad (aristas reales
#' que se detectan), especificidad (aristas nulas que no aparecen) y
#' correlación entre los pesos verdaderos y los estimados. En las redes no hay
#' potencia en el sentido clásico: se informa con qué N la red se recupera bien.
#'
#' @param red El resultado de [rp_red()].
#' @param tamanos Tamaños de muestra a simular.
#' @param replicas Réplicas por tamaño.
#' @param nucleos Núcleos del procesador.
#' @return Un resultado `rp_resultado` con la sensibilidad, la especificidad y
#'   la correlación por tamaño, y su lectura.
#' @examples
#' \donttest{
#' base <- rp_preparar(rp_ejemplo(), escalas = c(SCS = "1-5", BP = "1-3", EG = "1-5"), inversos = "EG5")
#' nodos <- rp_nodos(base, GRA = "EG1:EG5, EG7:EG10, EG12:EG17",
#'                   AAM = "SCS5, SCS12, SCS19, SCS23, SCS26", AJU = "SCS1, SCS8, SCS11, SCS16, SCS21",
#'                   PRO = "BP1, BP3, BP6, BP10", ACE = "BP2, BP11, BP13")
#' rp_muestra(rp_red(nodos), tamanos = c(150, 300), replicas = 10)
#' }
#' @export
rp_muestra <- function(red, tamanos = c(100, 150, 200, 300, 500), replicas = 50, nucleos = 1) {
  .comprobar_red(red)
  n <- nrow(stats::na.omit(red$nodos$puntajes))
  tamanos <- sort(unique(c(tamanos, n)))
  args <- list(input = red$pesos, nCases = tamanos, nReps = replicas, nCores = nucleos, default = red$estimador, verbose = FALSE)
  if (red$estimador == "ggmModSelect") args$stepwise <- TRUE
  invisible(utils::capture.output(sim <- suppressMessages(suppressWarnings(do.call(bootnet::netSimulator, args)))))
  sim <- as.data.frame(sim)
  r <- stats::aggregate(cbind(sensitivity, specificity, correlation) ~ nCases, sim, mean, na.rm = TRUE)
  tabla <- data.frame(N = r$nCases, Sensibilidad = .f2(r$sensitivity), Especificidad = .f2(r$specificity),
                      "Correlaci\u00f3n con la red verdadera" = .f2(r$correlation), check.names = FALSE)
  i <- which(r$nCases == n)
  ok <- r$nCases[r$sensitivity >= .80 & r$correlation >= .70]
  notas <- list(.nota(i, "Sensibilidad", sprintf("Con tu N = %d se detecta el %s %% de las aristas reales y la correlaci\u00f3n con la red verdadera es %s.",
    n, round(100 * r$sensitivity[i]), .f2(r$correlation[i]))),
    .nota(1, "Correlaci\u00f3n con la red verdadera", if (length(ok))
      sprintf("Desde N = %d la red se recupera bien (sensibilidad \u2265 .80 y correlaci\u00f3n \u2265 .70). Este p\u00e1rrafo va en Participantes.", min(ok)) else
      "Ning\u00fan tama\u00f1o simulado alcanza sensibilidad \u2265 .80 y correlaci\u00f3n \u2265 .70: la red es dif\u00edcil de recuperar; decl\u00e1ralo."))
  .resultado(tabla, "Tama\u00f1o de muestra por simulaci\u00f3n (rp_muestra)", notas)
}

#' Análisis de sensibilidad (paso 7)
#'
#' Repite la red con otras decisiones y la compara con la principal:
#' EBICglasso en lugar de ggmModSelect (o al revés), correlaciones de Pearson
#' y sin los participantes con respuestas planas. Para cada variante informa
#' la correlación de sus aristas con la red principal, la densidad y si se
#' mantienen la arista más fuerte y el nodo más central.
#'
#' @param red El resultado de [rp_red()].
#' @return Un resultado `rp_resultado` con cada variante y su lectura.
#' @examples
#' \donttest{
#' base <- rp_preparar(rp_ejemplo(), escalas = c(SCS = "1-5", BP = "1-3", EG = "1-5"), inversos = "EG5")
#' nodos <- rp_nodos(base, GRA = "EG1:EG5, EG7:EG10, EG12:EG17",
#'                   AAM = "SCS5, SCS12, SCS19, SCS23, SCS26", AJU = "SCS1, SCS8, SCS11, SCS16, SCS21",
#'                   PRO = "BP1, BP3, BP6, BP10", ACE = "BP2, BP11, BP13")
#' rp_sensibilidad(rp_red(nodos))
#' }
#' @export
rp_sensibilidad <- function(red) {
  .comprobar_red(red)
  P <- red$nodos$puntajes; W0 <- red$pesos
  otro <- if (red$estimador == "ggmModSelect") "EBICglasso" else "ggmModSelect"
  plana <- as.data.frame(red$nodos$base)$respuesta_plana
  var <- list(list(n = red$estimador, P = P, e = red$estimador, c = red$correlacion, et = "Principal"),
              list(P = P, e = otro, c = red$correlacion, et = paste("Con", otro)),
              list(P = P, e = red$estimador, c = "pearson", et = "Correlaciones de Pearson"))
  if (any(plana, na.rm = TRUE)) var[[4]] <- list(P = P[!plana, , drop = FALSE], e = red$estimador, c = red$correlacion,
                                                 et = sprintf("Sin respuestas planas (n = %d)", sum(!plana, na.rm = TRUE)))
  top <- function(W) { ij <- which(upper.tri(W) & W != 0, arr.ind = TRUE); k <- which.max(abs(W[ij]))
    paste(rownames(W)[ij[k, 1]], "\u2014", colnames(W)[ij[k, 2]]) }
  posibles <- ncol(P) * (ncol(P) - 1) / 2
  filas <- lapply(var, function(v) {
    W <- if (v$et == "Principal") W0 else { w <- .estimar(v$P, v$e, v$c)$graph; dimnames(w) <- dimnames(W0); w }
    ut <- upper.tri(W)
    data.frame(Variante = v$et, "r con la principal" = .f2(stats::cor(W[ut], W0[ut])),
               Densidad = .f2(sum(W[ut] != 0) / posibles), "Arista m\u00e1s fuerte" = top(W),
               "Nodo m\u00e1s central" = colnames(W)[which.max(colSums(W))], check.names = FALSE)
  })
  tabla <- do.call(rbind, filas)
  r <- as.numeric(tabla[["r con la principal"]][-1])
  mismo <- all(tabla[["Arista m\u00e1s fuerte"]] == tabla[["Arista m\u00e1s fuerte"]][1])
  notas <- list(.nota(which.min(c(Inf, r)), "r con la principal", sprintf(
    "Las aristas de las variantes correlacionan entre %s y %s con la red principal: %s",
    .f2(min(r)), .f2(max(r)), if (min(r) >= .90) "la red no depende de esas decisiones." else "alguna decisi\u00f3n cambia la red; inf\u00f3rmalo.")),
    .nota(2, "Densidad", sprintf("Con %s la densidad es %s (frente a %s): cada estimador equilibra distinto sensibilidad y especificidad, por eso cambia cu\u00e1ntas aristas retiene.",
      otro, tabla$Densidad[2], tabla$Densidad[1])))
  notas[[3]] <- .nota(1, "Arista m\u00e1s fuerte", if (mismo) "La arista m\u00e1s fuerte es la misma en todas las variantes." else
    "La arista m\u00e1s fuerte cambia en alguna variante: interpreta su primac\u00eda con cautela.")
  .resultado(tabla, "An\u00e1lisis de sensibilidad (rp_sensibilidad)", notas)
}
