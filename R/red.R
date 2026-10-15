#' Ruta de la base de ejemplo
#'
#' Base SIMULADA (ficticia) de un estudio de redes: gratitud (EG), escala de
#' autocompasión (SCS-26) y bienestar psicológico (BIEPS-A) en universitarios.
#' Una fila por persona, como llega de un formulario.
#'
#' @return La ruta (texto) del archivo Excel incluido en el paquete.
#' @examples
#' rp_ejemplo()
#' @export
rp_ejemplo <- function() system.file("extdata", "gratitud_autocompasion_bienestar.xlsx", package = "RedPsy", mustWork = TRUE)

#' Nodos de la red: puntajes, descriptivos y redundancia (paso 2)
#'
#' Calcula el puntaje de cada nodo (media de sus ítems; queda vacío si la
#' persona respondió menos del 80 % de ellos), describe cada nodo (media,
#' desviación estándar, asimetría y curtosis) y revisa si algún par de nodos es
#' redundante (procedimiento goldbricker: dos nodos que se correlacionan casi
#' igual con todos los demás miden lo mismo).
#'
#' @param base La base que devuelve [rp_preparar()].
#' @param ... Los nodos con su definición: `GRA = "EG1:EG5, EG7:EG17"`,
#'   `AAM = "SCS5, SCS12, SCS19, SCS23, SCS26"`, ...
#' @param comunidades Opcional: nombre de cada instrumento según el prefijo de
#'   sus ítems, por ejemplo `c(EG = "Gratitud", SCS = "Autocompasión",
#'   BP = "Bienestar")`. Cada nodo pertenece a la comunidad de sus ítems; las
#'   comunidades se usan para los nodos puente.
#' @return Un objeto `rp_nodos`: un `rp_resultado` con los descriptivos y su
#'   lectura, más `redundancia` (otro `rp_resultado`), los `puntajes` y las
#'   `comunidades`.
#' @examples
#' base <- rp_preparar(rp_ejemplo(), escalas = c(SCS = "1-5", BP = "1-3", EG = "1-5"), inversos = "EG5")
#' nodos <- rp_nodos(base, GRA = "EG1:EG5, EG7:EG10, EG12:EG17",
#'                   AAM = "SCS5, SCS12, SCS19, SCS23, SCS26", AJU = "SCS1, SCS8, SCS11, SCS16, SCS21",
#'                   PRO = "BP1, BP3, BP6, BP10", ACE = "BP2, BP11, BP13")
#' nodos
#' @export
rp_nodos <- function(base, ..., comunidades = NULL) {
  if (!inherits(base, "rp_base")) stop("Primero prepara la base con rp_preparar().", call. = FALSE)
  defs <- list(...)
  if (length(defs) < 3 || is.null(names(defs)) || any(names(defs) == ""))
    stop("Indica al menos tres nodos con nombre, por ejemplo: rp_nodos(base, GRA = \"EG\", ...).", call. = FALSE)
  d <- as.data.frame(base)
  it <- lapply(defs, function(x) .definir(x, names(d))$items)
  P <- sapply(it, function(i) { m <- d[, i, drop = FALSE]; ok <- rowMeans(!is.na(m)) >= .80
    ifelse(ok, rowMeans(m, na.rm = TRUE), NA) })
  pref <- vapply(it, function(i) sub("[0-9]+$", "", i[1]), "")
  com <- if (is.null(comunidades)) pref else ifelse(pref %in% names(comunidades), comunidades[pref], pref)
  names(com) <- names(defs)
  asim <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / stats::sd(x)^3 }
  curt <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^4) / stats::sd(x)^4 - 3 }
  g1 <- apply(P, 2, asim); g2 <- apply(P, 2, curt)
  tabla <- data.frame(Nodo = names(defs), Comunidad = unname(com), "\u00cdtems" = lengths(it),
                      M = .f2(colMeans(P, na.rm = TRUE)), DE = .f2(apply(P, 2, stats::sd, na.rm = TRUE)),
                      g1 = .f2(g1), g2 = .f2(g2), check.names = FALSE)
  ia <- which.max(abs(g1))
  notas <- list(
    .nota(1, "Comunidad", sprintf("%d nodos en %d comunidades (%s): cada nodo es una dimensi\u00f3n de un instrumento. La red tendr\u00e1 %d aristas posibles.",
      ncol(P), length(unique(com)), .lista(unique(com)), ncol(P) * (ncol(P) - 1) / 2)),
    .nota(ia, "g1", sprintf("g1 = %s en %s: %s", .f2(g1[ia]), names(defs)[ia], if (abs(g1[ia]) > 1)
      "distribuci\u00f3n asim\u00e9trica (techo o suelo). Por eso la red usa correlaciones de Spearman, que no suponen normalidad." else
      "todas las distribuciones son cercanas a la normal.")))
  # Redundancia (goldbricker)
  gb <- tryCatch(suppressWarnings(networktools::goldbricker(stats::na.omit(as.data.frame(P)), p = .05, method = "hittner2003",
                                                             threshold = .25, corMin = .50, progressbar = FALSE)), error = function(e) NULL)
  sr <- if (is.null(gb)) NULL else gb$suggested_reductions
  sr <- sr[!is.na(sr)]
  R <- stats::cor(P, method = "spearman", use = "pairwise")
  if (length(sr)) {
    ab <- strsplit(names(sr), " & ", fixed = TRUE)
    td <- data.frame("Par de nodos" = vapply(ab, paste, "", collapse = " y "),
                     "r de Spearman" = .f2(vapply(ab, function(x) R[x[1], x[2]], 0)),
                     "Correlaciones que difieren" = .f2(as.numeric(sr)), check.names = FALSE)
    nr <- list(.nota(1, "Correlaciones que difieren", sprintf(
      "%s: solo el %s %% de sus correlaciones con el resto de la red difieren de forma significativa. Son casi intercambiables: se conservan si la teor\u00eda los separa (y se declara), o se fusionan como sensibilidad.",
      td[1, 1], round(100 * as.numeric(sr[1])))))
  } else {
    td <- data.frame("Par de nodos" = "Ninguno", "r de Spearman" = "", "Correlaciones que difieren" = "", check.names = FALSE)
    nr <- list(.nota(1, "Par de nodos", "Ning\u00fan par de nodos es redundante: todos aportan informaci\u00f3n distinta a la red."))
  }
  res <- .resultado(tabla, "Nodos de la red (rp_nodos)", notas,
                    redundancia = .resultado(td, "Redundancia entre nodos (goldbricker)", nr))
  res$puntajes <- P; res$comunidades <- com; res$base <- base; res$defs <- defs
  class(res) <- c("rp_nodos", class(res))
  res
}

.estimar <- function(P, estimador, correlacion) {
  if (correlacion == "pearson") correlacion <- "cor"          # bootnet llama "cor" a Pearson
  args <- list(as.data.frame(P), default = estimador, corMethod = correlacion, verbose = FALSE)
  if (estimador == "ggmModSelect") args$stepwise <- TRUE
  if (estimador == "EBICglasso") args$tuning <- .5
  suppressMessages(suppressWarnings(do.call(bootnet::estimateNetwork, args)))
}

.predictibilidad <- function(P, semilla = 2027) {
  X <- as.matrix(scale(stats::na.omit(P)))
  .con_semilla(semilla, {
    invisible(utils::capture.output(fit <- suppressMessages(mgm::mgm(X, type = rep("g", ncol(X)), level = rep(1, ncol(X)),
                                                                     lambdaSel = "CV", ruleReg = "AND", pbar = FALSE))))
    pr <- stats::predict(fit, X, errorCon = "R2")
  })
  stats::setNames(as.numeric(pr$errors$R2), colnames(P))
}

#' Estimación de la red (paso 3)
#'
#' Estima la red como un modelo gráfico gaussiano: cada arista es la
#' correlación parcial entre dos nodos tras controlar a todos los demás. Por
#' defecto usa ggmModSelect (sin regularización, por pasos, minimizando el BIC)
#' sobre correlaciones de Spearman, como la aplicación NetPsy. Informa las
#' aristas presentes ordenadas por su fuerza, la densidad, las aristas entre
#' instrumentos y la predictibilidad de cada nodo (R2 con `mgm`).
#'
#' @param nodos El resultado de [rp_nodos()].
#' @param estimador `"ggmModSelect"` (por defecto) o `"EBICglasso"`.
#' @param correlacion `"spearman"` (por defecto), `"pearson"` o `"cor_auto"`.
#' @return Un objeto `rp_red`: un `rp_resultado` con las aristas y su lectura,
#'   más `predictibilidad` (otro `rp_resultado`), la red de `bootnet` (`red`) y
#'   la matriz de pesos (`pesos`). Con `plot()` se dibuja la red.
#' @examples
#' \donttest{
#' base <- rp_preparar(rp_ejemplo(), escalas = c(SCS = "1-5", BP = "1-3", EG = "1-5"), inversos = "EG5")
#' nodos <- rp_nodos(base, GRA = "EG1:EG5, EG7:EG10, EG12:EG17",
#'                   AAM = "SCS5, SCS12, SCS19, SCS23, SCS26", AJU = "SCS1, SCS8, SCS11, SCS16, SCS21",
#'                   PRO = "BP1, BP3, BP6, BP10", ACE = "BP2, BP11, BP13")
#' red <- rp_red(nodos)
#' red
#' plot(red)
#' }
#' @export
rp_red <- function(nodos, estimador = "ggmModSelect", correlacion = "spearman") {
  .comprobar_nodos(nodos)
  P <- nodos$puntajes; com <- nodos$comunidades
  net <- .estimar(P, estimador, correlacion)
  W <- net$graph; dimnames(W) <- list(colnames(P), colnames(P))
  ij <- which(upper.tri(W) & W != 0, arr.ind = TRUE)
  ar <- data.frame(a = rownames(W)[ij[, 1]], b = colnames(W)[ij[, 2]], w = W[ij])
  ar <- ar[order(-abs(ar$w)), ]
  ar$entre <- com[ar$a] != com[ar$b]
  posibles <- ncol(P) * (ncol(P) - 1) / 2
  tabla <- data.frame(Arista = paste(ar$a, "\u2014", ar$b), Peso = .f2(ar$w),
                      Tipo = ifelse(ar$entre, "Entre instrumentos", "Dentro del instrumento"), check.names = FALSE)
  ie <- which(ar$entre)
  notas <- list(
    .nota(1, "Peso", sprintf("Arista m\u00e1s fuerte: %s (%s). Es la correlaci\u00f3n parcial: la relaci\u00f3n entre ambos nodos que queda tras controlar todos los dem\u00e1s.",
      tabla$Arista[1], .f2(ar$w[1]))),
    .nota(nrow(tabla), "Arista", sprintf("Densidad = %s: %d de %d aristas posibles est\u00e1n presentes (%d negativas). %s retiene solo las necesarias, por eso la red es dispersa.",
      .f2(nrow(ar) / posibles), nrow(ar), posibles, sum(ar$w < 0), estimador)))
  if (length(ie)) notas[[3]] <- .nota(ie[1], "Tipo", sprintf("%d arista(s) unen instrumentos distintos; la m\u00e1s fuerte, %s (%s). Son las que responden a la pregunta de la tesis: c\u00f3mo se conectan los constructos.",
    length(ie), tabla$Arista[ie[1]], .f2(ar$w[ie[1]])))
  R2 <- .predictibilidad(P)
  o <- order(-R2)
  tp <- data.frame(Nodo = names(R2)[o], Comunidad = unname(com[names(R2)[o]]), "R\u00b2" = .f2(R2[o]), check.names = FALSE)
  np <- list(.nota(1, "R\u00b2", sprintf("R\u00b2 = %s: el %s %% de la variaci\u00f3n de %s se explica por los nodos con los que se conecta. Es el nodo m\u00e1s \"controlable\" desde la red.",
                                       .f2(R2[o][1]), .f2(100 * R2[o][1]), names(R2)[o][1])),
             .nota(nrow(tp), "R\u00b2", sprintf("R\u00b2 = %s en %s: casi todo lo que le pasa a este nodo se explica por fuera de la red. Predictibilidad media = %s.",
                                       .f2(R2[o][nrow(tp)]), names(R2)[o][nrow(tp)], .f2(mean(R2)))))
  res <- .resultado(tabla, sprintf("Red estimada con %s y correlaciones de %s (rp_red)", estimador, correlacion), notas,
                    predictibilidad = .resultado(tp, "Predictibilidad de cada nodo (mgm)", np))
  res$red <- net; res$pesos <- W; res$R2 <- R2; res$nodos <- nodos
  res$estimador <- estimador; res$correlacion <- correlacion
  class(res) <- c("rp_red", class(res))
  res
}

#' Dibujar la red
#'
#' Dibuja la red con `qgraph`: un color por comunidad, aristas verdes
#' (positivas) o rojas (negativas) con su grosor según la fuerza, y la
#' predictibilidad de cada nodo como un anillo a su alrededor.
#'
#' @param x El resultado de [rp_red()].
#' @param ... Argumentos adicionales para `qgraph::qgraph()`.
#' @return De forma invisible, el objeto de `qgraph`.
#' @export
plot.rp_red <- function(x, ...) {
  com <- x$nodos$comunidades
  grupos <- split(seq_along(com), factor(com, levels = unique(com)))
  invisible(qgraph::qgraph(x$pesos, groups = grupos, layout = "spring", pie = x$R2, edge.labels = TRUE,
                           palette = "ggplot2", vsize = 10, esize = 12, legend.cex = .5, labels = colnames(x$pesos),
                           theme = "colorblind", ...))
}
