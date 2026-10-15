#' Centralidad y nodos puente (paso 4)
#'
#' Calcula la influencia esperada de cada nodo (suma de sus aristas con signo;
#' válida cuando todos los nodos puntúan en la misma dirección) y la fuerza
#' puente (suma de sus aristas con nodos de OTRAS comunidades). Los nodos en el
#' 20 % superior de fuerza puente se señalan como puentes. No se usan la
#' intermediación ni la cercanía, que no se recomiendan en redes psicológicas.
#'
#' @param red El resultado de [rp_red()].
#' @return Un resultado `rp_resultado` con la tabla por nodo (influencia
#'   esperada en bruto y en z, fuerza puente, puente sí/no y R2) y su lectura.
#'   Con `plot()` se dibuja la influencia esperada y la fuerza puente en z.
#' @examples
#' \donttest{
#' base <- rp_preparar(rp_ejemplo(), escalas = c(SCS = "1-5", BP = "1-3", EG = "1-5"), inversos = "EG5")
#' nodos <- rp_nodos(base, GRA = "EG1:EG5, EG7:EG10, EG12:EG17",
#'                   AAM = "SCS5, SCS12, SCS19, SCS23, SCS26", AJU = "SCS1, SCS8, SCS11, SCS16, SCS21",
#'                   PRO = "BP1, BP3, BP6, BP10", ACE = "BP2, BP11, BP13")
#' rp_centralidad(rp_red(nodos))
#' }
#' @export
rp_centralidad <- function(red) {
  .comprobar_red(red)
  W <- red$pesos; com <- red$nodos$comunidades
  ie <- colSums(W); ie_z <- as.numeric(scale(ie))
  fp <- vapply(seq_len(ncol(W)), function(j) sum(abs(W[com != com[j], j])), 0)
  corte <- stats::quantile(fp, .80)
  puente <- fp >= corte & fp > 0
  o <- order(-ie_z)
  tabla <- data.frame(Nodo = colnames(W)[o], Comunidad = unname(com[o]), IE = .f2(ie[o]), "IE (z)" = .f2(ie_z[o]),
                      "Fuerza puente" = .f2(fp[o]), Puente = ifelse(puente[o], "S\u00ed", ""),
                      "R\u00b2" = .f2(red$R2[colnames(W)][o]), check.names = FALSE)
  pu <- which(tabla$Puente != "")
  notas <- list(
    .nota(1, "IE (z)", sprintf("%s tiene la mayor influencia esperada (z = %s): es el nodo m\u00e1s conectado de la red. Si cambiara, arrastrar\u00eda a sus vecinos m\u00e1s que cualquier otro.",
      tabla$Nodo[1], tabla[["IE (z)"]][1])),
    .nota(nrow(tabla), "IE (z)", sprintf("%s tiene la menor influencia esperada (z = %s): est\u00e1 en la periferia de la red.",
      tabla$Nodo[nrow(tabla)], tabla[["IE (z)"]][nrow(tabla)])))
  if (length(pu)) notas[[3]] <- .nota(pu[1], "Puente", sprintf("Puentes (20 %% superior de fuerza puente): %s. Conectan su instrumento con los dem\u00e1s; antes de interpretarlos, mira su estabilidad (rp_estabilidad).",
    .lista(tabla$Nodo[pu])))
  res <- .resultado(tabla, "Centralidad y nodos puente (rp_centralidad)", notas)
  res$z <- data.frame(nodo = colnames(W), ie = ie_z, fp = as.numeric(scale(fp)))
  class(res) <- c("rp_centralidad", class(res))
  res
}

#' Figura de centralidad
#'
#' @param x El resultado de [rp_centralidad()].
#' @param ... No se usa.
#' @return Un gráfico `ggplot` con la influencia esperada y la fuerza puente en z.
#' @export
plot.rp_centralidad <- function(x, ...) {
  z <- x$z; o <- z$nodo[order(z$ie)]
  l <- rbind(data.frame(nodo = z$nodo, medida = "Influencia esperada", valor = z$ie),
             data.frame(nodo = z$nodo, medida = "Fuerza puente", valor = z$fp))
  l$nodo <- factor(l$nodo, levels = o)
  ggplot2::ggplot(l, ggplot2::aes(valor, nodo, group = medida)) +
    ggplot2::geom_path(colour = "#BB1715") + ggplot2::geom_point(colour = "#BB1715", size = 2.5) +
    ggplot2::facet_wrap(~ medida) + ggplot2::labs(x = "z", y = NULL) +
    ggplot2::theme_minimal(base_size = 14) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

#' Precisión y estabilidad (paso 5)
#'
#' Evalúa cuánto confiar en la red con dos remuestreos de `bootnet`:
#' (1) bootstrap no paramétrico de las aristas, que da el IC 95 % de cada peso;
#' (2) bootstrap por eliminación de casos, que da el coeficiente de estabilidad
#' CS de la influencia esperada y de la fuerza puente: la proporción máxima de
#' casos que se puede quitar manteniendo, con 95 % de seguridad, una correlación
#' de .70 con el orden original. CS de .50 o más es adecuado; menos de .25, el
#' índice no se interpreta.
#'
#' @param red El resultado de [rp_red()].
#' @param remuestreos Número de remuestreos de cada tipo.
#' @param nucleos Núcleos del procesador para acelerar.
#' @return Un resultado `rp_resultado` con los coeficientes CS y su lectura;
#'   en `aristas`, otro con el IC bootstrap de cada arista presente.
#' @examples
#' \donttest{
#' base <- rp_preparar(rp_ejemplo(), escalas = c(SCS = "1-5", BP = "1-3", EG = "1-5"), inversos = "EG5")
#' nodos <- rp_nodos(base, GRA = "EG1:EG5, EG7:EG10, EG12:EG17",
#'                   AAM = "SCS5, SCS12, SCS19, SCS23, SCS26", AJU = "SCS1, SCS8, SCS11, SCS16, SCS21",
#'                   PRO = "BP1, BP3, BP6, BP10", ACE = "BP2, BP11, BP13")
#' rp_estabilidad(rp_red(nodos), remuestreos = 50)
#' }
#' @export
rp_estabilidad <- function(red, remuestreos = 1000, nucleos = 1) {
  .comprobar_red(red)
  com <- red$nodos$comunidades
  grupos <- as.integer(factor(com, levels = unique(com)))
  q <- function(expr) { invisible(utils::capture.output(r <- suppressMessages(suppressWarnings(expr)))); r }
  bc <- q(bootnet::bootnet(red$red, nBoots = remuestreos, type = "case", nCores = nucleos,
                           statistics = c("expectedInfluence", "bridgeStrength"), communities = grupos, verbose = FALSE))
  bn <- q(bootnet::bootnet(red$red, nBoots = remuestreos, type = "nonparametric", nCores = nucleos, verbose = FALSE))
  cs <- q(bootnet::corStability(bc, verbose = FALSE))
  lee <- function(v) if (v >= .75) "excelente" else if (v >= .50) "adecuado" else if (v >= .25) "bajo: interpretar solo a grandes rasgos" else "insuficiente: no se interpreta"
  tabla <- data.frame("\u00cdndice" = c("Influencia esperada", "Fuerza puente"),
                      CS = .f2(c(cs["expectedInfluence"], cs["bridgeStrength"])),
                      Lectura = c(lee(cs["expectedInfluence"]), lee(cs["bridgeStrength"])), check.names = FALSE)
  notas <- list(
    .nota(1, "CS", sprintf("CS = %s: se puede quitar hasta el %s %% de los casos y el orden de la influencia esperada se mantiene. %s.",
      .f2(cs["expectedInfluence"]), round(100 * cs["expectedInfluence"]), sub("^(.)", "\\U\\1", lee(cs["expectedInfluence"]), perl = TRUE))),
    .nota(2, "CS", sprintf("CS = %s en la fuerza puente: %s. Con un CS bajo, los nodos puente se presentan como tentativos.",
      .f2(cs["bridgeStrength"]), lee(cs["bridgeStrength"]))))
  se <- summary(bn, statistics = "edge")
  se <- as.data.frame(se)
  W <- red$pesos
  ij <- which(upper.tri(W) & W != 0, arr.ind = TRUE)
  pres <- data.frame(a = rownames(W)[ij[, 1]], b = colnames(W)[ij[, 2]], w = W[ij])
  pres <- pres[order(-abs(pres$w)), ]
  idx <- match(paste(pres$a, pres$b, sep = "--"), paste(se$node1, se$node2, sep = "--"))
  idx2 <- match(paste(pres$b, pres$a, sep = "--"), paste(se$node1, se$node2, sep = "--"))
  idx[is.na(idx)] <- idx2[is.na(idx)]
  li <- se$q2.5[idx]; ls <- se$q97.5[idx]
  excl <- !is.na(li) & li * ls > 0
  ta <- data.frame(Arista = paste(pres$a, "\u2014", pres$b), Peso = .f2(pres$w), "IC 95 % bootstrap" = .ic(li, ls),
                   "Excluye el cero" = ifelse(excl, "S\u00ed", "No"), check.names = FALSE)
  ino <- which(!excl)
  na <- list(.nota(1, "IC 95 % bootstrap", sprintf("%d de %d aristas tienen un IC que excluye el cero: son las que se pueden afirmar con seguridad. La m\u00e1s fuerte, %s, va de %s a %s.",
    sum(excl), nrow(ta), ta$Arista[1], .f2(li[1]), .f2(ls[1]))))
  if (length(ino)) na[[2]] <- .nota(ino[1], "Excluye el cero", sprintf("%s: su IC incluye el cero. Est\u00e1 en la red estimada, pero con esta muestra no se distingue de una arista nula.",
    ta$Arista[ino[1]]))
  .resultado(tabla, "Estabilidad de la centralidad (rp_estabilidad)", notas,
             aristas = .resultado(ta, "Precisi\u00f3n de las aristas (bootstrap no param\u00e9trico)", na), bootstrap = list(casos = bc, aristas = bn))
}
