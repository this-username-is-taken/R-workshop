rv <- function(x, probs = NULL) {
  if (is.rv(x)) x <- as.numeric(x)
  if (is.null(probs)) {
    probs <- rep(1, length(x)) / length(x)
  } else {
    if (any(probs < 0)) stop("Probabilities must be positive")
    if (abs(sum(probs) - 1) > 1e-6) stop("Probabilities must sum to 1")
  }

  # Simplify by summing probabilities with equal x's. Need to use
  # addNA since otherwise tapply silently drops groups with missing values
  grp <- addNA(x, ifany = TRUE)
  x_new <- as.vector(tapply(x, grp, "[", 1))
  probs <- as.vector(tapply(probs, grp, sum))

  # Set probs and class attributes
  structure(x_new, probs = probs, class = "rv")
}

is.rv <- function(x) inherits(x, "rv")

as.rv <- function(x) UseMethod("as.rv")

as.rv.rv <- function(x) x

as.rv.numeric <- function(x) rv(x)
as.rv.integer <- function(x) rv(x)
as.rv.logical <- function(x) rv(x)
as.rv.character <- function(x) rv(x)


probs <- function(x) attr(x, "probs")

print.rv <- function(x, ...) {
  X <- format(x, digits = 3)
  P <- format(probs(x), digits = 3)
  out <- cbind(X = X, "P(X)" = P)
  rownames(out) <- rep("", nrow(out))
  print(out, quote = FALSE)
}

"[.rv" <- function(x, i, ...) {
  rv(as.numeric(x)[i], prop.table(probs(x)[i]))
}

abs.rv <- function(x) {
  rv(NextMethod(), probs(x))
}

log.rv <- function(x, base = exp(1)) {
  rv(NextMethod(), probs(x))
}

exp.rv <- function(x) {
  rv(NextMethod(), probs(x))
}

sqrt.rv <- function(x) {
  rv(NextMethod(), probs(x))
}

plot.rv <- function(x, ...) {
  name <- deparse(substitute(x))
  ylim <- range(0, probs(x))

  graphics::plot(as.numeric(x), probs(x), type = "h", ylim = ylim,
    xlab = name, ylab = paste0("P(", name, ")"), ...)
  graphics::points(as.numeric(x), probs(x), pch = 20)
  graphics::abline(h = 0, col = "gray")
}
P <- function(x) {
  stopifnot(is.logical(x), is.rv(x))
  sum(probs(x)[x])
}

E <- function(x) {
  if (!is.rv(x)) stop("Input must be an rv object")
  sum(as.numeric(x) * probs(x))
}

VAR <- function(x) E((x - E(x)) ^ 2)

SD <- function(x) sqrt(VAR(x))

Z <- function(x) (x - E(x)) / SD(x)
rsim <- function(x, n) {
  stopifnot(is.rv(x))
  sample(as.vector(x), n, prob = probs(x), replace = TRUE)
}
# Combine two independent random variables (including the special case
# of a random variable and a number). We basically generate the complete 
# two way table, and then collapse down to a random variable
combine <- function(e1, e2, fun) {
  e1 <- as.rv(e1)
  e2 <- as.rv(e2)
  
  # Use outer to generate all pairwise combinations, combining using fun
  vals <- outer(e1, e2, fun)
  # Probabilities get multipled together
  probs <- outer(probs(e1), probs(e2), "*")
  
  # Rely on rv to collapse any duplicates
  rv(as.vector(vals), as.vector(probs))
}

"+.rv" <- function(e1, e2) combine(e1, e2, `+`)
"-.rv" <- function(e1, e2) combine(e1, e2, `-`)
"*.rv" <- function(e1, e2) combine(e1, e2, `*`)
"%%.rv" <- function(e1, e2) combine(e1, e2, `%%`)
"%/%.rv" <- function(e1, e2) combine(e1, e2, `%/%`)
"/.rv" <- function(e1, e2) combine(e1, e2, `/`)
"^.rv" <- function(e1, e2) combine(e1, e2, `^`)
"<.rv" <- function(e1, e2) combine(e1, e2, `<`)
"<=.rv" <- function(e1, e2) combine(e1, e2, `<=`)
">.rv" <- function(e1, e2) combine(e1, e2, `>`)
">=.rv" <- function(e1, e2) combine(e1, e2, `>=`)
"==.rv" <- function(e1, e2) combine(e1, e2, `==`)
"!=.rv" <- function(e1, e2) combine(e1, e2, `!=`)
"&.rv" <- function(e1, e2) combine(e1, e2, `&`)
"|.rv" <- function(e1, e2) combine(e1, e2, `|`)


rif <- function(x, yes, no) {
  stopifnot(is.rv(x))
  stopifnot(is.numeric(yes), length(yes) == 1)
  stopifnot(is.numeric(no), length(no) == 1)

  rv(c(no, yes), probs(x))
}
