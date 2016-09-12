# Simple constructor ----------------------------------------------------------

rv <- function(x, probs = NULL) {
  if (is.null(probs)) {
    probs <- rep(1, length(x)) / length(x)
  }
  structure(x, probs = probs, class = "rv")
}


# Safer constructor ------------------------------------------------------------

rv <- function(x, probs = NULL) {
  if (is.null(probs)) {
    probs <- rep(1, length(x)) / length(x)
  } else {
    if (length(x) != length(probs)) 
      stop("`x` and `probs` must be same length")
    check_probs(probs)
  }
  
  # Simplify by summing probabilities with equal x's. Need to use 
  # addNA since otherwise tapply silently drops groups with missing values
  grp <- addNA(x, ifany = TRUE)
  x_new <- as.vector(tapply(x, grp, "[", 1))
  probs <- as.vector(tapply(probs, grp, sum))
  
  # Set probs and class attributes
  structure(x_new, probs = probs, class = "rv")
}

check_probs <- function(x) {
  if (!is.numeric(x)) {
    stop("`prob` must be numeric.")
  }
  if (any(is.na(x))) {
    stop("`prob` must not contain any NA")
  }
  if (any(x < 0)) {
    stop("All `prob` must be >= 0")
  }
  if (abs(sum(x) - 1) > 1e-6) {
    stop("`sum(prob)` must equal 1")
  }
}

# Some helpers -----------------------------------------------------------------

is.rv <- function(x) inherits(x, "rv")

probs <- function(x) attr(x, "probs")
P <- function(x) {
  stopifnot(is.logical(x), is.rv(x))
  sum(probs(x)[x])
}


# Basic methods ----------------------------------------------------------------

print.rv <- function(x, ...) {
  X <- format(x, digits = 3)
  P <- format(probs(x), digits = 3)
  out <- cbind(X = X, "P(X)" = P)
  rownames(out) <- rep("", nrow(out))
  print(out, quote = FALSE)
}

plot.rv <- function(x, ...) {
  name <- deparse(substitute(x))
  ylim <- range(0, probs(x))
  
  plot(as.numeric(x), probs(x), type = "h", ylim = ylim,
    xlab = name, ylab = paste0("P(", name, ")"), ...)
  points(as.numeric(x), probs(x), pch = 20)
  abline(h = 0, col = "gray")  
}

mean.rv <- function(x, ...) {
  sum(x * probs(x))
}

# Methods that use inheritance ------------------------------------------------

`[.rv` <- function(x, i, ...) {
  rv(NextMethod(), prop.table(probs(x)[i]))
}

abs.rv <- function(x)                 rv(NextMethod(), probs(x))
log.rv <- function(x, base = exp(1))  rv(NextMethod(), probs(x))
exp.rv <- function(x)                 rv(NextMethod(), probs(x))
sqrt.rv <- function(x)                rv(NextMethod(), probs(x))

# OR 
Math.rv <- function(x) rv(NextMethod(), probs(x))

# Combine two independent random variables -------------------------------------

as.rv <- function(x) UseMethod("as.rv")
as.rv.rv <- function(x) x
as.rv.numeric <- function(x) rv(x)
as.rv.logical <- function(x) rv(x)

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

"+.rv" <-   function(e1, e2) combine(e1, e2, `+`)
"-.rv" <-   function(e1, e2) combine(e1, e2, `-`)
"/.rv" <-   function(e1, e2) combine(e1, e2, `/`)

# OR
Ops.rv <- function(e1, e2) combine(e1, e2, .Generic)
