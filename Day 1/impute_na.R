# Impute missing values with average of neighbours ------------------------

impute_na1 <- function(x) {
  for (i in 1:length(x - 1)) {
    if (x[i] == "NA") {
      x[i] <- (x[i - 1] + x[i + 1]) / 2
    } else {
      x[i] <- x[i]
    }
  }
  x
}
impute_na1(c(1, 4, 5, "NA", 10, 13, 10))

# Better NA handling ------------------------------------------------------

impute_na2 <- function(x) {
  stopifnot(is.numeric(x))
  
  for (i in 2:(length(x) - 1)) {
    if (is.na(x[i])) {
      x[i] <- (x[i - 1] + x[i + 1]) / 2
    }
  }
  x
}



impute_na2(c(1, 4, 5, "NA", 10, 13, 10))
impute_na2(c(1, 4, 5, NA, 10, 13, 10))

impute_na2(numeric())
impute_na2(NA_real_)

impute_na2(c(NA, 1))
impute_na2(c(1, NA, NA, 2))
impute_na2(c(1, NA))

# Generalise the approach -------------------------------------------------

impute_na3 <- function(x) {
  miss <- is.na(x)
  interp <- approxfun(seq_along(x)[!miss], x[!miss])
  
  x[miss] <- interp(seq_along(x)[miss])
  x
}

impute_na3(c(NA, NA))
impute_na3(c(NA, NA, 1))
impute_na3(c(NA, NA, 1, 2))
