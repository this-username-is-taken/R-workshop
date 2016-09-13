col_means <- function(df) {
  if (!is.data.frame(df)) {
    df <- as.data.frame(df)
  }
  
  numeric <- vapply(df, is.numeric, logical(1))
  numeric_cols <- df[, numeric, drop = FALSE]
  
  data.frame(lapply(numeric_cols, mean))
}

col_means(mtcars)
col_means(mtcars[, 0])
col_means(mtcars[0, ])
col_means(mtcars[, "mpg", drop = F])
col_means(1:10)
col_means(as.matrix(mtcars))
# col_means(as.list(mtcars))
# 
# mtcars2 <- mtcars
# mtcars2[-1] <- lapply(mtcars2[-1], as.character)
# col_means(mtcars2)


col_summarise <- function(df, f, ...) {
  if (!is.data.frame(df)) {
    df <- as.data.frame(df)
  }
  
  numeric <- vapply(df, is.numeric, logical(1))
  numeric_cols <- df[, numeric, drop = FALSE]
  
  data.frame(lapply(numeric_cols, f, ...))
}

col_summarise(mtcars, mean)
col_summarise(mtcars, median)
col_summarise(mtcars, sd)
