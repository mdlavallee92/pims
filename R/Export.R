#' @export
#'
computeCiPrev <- function(result){
  out <- result |>
    mutate(
      se = sqrt((prev/1e5) * (1-prev/1e5)/denominator) * 1e5,
      ci_lower = prev - 1.96 * se,
      ci_upper = prev + 1.96 * se
      )
  return(out)
}

#' @export
#'
computeCiInc <- function(result){
  out <- result |>
    mutate(
      se = sqrt((ir/1e5) * (1-ir/1e5)/denom) * 1e5,
      ci_lower = ir - 1.96 * se,
      ci_upper = ir + 1.96 * se
    )
  return(out)
}

#' @export
#'
exportResult <- function(result, filePath){
  write.csv(result,
            file = filePath,
            quote = FALSE,
            row.names = FALSE)
}
