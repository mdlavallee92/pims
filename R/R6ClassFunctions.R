.setNumber <- function(private, key, value, nullable = FALSE) {
  checkmate::assert_numeric(x = value, null.ok = nullable)
  private[[key]] <- value
  invisible(private)
}

.setString <- function(private, key, value, naOk = FALSE) {
  checkmate::assert_string(x = value, na.ok = naOk, min.chars = 1, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

.getIncidenceRate <- function(){

}
