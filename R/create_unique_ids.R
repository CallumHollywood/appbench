

#' create_unique_ids
#'
#' @description create unique ids with a combo of alpha and numeric characters
#'
#' @param n integer
#' @param char_len integer
#'
#' @return character string
#' @export
#'

create_unique_ids <- function(n = 1, char_len = 5){

  pool <- c(letters, LETTERS, 0:9)

  res <- character(n)
  for(i in seq(n)){

    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    # as long as n = 1 then wont need to worry about dupicates (like in 'while')
    # though !!! there should be a duplicate check, after prefixing username[1:4]
    # in write RDS and write to DB, with a rerun of create_unique_ids if duplicate
    # is found already on on file
    res[i] <- this_res
  }
  res
}
