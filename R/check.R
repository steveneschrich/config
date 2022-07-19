#' Verify config contains all reference entries
#'
#' @param config A configuration object (list)
#' @param reference A reference configuration object (list)
#'
#' @return TRUE if all reference entries are in the config list, FALSE otherwise.
#' @export
#'
#' @examples
#'
check <- function(config, reference) {
  all(names(unlist(reference)) %in% names(unlist(config)))
}
