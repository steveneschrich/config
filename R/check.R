#' Verify config contains all reference entries
#'
#' @param config A configuration object (list)
#' @param reference A reference configuration object (list)
#'
#' @return TRUE if all reference entries are in the config list, FALSE otherwise.
#' @export
#'
#' @examples
#'  \dontrun{
#'  check("config_file","reference_file")
#'  }
check <- function(config, reference) {
  all(names(unlist(reference)) %in% names(unlist(config)))
}
