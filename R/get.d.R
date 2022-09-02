#' Get config from multiple individual files
#'
#' @description This uses the linux approach of a `config.d` type directory with individual configs to load
#' and merge.
#'
#' @param dir The directory to use for config files.
#' @param pattern (default *.yml) File pattern for config files.
#' @param ... Any other parameters to pass to [config::get()].
#'
#' @return A configuration list (see [config::get()]).
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' conf <- get.d(dir = "config.d")
#' }
get.d <- function(dir, pattern="*.yml", ...) {

  # TODO: This needs to be more defensive. No files found, etc.

  # Load all configuration
  # We are trying to use package config, which does allow a merge of configuration options.
  conf <- list.files(dir, pattern=pattern, full.names=TRUE) |>
    purrr::map(~get(file=., ...)) |>
    purrr::reduce(merge)

  conf
}
