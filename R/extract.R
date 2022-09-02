#' Extract element from config list (any level)
#'
#' @description Extract a value (can be a value or list) from a configuration list named by `key`.
#'
#' @details
#' With complex configurations, extracting a value from the configuration becomes a
#' challenge. There isn't a great general-purpose way that I could find for extracting a
#' list/value at an arbitrary depth of configuration. This function performs that function.
#'
#' Do note that a list is not guaranteed to have unique names at different levels. This function
#' will return the object found at the shallowest level (from the initial object). If multiple
#' such matches are found, this will be noted but ignored.
#'
#' Note that the function returns NULL if the key is not found
#'
#' @param conf A configuration list
#' @param key The key to extract
#'
#' @return A list or value from the configuration object with the name of `key`. NULL if key is not found.
#' @export
#'
#' @examples
#'
#' extract_values(list(l11=list(
#'      l21=list(l31=letters, l32=rev(letters)),
#'      l22=toupper(letters)),
#'   l12=mtcars$mpg)
#' )
extract_config <- function(conf, key) {
  purrr::pluck(conf, function(x) { key_finder(x, key = key)})
}



key_finder <- function(x, key) {

  # Base cases:
  #  - we found our key among the names at this level
  #  - if not the key, we are atomic so cannot explore further.
  if (key %in% names(x)) {
    return(x[[which(names(x) %in% key)]])
  } else if ( is.atomic(x)) {
    return(NULL)
  }

  # Default case, we recursively explore the list children looking for our key.
  # Note that this works because children can return NULL when unsuccessful at
  # each level. So compact will remove all the NULL's at the top level and return
  # the first of what's left over. It can be a NULL as well, meaning we've flattened
  # everything below this point to a single value.
  v <- purrr::map(x, key_finder, key=key) |>
    purrr::compact()

  # Could be empty, special-case the return
  if ( length(v) < 1 ) return(NULL)
  # Could be multiple matches (at different levels of hierarchy). Warn if this happens.
  if ( length(v) > 1) {
    cli::cli_warn("Configuration contains multiple entries for key {key}. The first match will be returned.")
  }

  # We return the first element of mapped list (which is either NULL or an object based at the key's name).
  v[[1]]

}

