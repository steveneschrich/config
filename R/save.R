#' Save configuration values to file.
#'
#' Save the currently active configuration from a list.
#'
#' For additional details see <https://rstudio.github.io/config/>.
#'
#'
#' @param config_list List of configuration options to be saved to config file.
#' @param file Configuration file to write to (defaults to
#'   `"config.yml"`).
#' @param config Name of configuration profile to save. Defaults to
#'   the value of the `R_CONFIG_ACTIVE` environment variable
#'   ("default" if the variable does not exist).
#' @param overwrite Should an existing config file be overwritten (default is FALSE)
#'
#' @return None (invisible NULL) as returned from [base::cat()].
#'
#'
#' @export
save <- function(config_list = list(),
                 file = "config.yml",
                 config =  Sys.getenv("R_CONFIG_ACTIVE", "default"),
                 overwrite = FALSE) {
  file <- normalizePath(file, mustWork = FALSE)

  # check for file existence
  if (file.exists(file) && !overwrite) {
    stop("Existing config file ", basename(file), " found in current working ",
         "directory.")
  }
  # config_list is wrapped in the config indicator
  config_list <- list(config_list)
  names(config_list)<-config

  # save to yaml
  yaml::write_yaml(config_list, file = file)

}
