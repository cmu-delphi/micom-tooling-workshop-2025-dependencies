#' @importFrom tibble tibble
required_pkgs <- tibble::tribble(
  ~ package, ~ op, ~ req_version,
  "cowplot", NA, NA,
  "epidatasets", ">=", "0.0.1",
  "epidatr", ">=", "1.2.0",
  "epipredict", "==", "0.1.0",
  "epiprocess", "==", "0.9.6",
  "glmnet", NA, NA,
  "here", NA, NA,
  "httr", NA, NA,
  "knitr", NA, NA,
  "quantreg", NA, NA,
  "quarto", NA, NA,
  "readr", NA, NA,
  "svglite", NA, NA,
  "tidymodels", NA, NA,
  "tidyverse", NA, NA,
  "zoo", NA, NA
)



#' Verify system setup
#'
#' Call this function to ensure that all necessary packages are installed.
#'
#' @return Returns `TRUE` if assigned. Otherwise produces status messages.
#' @export
verify_setup <- function() {
  statuses <- pak::pkg_status(required_pkgs$package) |>
    dplyr::select(package, version)
  statuses <- dplyr::right_join(statuses, required_pkgs, by = "package")
  not_installed <- is.na(statuses$version)
  if (any(not_installed)) {
    pkg_not_installed <- statuses$package[not_installed]
    cli::cli_abort(
      "The following required packages are not installed: {.pkg {pkg_not_installed}}."
    )
  }
  statuses <- dplyr::filter(statuses, !is.na(req_version))
  version_match <- logical(nrow(statuses))
  for (ii in seq(nrow(statuses))) {
    version_match[ii] <- with(statuses[ii, ], get(op)(version, req_version))
  }
  if (any(!version_match)) {
    statuses <- statuses[!version_match, ]
    versions <- paste(statuses$package, statuses$version)
    req_versions <- paste(statuses$package, statuses$op, statuses$req_version)
    cli::cli_abort(c(
      "The following packages do not have the correct version:",
      "i" = "Installed: {.pkg {versions}}.",
      "i" = "Required: {.pkg {req_versions}}."
    ))
  }
  cli::cli_alert_success("You should be good to go!")
  invisible(TRUE)
}
