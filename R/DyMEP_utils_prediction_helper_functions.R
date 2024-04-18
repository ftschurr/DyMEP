
#' A function to get all potential permutations
#'
#'@param your_environmental_covariates list of the environmental covariates you
#' have at hand (tas, tasmax, tasmin, RH, VPD, SPI, global_radiation are
#'  available)
#' @keywords internal
#' @export
#' @return provides all potential permutations given your input vector
#' @examples
#' predhelputils.get_all_permutations(your_environmental_covariates =
#'                                       c("tas","tasmin","VPD"))
predhelputils.get_all_permutations <- function(your_environmental_covariates){
  # Initialize an empty vector to store all permutations
  all_permutations <- NULL
  # Generate permutations for subsets of different lengths

  for (len in 1:length(your_environmental_covariates)) {
    perms <- permutations_fun(
      n = length(your_environmental_covariates),
      r = len,
      v = your_environmental_covariates
    )
    perms <- apply(perms, 1, paste, collapse = "_")

    all_permutations <- c(all_permutations, perms)
    # Convert permutations to a character vector

    # Add the permutations to the result vector
    # all_permutations <- c(all_permutations, perms)
  }
  return(all_permutations)

}


permutations_fun <- function (n,
                              r,
                              v = 1:n,
                              set = TRUE,
                              repeats.allowed = FALSE)
{
  if (mode(n) != "numeric" || length(n) != 1 || n < 1 || (n%%1) !=
      0)
    stop("bad value of n")
  if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) !=
      0)
    stop("bad value of r")
  if (!is.atomic(v) || length(v) < n)
    stop("v is either non-atomic or too short")
  if ((r > n) & repeats.allowed == FALSE)
    stop("r > n and repeats.allowed=FALSE")
  if (set) {
    v <- unique(sort(v))
    if (length(v) < n)
      stop("too few different elements")
  }
  v0 <- vector(mode(v), 0)
  if (repeats.allowed)
    sub <- function(n, r, v) {
      if (r == 1)
        matrix(v, n, 1)
      else if (n == 1)
        matrix(v, 1, r)
      else {
        inner <- Recall(n, r - 1, v)
        cbind(rep(v, rep(nrow(inner), n)), matrix(t(inner),
                                                  ncol = ncol(inner),
                                                  nrow = nrow(inner) * n,
                                                  byrow = TRUE))
      }
    }
  else sub <- function(n, r, v) {
    if (r == 1)
      matrix(v, n, 1)
    else if (n == 1)
      matrix(v, 1, r)
    else {
      X <- NULL
      for (i in 1:n) X <- rbind(X, cbind(v[i], Recall(n -
                                                        1, r - 1, v[-i])))
      X
    }
  }
  sub(n, r, v[1:n])
}



#' A function to download additional crop parameters
#'
#'@param external_params_path path where additional crop parameters should be
#' stored if not possible to download in to the regular R repository. The
#' default is NULL, which will use the regular R repository as path
#' @importFrom utils download.file
#' @importFrom utils unzip
#' @keywords internal
#' @return no return
#' @export
get_parameters <- function(external_params_path = NULL){
  download_link <- "https://github.com/ftschurr/DyMEPparameter/archive/refs/heads/main.zip"

  path_params_dymep <- file.path(system.file(package = "DyMEP"),"extdata")

  if(!is.null(external_params_path)){
    stop(cat("please provide an external_params_path, if you dont have
             parameters of multiple crops here: ",path_params_dymep))
  }

  # download file
  try_output <- try(download.file(download_link,file.path(external_params_path,
                                                        "DyMEPparameter.zip")))

  if(try_output != 0){
    stop("We tried to download remainign parameter infromation to: ",
         external_params_path, " . However, we do not have permission to write
        files here. Please either open the permissions for this folder, download
        yourself the parameter from: https://github.com/ftschurr/DyMEPparameter/archive/refs/heads/main.zip
        into this directory. Or as a last option provide via the
        'external_params_path' argument a path where the parameters should be
        stored. Please always put this infromation for further usage.
        Sorry for this inconvenience, there are very strict size restriction in
        place for R packages (: ")

  }
  # unzip file

  unzip(file.path(external_params_path,"DyMEPparameter.zip"), exdir = external_params_path)
  file_dest <- file.path(external_params_path,"pmem")
  dir.create(file_dest,recursive = TRUE, showWarnings = FALSE)
  crops <- list.dirs(file.path(external_params_path,"DyMEPparameter-main",
                               "extdata", "pmem"), full.names = FALSE)[-1]
  # copy to the correct place
  for(cr in crops){
    momentary_path <- file.path(external_params_path,"DyMEPparameter-main",
                                "extdata", "pmem",cr)
    file.copy(momentary_path,file_dest,recursive = TRUE)

  }

  # delte
  unlink(file.path(external_params_path,"DyMEPparameter-main"), recursive=TRUE)
  file.remove(file.path(external_params_path,"DyMEPparameter.zip"))
}


