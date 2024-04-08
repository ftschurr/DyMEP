
#' A function to get all potential permutations
#'
#'@param your_environmental_covariates list of the environmental covariates you
#' have at hand (tas, tasmax, tasmin, RH, VPD, SPI, global_radiation are
#'  available)
#' @keywords internal
#' @export
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
