# utils

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL




#' Pipe-friendly gsub
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' @param pattern character string containing a regular expression (or character
#'   string for fixed = TRUE) to be matched in the given character vector.
#'   Coerced by as.character to a character string if possible. If a character
#'   vector of length 2 or more is supplied, the first element is used with a
#'   warning. Missing values are allowed except for regexpr, gregexpr and
#'   regexec.
#' @param replacement a replacement for matched pattern in sub and gsub. Coerced
#'   to character if possible. If a character vector of length 2 or more is
#'   supplied, the first element is used with a warning. If NA, all elements in
#'   the result corresponding to matches will be set to NA.
ggsub <- function(x, pattern, replacement) {
  return(gsub(pattern, replacement, x))
}
