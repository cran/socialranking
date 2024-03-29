% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createPowerset.R
\name{createPowerset}
\alias{createPowerset}
\title{Create powerset}
\usage{
createPowerset(
  elements,
  includeEmptySet = TRUE,
  result = c("return", "print", "copy", "printCompact", "copyCompact")
)
}
\arguments{
\item{elements}{vector of elements}

\item{includeEmptySet}{If \code{TRUE}, an empty vector is added at the end}

\item{result}{What to do with the result. Can be either:
\itemize{
\item \code{"return"}: return list object
\item \code{"print"}: create valid string to call \code{\link[=PowerRelation]{PowerRelation()}} or \code{\link[=as.PowerRelation]{as.PowerRelation()}} and print it
\item \code{"copy"}: create valid string to call \code{\link[=PowerRelation]{PowerRelation()}} or \code{\link[=as.PowerRelation]{as.PowerRelation()}} and copy it to clipboard
\item \code{"printCompact"} and \code{"copyCompact"}: same as \code{"print"} and \code{"copy"} but without newlines
}}
}
\value{
List of power set vectors.
If the parameter \code{result} is set to \code{"print"} or \code{"copy"}, nothing is returned.
Instead, a character string is generated that can be used in R to call and create a new \code{\link{PowerRelation}} object.
This string is either printed or copied to clipboard (see argument \code{result}).
}
\description{
Given a vector of elements generate a power set.
}
\examples{
# normal return type is a list of vectors
createPowerset(c("Alice", "Bob"), includeEmptySet = FALSE)
## [[1]]
## [1] "Alice" "Bob"
##
## [[2]]
## [1] "Alice"
##
## [[3]]
## [1] "Bob"

# instead of creating a list, print the power set such that it can be copy-pasted
# and used to create a new PowerRelation object
createPowerset(letters[1:4], result = "print")
# prints
# as.PowerRelation("
#   abcd
#   > abc
#   > abd
#   > acd
#   > bcd
#   > ab
#   ...
#   > {}
# ")

createPowerset(letters[1:3], includeEmptySet = FALSE, result = "printCompact")
# as.PowerRelation("abc > ab > ac > bc > a > b > c")

# create the same string as before, but now copy it to the clipboard instead
if(interactive()) {
  createPowerset(1:3, result = "copyCompact")
}

# Note that as.PowerRelation(character) only assumes single-char elements.
# As such, the generated function call string with multi-character names
# looks a little different.
createPowerset(c("Alice", "Bob"), result = "print")
# PowerRelation(rlang::list2(
#   list(c("Alice", "Bob")),
#   list(c("Alice")),
#   list(c("Bob")),
#   list(c()),
# ))

}
