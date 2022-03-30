#' Test relation between two elements
#'
#' On a given `PowerRelation` object `pr`, check if `e1` relates to `e2` based on the given social ranking solution.
#'
#' The function `testRelation` is somewhat only used to make the offered comparison operators in the package better discoverable.
#'
#' `testRelation(pr, e1)` is equivalent to `pr %:% e1` and `list(pr, e1)`. It should be used together with one of the
#' comparison operators listed in the usage section.
#'
#' @template param/powerRelation
#' @template param/e1and2
#' @param pr_e1 `PowerRelation` and `e1` element, packed into a list using `pr %:% e1`
#'
#' @return `testRelation()` and `%:%` returns `list(powerRelation, e1)`.
#'
#' Followed by a `%>=comparison%` or `%>comparison%` it returns `TRUE` or `FALSE`, depending on the relation between
#' `e1` and `e2`.
#'
#' @seealso Comparison function: [`dominates()`], [`cumulativelyDominates()`], [`cpMajorityComparison()`].
#'
#' Score Functions: [`ordinalBanzhafScores()`], [`copelandScores()`], [`kramerSimpsonScores()`], [`lexcelScores()`].
#'
#' @examples
#' pr <- newPowerRelationFromString(
#'   "123 > 12 ~ 13 > 3 > 1 ~ 2", asWhat = as.numeric
#' )
#'
#' @export
testRelation <- function(powerRelation, e1) {
  list(powerRelation, e1)
}


#' @rdname testRelation
#' @export
`%:%` <- function(powerRelation, e1) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  stopifnot(e1 %in% powerRelation$elements)
  stopifnot(class(e1) == class(powerRelation$elements))
  # --- end checks --- #

  list(powerRelation, e1)
}

#' @rdname testRelation
#' @examples
#' # Dominance
#' stopifnot(pr %:% 1 %>=dom% 2)
#'
#' @export
`%>=dom%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #
  dominates(powerRelation, e1, e2, strictly = FALSE)
}

#' @rdname testRelation
#' @examples
#' # Strict dominance
#' stopifnot((pr %:% 1 %>dom% 2) == FALSE)
#'
#' @export
`%>dom%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #
  dominates(powerRelation, e1, e2, strictly = TRUE)
}


#' @rdname testRelation
#' @examples
#' # Cumulative dominance
#' stopifnot(pr %:% 1 %>=cumuldom% 2)
#'
#' @export
`%>=cumuldom%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #
  cumulativelyDominates(powerRelation, e1, e2, strictly = FALSE)
}


#' @rdname testRelation
#' @examples
#' # Strict cumulative dominance
#' stopifnot(pr %:% 1 %>cumuldom% 2)
#'
#' @export
`%>cumuldom%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #
  cumulativelyDominates(powerRelation, e1, e2, strictly = TRUE)
}


#' @rdname testRelation
#' @examples
#' # CP-Majority relation
#' stopifnot(pr %:% 1 %>=cp% 2)
#'
#' @export
`%>=cp%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  sum(cpMajorityComparisonScore(powerRelation, e1, e2)) >= 0
}

#' @rdname testRelation
#' @examples
#' # Strict CP-Majority relation
#' stopifnot((pr %:% 1 %>cp% 2) == FALSE)
#'
#' @export
`%>cp%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  sum(cpMajorityComparisonScore(powerRelation, e1, e2)) > 0
}

#' @rdname testRelation
#' @examples
#' # Ordinal banzhaf relation
#' stopifnot(pr %:% 1 %>=banz% 2)
#'
#' @export
`%>=banz%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- ordinalBanzhafScores(powerRelation)
  scores[paste(e1)] > scores[paste(e2)] || scores[paste(e1)] == scores[paste(e2)]
}

#' @rdname testRelation
#' @examples
#' # Strict ordinal banzhaf relation
#' # (meaning 1 had a strictly higher positive contribution than 2)
#' stopifnot((pr %:% 1 %>banz% 2) == FALSE)
#'
#' @export
`%>banz%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- ordinalBanzhafScores(powerRelation)
  scores[paste(e1)] > scores[paste(e2)]
}

#' @rdname testRelation
#' @examples
#' # Copeland-like method
#' stopifnot(pr %:% 1 %>=cop% 2)
#' stopifnot(pr %:% 2 %>=cop% 1)
#'
#' @export
`%>=cop%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- copelandScores(powerRelation, c(e1, e2))
  scores[paste(e1)] > scores[paste(e2)] || scores[paste(e1)] == scores[paste(e2)]
}

#' @rdname testRelation
#' @examples
#' # Strict Copeland-like method
#' # (meaning pairwise winning minus pairwise losing comparison of
#' # 1 is strictly higher than of 2)
#' stopifnot((pr %:% 1 %>cop% 2) == FALSE)
#' stopifnot((pr %:% 2 %>cop% 1) == FALSE)
#' stopifnot(pr %:% 3 %>cop% 1)
#'
#' @export
`%>cop%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- copelandScores(powerRelation, c(e1, e2))
  scores[1] > scores[2]
}


#' @rdname testRelation
#' @examples
#' # Kramer-Simpson-like method
#' stopifnot(pr %:% 1 %>=ks% 2)
#' stopifnot(pr %:% 2 %>=ks% 1)
#'
#' @export
`%>=ks%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- kramerSimpsonScores(powerRelation, c(e1, e2))
  scores[paste(e1)] > scores[paste(e2)] || scores[paste(e1)] == scores[paste(e2)]
}

#' @rdname testRelation
#' @examples
#' # Strict Kramer-Simpson-like method
#' # (meaning ks-score of 1 is actually higher than 2)
#' stopifnot((pr %:% 2 %>ks% 1) == FALSE)
#' stopifnot((pr %:% 1 %>ks% 2) == FALSE)
#' stopifnot(pr %:% 3 %>ks% 1)
#'
#' @export
`%>ks%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- kramerSimpsonScores(powerRelation, c(e1, e2))
  scores[paste(e1)] > scores[paste(e2)]
}


#' @rdname testRelation
#' @examples
#' # Lexicographical and dual lexicographical excellence
#' stopifnot(pr %:% 1 %>=lex% 3)
#' stopifnot(pr %:% 3 %>=duallex% 1)
#'
#' # Strict lexicographical and dual lexicographical excellence
#' # (meaning their lexicographical scores don't match)
#' stopifnot(pr %:% 1 %>lex% 3)
#' stopifnot(pr %:% 3 %>duallex% 1)
#'
#' @export
`%>=lex%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- lexcelScores(powerRelation, c(e1, e2))
  scores[paste(e1)] > scores[paste(e2)] || scores[paste(e1)] == scores[paste(e2)]
}

#' @rdname testRelation
#'
#' @export
`%>lex%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- lexcelScores(powerRelation, c(e1, e2))
  scores[paste(e1)] > scores[paste(e2)]
}


#' @rdname testRelation
#'
#' @export
`%>=duallex%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- lexcelScores(powerRelation, c(e1, e2))
  scores <- structure(
    lapply(lexcelScores(powerRelation, c(e1, e2)), function(x) -rev(x)),
    class = class(scores)
  )
  scores[paste(e1)] > scores[paste(e2)] || scores[paste(e1)] == scores[paste(e2)]
}

#' @rdname testRelation
#'
#' @export
`%>duallex%` <- function(pr_e1, e2) {
  # --- checks (generated) --- #
  if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add "%:%" and then test between 2 elements.')
  powerRelation <- pr_e1[[1]]
  e1 <- pr_e1[[2]]
  if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')
  if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  scores <- lexcelScores(powerRelation, c(e1, e2))
  scores <- structure(
    lapply(lexcelScores(powerRelation, c(e1, e2)), function(x) -rev(x)),
    class = class(scores)
  )
  scores[paste(e1)] > scores[paste(e2)]
}