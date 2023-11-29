## ----echo=FALSE---------------------------------------------------------------
# setup for tex documents
# remove ## in front of output
# adjust spacing before and after code chunks
if(knitr::is_latex_output()) {
  hook_output <- knitr::knit_hooks$get("output")
  hook_warning <- knitr::knit_hooks$get("warning")
  
  # remove ##, wrap long output lines
  knitr::knit_hooks$set(output = function(x, options) {
    x <- knitr:::split_lines(x)
    # x <- gsub("^## ", "#| ", x)
    if (any(nchar(x) > 70)) x <- strwrap(x, width = 70)
    x <- paste(x, collapse = "\n")
    hook_output(x, options)
    
  }, warning = function(x, options) {
    x <- knitr:::split_lines(x)
    x <- gsub("^## ", "#! ", x)
    if (any(nchar(x) > 70)) x <- strwrap(x, width = 70)
    x <- paste(x, collapse = "\n")
    hook_warning(x, options)
  })
  
  # adjust spacing around code chunks
  oldSource <- knitr::knit_hooks$get("source")
  knitr::knit_hooks$set(source = function(x, options) {
    x <- oldSource(x, options)
  
    if(options$echo == FALSE)
      return(x)
  
    paste0(
      '\\vspace{10pt}',
      x,
      '\n\\vspace{-10pt}'
    )
  })
}

examples <- list()
exampleCounter <- function(id) {
  if(missing(id)) {
    examples[[length(examples) + 1]] <- -1
  
  } else if(id %in% names(examples)) {
    stop(paste("id", id, "is already in examples"))
  } else {
    examples[[id]] <<- length(examples) + 1
  }
  return(length(examples))
}

definitions <- list()
definitionCounter <- function(id) {
  if(missing(id)) {
    definitions[[length(definitions)+1]] <<- length(definitions) + 1
    
  } else if(id %in% names(examples)) {
    stop(paste("id", id, "is already in examples"))
  } else {
    definitions[[id]] <<- length(definitions) + 1
  }
  length(definitions)
}

refId <- function(id, htmlTemplate = "[ID](ID)") {
  if(knitr::is_latex_output())
    paste0("\\ref{", id, "}")
  else
    gsub("ID", paste0("#", id), htmlTemplate)
}

refDef <- function(def) {
  if(!(def %in% names(definitions)))
    stop(paste0("Definition ", def, " is not defined"))
  
  if(knitr::is_latex_output())
    paste0("\\ref{", def, "}")
  else
    paste0("[", definitions[[def]], "](#", def, ")")
}

## -----------------------------------------------------------------------------
library(socialranking)
PowerRelation(list(list(c(1,2)), list(1, c()), list(2)))

as.PowerRelation("12 > 1 ~ {} > 2")

as.PowerRelation("ab > a ~ {} > b")

as.PowerRelation(list(c(1,2), 1, c(), 2))

as.PowerRelation(list(c(1,2), 1, c(), 2), comparators = c(">", "~", ">"))

## ---- echo=FALSE, results='asis'----------------------------------------------
xfun::file_string("tables/functionTable.tex")

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("ab > abc ~ ac ~ bc > a ~ c > {} > b")

# a dominates b, but b does not dominate a
c(dominates(pr, "a", "b"),
  dominates(pr, "b", "a"))

# calculate cumulative scores
scores <- cumulativeScores(pr)
# show score of element a
scores$a

# performing a bunch of rankings
lexcelRanking(pr)
L1Ranking(pr)
dualLexcelRanking(pr)
copelandRanking(pr)
kramerSimpsonRanking(pr)
ordinalBanzhafRanking(pr)

## -----------------------------------------------------------------------------
rel <- relations::as.relation(pr)
rel

relations::relation_incidence(rel)

## -----------------------------------------------------------------------------
library(socialranking)
pr <- PowerRelation(list(
  list(c(1,2)),
  list(2, c()),
  list(1)
))
pr

class(pr)

## -----------------------------------------------------------------------------
as.PowerRelation("12 > 2~{} > 1")

## -----------------------------------------------------------------------------
prLong <- PowerRelation(list(
  list(c("Alice", "Bob")), 
  list("Bob", c()),
  list("Alice")
))
prLong

class(prLong)

## -----------------------------------------------------------------------------
class(pr) <- class(pr)[-which(class(pr) == "SingleCharElements")]
pr

## ---- echo=FALSE, results='asis'----------------------------------------------
xfun::file_string('tables/prObject.tex')

## -----------------------------------------------------------------------------
prAtts <- PowerRelation(list(
  list(c(2,2,1,1,2)),
  list(c(2,1), c())
))
prAtts

prAtts$elements

prAtts$coalitionLookup(c(1,2))
prAtts$coalitionLookup(c(2,1))
prAtts$coalitionLookup(c(2,1,2,1,2))

prAtts$elementLookup(2)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("12 > (1 ~ {}) > 2")
PowerRelation(pr$eqs[c(2, 3, 1)])

PowerRelation(rev(pr$eqs))

## -----------------------------------------------------------------------------
coalitions <- unlist(pr$eqs, recursive = FALSE)
compares <- c(">", "~", ">")
as.PowerRelation(coalitions[c(2,1,3,4)], comparators = compares)

# notice that the length of comparators does not need to match
# length(coalitions)-1
as.PowerRelation(rev(coalitions), comparators = c("~", ">"))

# not setting the comparators parameter turns it into a linear order
as.PowerRelation(coalitions)

## -----------------------------------------------------------------------------
pr <- PowerRelation(list(
  list(c("AT", "DE"), "FR"),
  list("DE"),
  list(c("AT", "FR"), "AT")
))
pr

# since we have 3 elements, the super set 2^N should include 8 coalitions
appendMissingCoalitions(pr)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("a > b > c ~ ac > abc")
makePowerRelationMonotonic(pr)

makePowerRelationMonotonic(pr, addMissingCoalitions = FALSE)

# notice how an empty coalition in some equivalence class
# causes all remaining coalitions to be moved there
makePowerRelationMonotonic(as.PowerRelation("ab > c > {} > abc > a > b"))

## -----------------------------------------------------------------------------
createPowerset(
  c("a", "b", "c"),
  result = "print"
)

## -----------------------------------------------------------------------------
ps <- createPowerset(1:2, includeEmptySet = FALSE)
ps

as.PowerRelation(ps)

# equivalent
PowerRelation(list(ps))

as.PowerRelation(createPowerset(letters[1:4]))

## -----------------------------------------------------------------------------
coalitions <- list(c(1,2), 1, 2)
gen <- powerRelationGenerator(coalitions)
while(!is.null(pr <- gen())) {
  print(pr)
}

## -----------------------------------------------------------------------------
gen <- powerRelationGenerator(coalitions, startWithLinearOrder = TRUE)
while(!is.null(pr <- gen())) {
  print(pr)
}

## -----------------------------------------------------------------------------
gen <- powerRelationGenerator(coalitions)
# partition 3

gen <- generateNextPartition(gen)
# partition 2+1

gen <- generateNextPartition(gen)
# partition 1+2
gen()

## ----echo=FALSE---------------------------------------------------------------
stirlingSecond <- function(n, k) {
  s <- sapply(0:k, function(j) (-1)^j * choose(k, j) * (k - j)^n)
  sum(s) / factorial(k)
}
bellNum <- function(n) sapply(0:n, stirlingSecond, n = n) |> sum()
preorderNum <- function(x) sapply(0:x, function(k) factorial(k) * stirlingSecond(x,k)) |> sum()
# for(i in 1:10) writeLines(paste('|', i, '|', bellNum(i), '|', preorderNum(i), '|'))

## -----------------------------------------------------------------------------
# we define some arbitrary score vector where "a" scores highest.
# "b" and "c" both score 1, thus they are indifferent.
scores <- c(a = 100, b = 1, c = 1)
doRanking(scores)

# we can also tell doRanking to punish higher scores
doRanking(scores, decreasing = FALSE)

## -----------------------------------------------------------------------------
scores <- list(a = c(3, 3, 3), b = c(2, 3, 2), c = c(7, 0, 2))
doRanking(scores, compare = function(a, b) sum(a) - sum(b))
# a and c are considered to be indifferent, because their sums are the same

doRanking(scores, compare = function(a,b) sum(a) - sum(b), decreasing = FALSE)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("3 > 1 > 2 > 12 > 13 > 23")

# 1 clearly dominates 2
dominates(pr, 1, 2)
dominates(pr, 2, 1)

# 3 does not dominate 1, nor does 1 dominate 3, because
# {}u3 > {}u1, but 2u1 > 2u3
dominates(pr, 1, 3)
dominates(pr, 3, 1)

# an element i dominates itself, but it does not strictly dominate itself
# because there is no Sui > Sui
dominates(pr, 1, 1)
dominates(pr, 1, 1, strictly = TRUE)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("ac > bc ~ b > a ~ abc > ab")

# FALSE because ac > bc, whereas b > a
dominates(pr, "a", "b")

# TRUE because ac > bc, ignoring b > a comparison
dominates(pr, "a", "b", includeEmptySet = FALSE)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("ab > (ac ~ bc) > (a ~ c) > {} > b")
cumulativeScores(pr)

# for each index k, $a[k] >= $b[k]
cumulativelyDominates(pr, "a", "b")

# $a[3] > $b[3], therefore a also strictly dominates b
cumulativelyDominates(pr, "a", "b", strictly = TRUE)

# $b[1] > $c[1], but $c[3] > $b[3]
# therefore neither b nor c dominate each other
cumulativelyDominates(pr, "b", "c")
cumulativelyDominates(pr, "c", "b")

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("ab > (ac ~ bc) > (a ~ c) > {} > b")
cpMajorityComparisonScore(pr, "a", "b")

cpMajorityComparisonScore(pr, "b", "a")

if(sum(cpMajorityComparisonScore(pr, "a", "b")) >= 0) {
  print("a >= b")
} else {
  print("b > a")
}

## -----------------------------------------------------------------------------
# Now (ac ~ bc) is not counted
cpMajorityComparisonScore(pr, "a", "b", strictly = TRUE)

# Notice that the sum is still the same
sum(cpMajorityComparisonScore(pr, "a", "b", strictly = FALSE)) ==
  sum(cpMajorityComparisonScore(pr, "a", "b", strictly = TRUE))

## -----------------------------------------------------------------------------
# extract more information in cpMajorityComparison
cpMajorityComparison(pr, "a", "b")

# with strictly set to TRUE, coalition c does
# neither appear in D_ab nor in D_ba
cpMajorityComparison(pr, "a", "b", strictly = TRUE)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation(list(c(1,2), c(1), c(2)))
pr

# both players 1 and 2 have an Ordinal Banzhaf Score of 1
# therefore they are indifferent to one another
# note that the empty set is missing, as such we cannot compare {}u{i} with {}
ordinalBanzhafScores(pr)

ordinalBanzhafRanking(pr)

pr <- as.PowerRelation("ab > a > {} > b")

# player b has a negative impact on the empty set
# -> player b's score is 1 - 1 = 0
# -> player a's score is 2 - 0 = 2
sapply(ordinalBanzhafScores(pr), function(score) sum(score[c(1,2)]))

ordinalBanzhafRanking(pr)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("(abc ~ ab ~ c ~ a) > (b ~ bc) > ac")
scores <- copelandScores(pr)

# Based on CP-Majority, a>=b and a>=c (+2), but b>=a (-1)
scores$a

sapply(copelandScores(pr), sum)

copelandRanking(pr)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("(abc ~ ab ~ c ~ a) > (b ~ bc) > ac")
kramerSimpsonScores(pr)

kramerSimpsonRanking(pr)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("12 > (123 ~ 23 ~ 3) > (1 ~ 2) > 13")

# show the number of times an element appears in each equivalence class
# e.g. 3 appears 3 times in [[2]] and 1 time in [[4]]
lapply(pr$equivalenceClasses, unlist)

lexScores <- lexcelScores(pr)
for(i in names(lexScores))
  paste0("Lexcel score of element ", i, ": ", lexScores[i])

# at index 1, element 2 ranks higher than 3
lexScores['2'] > lexScores['3']

# at index 2, element 2 ranks higher than 1
lexScores['2'] > lexScores['1']

lexcelRanking(pr)

## -----------------------------------------------------------------------------
lexcelCumulated <- lapply(lexScores, cumsum)
cumulScores <- cumulativeScores(pr)

paste0(names(lexcelCumulated), ": ", lexcelCumulated, collapse = ', ')
paste0(names(cumulScores), ": ", cumulScores, collapse = ', ')

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("12 > (123 ~ 23 ~ 3) > (1 ~ 2) > 13")

lexScores <- lexcelScores(pr)

# in regular Lexcel, 1 scores higher than 3
lexScores['1'] > lexScores['3']

# turn Lexcel score into Dual Lexcel score
dualLexScores <- structure(
  lapply(lexcelScores(pr), function(r) -rev(r)),
  class = 'LexcelScores'
)

# now 1 scores lower than 3
dualLexScores['1'] > dualLexScores['3']

# element 2 comes out at the top in both Lexcel and Dual Lexcel
lexcelRanking(pr)

dualLexcelRanking(pr)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation('(12 ~ 1 ~ 23) > 123 > {} > (13 ~ 2 ~ 3)')
L1Scores(pr)

## -----------------------------------------------------------------------------
L1Ranking(pr)

## -----------------------------------------------------------------------------
L2Ranking(pr)
pr2 <- as.PowerRelation('1 ~ 23 ~ 24 ~ 234')
pr2 <- appendMissingCoalitions(pr2)
L1Ranking(pr)
L2Ranking(pr)

## -----------------------------------------------------------------------------
LPScores(pr)
LPRanking(pr)

## -----------------------------------------------------------------------------
L1Scores(pr)
LPSScores(pr)
LPSRanking(pr)

## -----------------------------------------------------------------------------
pr <- as.PowerRelation("ab > a > {} > b")
rel <- relations::as.relation(pr)

relations::relation_incidence(rel)

c(
  relations::relation_is_acyclic(rel),
  relations::relation_is_antisymmetric(rel),
  relations::relation_is_linear_order(rel),
  relations::relation_is_complete(rel),
  relations::relation_is_reflexive(rel),
  relations::relation_is_transitive(rel)
)

## -----------------------------------------------------------------------------
# a power relation where coalitions {1} and {2} are indifferent
pr <- as.PowerRelation("12 > (1 ~ 2)")
rel <- relations::as.relation(pr)

# we have both binary relations {1}R{2} as well as {2}R{1}
relations::relation_incidence(rel)

# FALSE
c(
  relations::relation_is_acyclic(rel),
  relations::relation_is_antisymmetric(rel),
  relations::relation_is_linear_order(rel),
  relations::relation_is_complete(rel),
  relations::relation_is_reflexive(rel),
  relations::relation_is_transitive(rel)
)

## -----------------------------------------------------------------------------
as.PowerRelation("12 > 2 > (1 ~ 2) > 12")

## -----------------------------------------------------------------------------
pr <- suppressWarnings(as.PowerRelation(list(1, 2, 1)))
pr

transitiveClosure(pr)

# two cycles, (1>3>1) and (2>23>2)
pr <- suppressWarnings(
  as.PowerRelation("1 > 3 > 1 > 2 > 23 > 2")
)

transitiveClosure(pr)

# overlapping cycles
pr <- suppressWarnings(
  as.PowerRelation("c > ac > b > ac > (a ~ b) > abc")
)

transitiveClosure(pr)

