% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lexcel1.R
\name{L1Scores}
\alias{L1Scores}
\alias{L1Ranking}
\alias{lexcel1Scores}
\alias{lexcel1Ranking}
\title{L1 Ranking}
\usage{
L1Scores(powerRelation, elements = powerRelation$elements)

L1Ranking(powerRelation)

lexcel1Scores(powerRelation, elements = powerRelation$elements)

lexcel1Ranking(powerRelation)
}
\arguments{
\item{powerRelation}{A \code{\link{PowerRelation}} object created by \code{\link[=PowerRelation]{PowerRelation()}} or \code{\link[=as.PowerRelation]{as.PowerRelation()}}}

\item{elements}{Vector of elements of which to calculate their scores.
By default, the scores of all elements in \code{powerRelation$elements} are considered.}
}
\value{
Score function returns a list of type \code{L1Scores} and length of \code{powerRelation$elements}
(unless parameter \code{elements} is specified).
Each index contains a vector of length \code{powerRelation$eqs}, the number of
times the given element appears in each equivalence class.

Ranking function returns corresponding \code{\link{SocialRanking}} object.
}
\description{
Calculate the \eqn{L^{(1)}}{L^(1)} scores.
}
\details{
Similar to \code{\link[=lexcelRanking]{lexcelRanking()}}, the number of times an element appears in each equivalence class is counted.
In addition, we now also consider the size of the coalitions.

Let \eqn{N}{N} be a set of elements, \eqn{\succsim \in \mathcal{T}(\mathcal{P})}{>= in T(P)} a power relation,
and \eqn{\Sigma_1 \succ \Sigma_2 \succ \dots \succ \Sigma_m}{E_1 > E_2 > ... > E_m} its corresponding quotient order.

For an element \eqn{i \in N}{i in N}, construct a matrix \eqn{M^\succsim_i}{M^(>=)_i} with \eqn{m}{m} columns and \eqn{|N|}{|N|} rows.
Whereas each column \eqn{q}{q} represents an equivalence class, each row \eqn{p}{p} corresponds to the coalition size.

\deqn{(M^\succsim_i)_{p,q} = |\lbrace S \in \Sigma_q: |S| = p \text{ and } i \in S\rbrace|}{(M^(>=)_i)_pq = |\{S in E_q: |S| = p and i in S\}}

The \eqn{L^{(1)}}{L^(1)} rewards elements that appear in higher ranking coalitions as well as in smaller coalitions.
When comparing two matrices for a power relation, if \eqn{M^\succsim_i >_{L^{(1)}} M^\succsim_j}{M^(>=)_i > M^(>=)_j (the ^(>=) will be omitted to improve readability)},
this suggests that there exists a \eqn{p^0 \in \{1, \dots, |N|\}}{p^0 in \{1, ..., |N|\}} and \eqn{q^0 \in \{1, \dots, m\}}{q^0 in \{1, ..., m\}} such that the following holds:
\enumerate{
\item \eqn{(M^\succsim_i)_{p^0,q^0} > (M^\succsim_j)_{p^0,q^0}}{(M_i)_(p^0 q^0) > (M_j)_(p^0 q^0)}
\item \eqn{(M^\succsim_i)_{p,q^0} = (M^\succsim_j)_{p,q^0}}{(M_i)_(p q^0) = (M_j)_(p q^0)} for all \eqn{p < p^0}{p < p^0}
\item \eqn{(M^\succsim_i)_{p,q} = (M^\succsim_j)_{p,q}}{(M_i)_(pq) = (M_j)_(p,q)} for all \eqn{q < q^0}{q < q^0} and \eqn{p \in \{1, \dots, |N|\}}{p in \{1, ..., |N|\}}
}
}
\section{Example}{

Let \eqn{\succsim: (123 \sim 13 \sim 2) \succ (12 \sim 1 \sim 3) \succ (23 \sim \{\})}{>=: (123 ~ 13 ~ 2) > (12 ~ 1 ~ 3) > (23 ~ \{\})}.
From this, we get the following three matrices:

\deqn{
M^\succsim_1 = \begin{bmatrix}
0 & 1 & 0\\
1 & 1 & 0\\
1 & 0 & 0
\end{bmatrix}
M^\succsim_2 = \begin{bmatrix}
1 & 0 & 0\\
0 & 1 & 1\\
1 & 0 & 0
\end{bmatrix}
M^\succsim_3 = \begin{bmatrix}
0 & 1 & 0\\
1 & 0 & 1\\
1 & 0 & 0
\end{bmatrix}
}{
M^(>=)_1 = matrix(c(0,1,1,1,1,0,0,0,0),nrow=3)\\
M^(>=)_2 = matrix(c(1,0,1,0,1,0,0,1,0),nrow=3)\\
M^(>=)_3 = matrix(c(0,1,1,1,0,0,0,1,0),nrow=3)
}

From \eqn{(M^\succsim_2)_{1,1} > (M^\succsim_1)_{1,1}}{(M_2)_(1,1) > (M_1)_1,1} and \eqn{(M^\succsim_2)_{1,1} > (M^\succsim_3)_{1,1}}{(M_2)_(1,1) > (M_3)_1,1} it
immediately follows that \eqn{2}{2} is ranked above \eqn{1}{1} and \eqn{3}{3} according to \eqn{L^{(1)}}{L^(1)}.

Comparing \eqn{1}{1} against \eqn{3}{3} we can set \eqn{p^0 = 2}{p^0 = 2} and \eqn{q^0 = 2}{q^0 = 2}.
Following the constraints from the definition above, we can verify that the entire column 1 is identical.
In column 2, we determine that \eqn{(M^\succsim_1)_{1,q^0} = (M^\succsim_3)_{1,q^0}}{(M_1)_(1,q^0) = (M_3)_(1,q^0)}, whereas
\eqn{(M^\succsim_1)_{p^0,q^0} > (M^\succsim_3)_{p^0,q^0}}{(M_1)_(p^0,q^0) > (M_3)_(p^0,q^0)}, indicating that \eqn{1}{1}
is ranked higher than \eqn{3}{3}, hence \eqn{2 \succ 1 \succ 3}{2 > 1 > 3} according to \eqn{L^{(1)}}{L^(1)}.
}

\section{Aliases}{


For better discoverability, \code{lexcel1Scores()} and \code{lexcel1Ranking()} serve as aliases for \code{L1Scores()} and \code{L1Ranking()}, respectively.
}

\examples{
pr <- as.PowerRelation("(123 ~ 13 ~ 2) > (12 ~ 1 ~ 3) > (23 ~ {})")
scores <- L1Scores(pr)
scores$`1`
#      [,1] [,2] [,3]
# [1,]    0    1    0
# [2,]    1    1    0
# [3,]    1    0    0

L1Ranking(pr)
# 2 > 1 > 3

}
\references{
\insertRef{2021Lexcel}{socialranking}
}
\seealso{
Other ranking solution functions: 
\code{\link{L2Scores}()},
\code{\link{LPSScores}()},
\code{\link{LPScores}()},
\code{\link{copelandScores}()},
\code{\link{cumulativeScores}()},
\code{\link{kramerSimpsonScores}()},
\code{\link{lexcelScores}()},
\code{\link{ordinalBanzhafScores}()}
}
\concept{ranking solution functions}
