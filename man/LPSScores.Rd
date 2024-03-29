% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lexcelPS.R
\name{LPSScores}
\alias{LPSScores}
\alias{LPSRanking}
\alias{lexcelPSScores}
\alias{lexcelPSRanking}
\title{LP* Ranking}
\usage{
LPSScores(powerRelation, elements = powerRelation$elements)

LPSRanking(powerRelation)

lexcelPSScores(powerRelation, elements = powerRelation$elements)

lexcelPSRanking(powerRelation)
}
\arguments{
\item{powerRelation}{A \code{\link{PowerRelation}} object created by \code{\link[=PowerRelation]{PowerRelation()}} or \code{\link[=as.PowerRelation]{as.PowerRelation()}}}

\item{elements}{Vector of elements of which to calculate their scores.
By default, the scores of all elements in \code{powerRelation$elements} are considered.}
}
\value{
Score function returns a list of type \code{LP*Scores} and length of \code{powerRelation$elements}
(unless parameter \code{elements} is specified).
Each index contains a matrix with \code{length(powerRelation$elements)} rows and a variable number of columns, depending on the equivalence class index containing the singleton coalition of that element (matrix can have 0 columns).

Ranking function returns corresponding \code{\link{SocialRanking}} object.
}
\description{
Calculate the \eqn{L^{p^*}}{L^p*} scores.
}
\details{
Let \eqn{N}{N} be a set of elements, \eqn{\succsim \in \mathcal{T}(\mathcal{P})}{>= in T(P)} a power relation,
and \eqn{\Sigma_1 \succ \Sigma_2 \succ \dots \succ \Sigma_m}{E_1 > E_2 > ... > E_m} its corresponding quotient order.

For an element \eqn{i \in N}{i in N}, construct a matrix \eqn{M^\succsim_i}{M^(>=)_i} with \eqn{m}{m} columns and \eqn{|N|}{|N|} rows.
Whereas each column \eqn{q}{q} represents an equivalence class, each row \eqn{p}{p} corresponds to the coalition size.

\deqn{(M^\succsim_i)_{p,q} = |\lbrace S \in \Sigma_q: |S| = p \text{ and } i \in S\rbrace|}{(M^(>=)_i)_pq = |\{S in E_q: |S| = p and i in S\}}

For \eqn{i, j \in N}{i, j in N}, the social ranking solution \eqn{L^{p^*}}{L^p*} then ranks \eqn{i} strictly above \eqn{j} if one of the following conditions hold:
\enumerate{
\item \eqn{\lbrace i \rbrace \succ \lbrace j \rbrace}{\{i\} > \{j\}};
\item \eqn{\lbrace i \rbrace, \lbrace j \rbrace \in \Sigma_k}{\{i\}, \{j\} in E_k} and there exists a row \eqn{p_0 \in \lbrace 2, \dots, |N|\rbrace}{p_0 in \{2, ..., |N|-1\}} and column \eqn{q_0 \in \lbrace 1, \dots, k-1\rbrace}{q_0 in \{1, ..., k-1\}} such that:
\deqn{(M^\succsim_i)_{p,q} = (M^\succsim_j)_{p,q}\quad \forall p < p_0, q < k,}{(M^(>=)_i)_(p,q) = (M^(>=)_j)_(p,q) for all p < p_0, q < k,}
\deqn{(M^\succsim_i)_{p_0,q} = (M^\succsim_j)_{p_0,q}\quad \forall q < q_0,\text{ and}}{(M^(>=)_i)_(p_0,q) = (M^(>=)_j)_(p_0,q) for all q < q_0, and}
\deqn{(M^\succsim_i)_{p_0,q_0} > (M^\succsim_j)_{p_0,q_0}.}{(M^(>=)_i)_(p_0,q_0) > (M^(>=)_j)_(p_0,q_0).}
}
}
\section{Example}{


Let \eqn{\succsim: (123 \sim 12 \sim 2) \succ (13 \sim 23) \succ (1 \sim 3 \sim \{\})}{>=: (123 ~ 13 ~ 2) > (12 ~ 23) > (1 ~ 3 ~ \{\})}.
From this, we get the following three matrices:

\deqn{
M^\succsim_1 = \begin{bmatrix}
0 & 0 & 1\\
1 & 1 & 0\\
1 & 0 & 0
\end{bmatrix}
M^\succsim_2 = \begin{bmatrix}
1 & 0 & 0\\
1 & 0 & 1\\
1 & 0 & 0
\end{bmatrix}
M^\succsim_3 = \begin{bmatrix}
0 & 0 & 1\\
0 & 2 & 0\\
1 & 0 & 0
\end{bmatrix}
}{
M^(>=)_1 = matrix(c(0,1,1,0,1,0,1,0,0),nrow=3)\\
M^(>=)_2 = matrix(c(1,1,1,0,0,0,0,1,0),nrow=3)\\
M^(>=)_3 = matrix(c(0,0,1,0,2,0,1,0,0),nrow=3)
}

\eqn{(M^\succsim_2)_{2,3}}{(M^(>=)_2)_(2,3)} in this context refers to the value in the second row and third column of element 2, in this case \eqn{1}{1}.

In the example, \eqn{2}{2} will be immediately put above \eqn{1}{1} and \eqn{3}{3} because \eqn{\lbrace 2 \rbrace \succ \lbrace 1 \rbrace}{\{2\} > \{1\}} and \eqn{\lbrace 2 \rbrace \succ \lbrace 3 \rbrace}{\{2\} > \{3\}}.
Since \eqn{\lbrace 1 \rbrace \sim \lbrace 3 \rbrace}{\{1\} ~ \{3\}}, we next consider the coalitions of size 2. Here, it turns out that \eqn{(M^\succsim_1)_{2,1} = 1 > 0 = (M^\succsim_3)_{2,1}}{(M^(>=)_1)_(2,1) = 1 > 0 = (M^(>=)_3)_(2,1)},
setting \eqn{3}{3} to be the least preferred option (this is opposed to the \eqn{L^p}{L^p} relation, which has no strict preference of \eqn{1}{1} over \eqn{3}{3}).

As alluded to, \eqn{L^{p^*}}{L^p*} is similar to \eqn{L^p}{L^p}, \code{\link[=LPRanking]{LPRanking()}}, in that it first considers the singleton coalitions, then sequentially every coalition of size 2 and above that ranks better than the corresponding singleton.
It can be assumed, however, that \eqn{L^{p^*}}{L^p*} is more granular, as it doesn't throw away any information about \emph{which} equivalence class these bigger coalitions belong to.
}

\section{Alterations}{


The matrices as described above and in \insertRef{beal2022lexicographic}{socialranking} can be investigated with the \code{\link[=L1Scores]{L1Scores()}} function.

\code{LPSScores()} discards some redundant information, most notably all columns from each element's singleton class and the ones thereafter.
The first row is also removed, as all values there are guaranteed to be 0.

For the example above, this would actually result in the matrices

\preformatted{
matrix(c(1,1, 1,0), nrow=2)
matrix(numeric(), nrow=2)
matrix(c(0,1, 2,0), nrow=2)
}
}

\section{Aliases}{


For better discoverability, \code{lexcelPSScores()} and \code{lexcelPSRanking()} serve as aliases for \code{LPSScores()} and \code{LPSRanking()}, respectively.
}

\examples{
pr <- as.PowerRelation("(123 ~ 12 ~ 2) > (13 ~ 23) > (1 ~ 3 ~ {})")
scores <- LPSScores(pr)
scores$`1`
#      [,1] [,2]
# [1,]    1    1
# [2,]    1    0

scores$`2`
#
# [1,]
# [2,]

LPSRanking(pr)
# 2 > 1 > 3

}
\references{
\insertRef{beal2022lexicographic}{socialranking}
}
\seealso{
Other ranking solution functions: 
\code{\link{L1Scores}()},
\code{\link{L2Scores}()},
\code{\link{LPScores}()},
\code{\link{copelandScores}()},
\code{\link{cumulativeScores}()},
\code{\link{kramerSimpsonScores}()},
\code{\link{lexcelScores}()},
\code{\link{ordinalBanzhafScores}()}
}
\concept{ranking solution functions}
