\name{profit.factor}
\alias{profit.factor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
profit.factor(strat, ptc, s1, def, price)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strat}{
%%     ~~Describe \code{strat} here~~
}
  \item{ptc}{
%%     ~~Describe \code{ptc} here~~
}
  \item{s1}{
%%     ~~Describe \code{s1} here~~
}
  \item{def}{
%%     ~~Describe \code{def} here~~
}
  \item{price}{
%%     ~~Describe \code{price} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (strat, ptc, s1, def, price) 
{
    if (strat == "call") {
        if (price <= s1) {
            prft = def
        }
        else {
            prft = def * (1 + ptc * (price/s1 - 1))
        }
    }
    if (strat == "put") {
        if (price >= s1) {
            prft = def
        }
        else {
            prft = def * (1 - ptc * (price/s1 - 1))
        }
    }
    return(prft)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
