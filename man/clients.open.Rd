\name{clients.open}
\alias{clients.open}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("clients.open")}
\format{
  A data frame with 1 observations on the following 9 variables.
  \describe{
    \item{\code{num}}{a numeric vector}
    \item{\code{base.asset}}{a factor with levels \code{USDRUB}}
    \item{\code{strategy}}{a factor with levels \code{put}}
    \item{\code{def}}{a numeric vector}
    \item{\code{ptc}}{a numeric vector}
    \item{\code{s1}}{a numeric vector}
    \item{\code{amount}}{a numeric vector}
    \item{\code{s.date}}{a Date}
    \item{\code{e.date}}{a Date}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(clients.open)
## maybe str(clients.open) ; plot(clients.open) ...
}
\keyword{datasets}
