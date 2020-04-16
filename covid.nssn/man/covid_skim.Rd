\name{covid_skim}
\alias{covid_skim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Skimming COVID-nssn
%%  ~~function to do ... ~~
}
\description{
skims over covid data easily
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
covid_skim(dt, var_list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{var_list}{
%%     ~~Describe \code{var_list} here~~
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
function (dt, var_list = NULL) 
{
    dt <- data.table::as.data.table(dt)
    if (is.null(var_list) == FALSE) {
        dt <- dt[, var_list, with = FALSE]
    }
    else {
        dt <- dt
    }
    dt <- dtplyr::lazy_dt(dt)
    skimr::skim(dt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
