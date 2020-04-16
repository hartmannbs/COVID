\name{covid_table}
\alias{covid_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
covid_table(group_var, ind_var, data, row = FALSE, column = TRUE, total = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{group_var}{
%%     ~~Describe \code{group_var} here~~
}
  \item{ind_var}{
%%     ~~Describe \code{ind_var} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{row}{
%%     ~~Describe \code{row} here~~
}
  \item{column}{
%%     ~~Describe \code{column} here~~
}
  \item{total}{
%%     ~~Describe \code{total} here~~
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
function (group_var, ind_var, data, row = FALSE, column = TRUE, 
    total = FALSE) 
{
    ddpcr::quiet(attach(data))
    if (class(ind_var) == "numeric") {
        suppressWarnings(descr::compmeans(ind_var, group_var, 
            plot = FALSE))
    }
    else {
        suppressWarnings(descr::CrossTable(group_var, ind_var, 
            prop.r = row, prop.c = column, prop.t = total, prop.chisq = FALSE, 
            dnn = c(deparse(substitute(ind_var)), deparse(substitute(group_var)))))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
