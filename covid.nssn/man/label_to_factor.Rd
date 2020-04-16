\name{label_to_factor}
\alias{label_to_factor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Labeled to Factor
%%  ~~function to do ... ~~
}
\description{
Function that converts haven_labelled to factor variables with levels
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
label_to_factor(dt, var_list = NULL)
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
    varClass <- sapply(dt, class)
    varClass <- sapply(varClass, function(x) {
        x[1]
    })
    characterVars_haven <- names(varClass[varClass \%in\% c("haven_labelled")])
    temp <- as.data.table(purrr::map(dt[, ..characterVars_haven], 
        labelled::to_character))
    temp <- as.data.table(purrr::map(temp[, ..characterVars_haven], 
        labelled::to_factor))
    factor_levels <- sapply(temp, function(x) {
        length(unique(x))
    })
    non_binary <- names(factor_levels > 2)
    characterVars_ordered <- characterVars_haven[characterVars_haven \%in\% 
        non_binary]
    temp2 <- as.data.table(purrr::map(temp[, ..characterVars_ordered], 
        ordered))
    suppressWarnings(temp[, names(temp2)] <- temp2)
    temp3 <- dt[, !characterVars_haven, with = FALSE]
    new_data <- cbind(temp, temp3)
    return(new_data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
