\name{read.files}
\alias{read.files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.files(filesdir, skip = 3, recursive = F, pattern = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filesdir}{
%%     ~~Describe \code{filesdir} here~~
}
  \item{skip}{
%%     ~~Describe \code{skip} here~~
}
  \item{recursive}{
%%     ~~Describe \code{recursive} here~~
}
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
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
function (filesdir, skip = 3, recursive = F, pattern = ".") 
{
    files <- dir(filesdir, full.names = T, recursive = recursive, 
        pattern = pattern)
    Reduce(rbind, lapply(files, read.file, skip = skip))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
