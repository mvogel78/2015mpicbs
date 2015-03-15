\name{read.files}
\alias{read.files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Reading several files
}
\description{
  function created in context of a introductory R course March 2015 to read in presentation file. These functions does not work with presentation files in general.
}
\usage{
read.files(filesdir, skip = 3, recursive = F, pattern = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filesdir}{
    path including the folder containing the files which are to be read in
}
  \item{skip}{
    argument to \code{read.file}: number of rows to be skipped at the beginning of each file
}
  \item{recursive}{
    should also files contained in subdirectories be read
}
  \item{pattern}{
    pattern to specify the file
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  data frame containing the read and transformed data
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Mandy Vogel
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{read.file}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~presentation }
\keyword{ ~read file }% __ONLY ONE__ keyword per line
