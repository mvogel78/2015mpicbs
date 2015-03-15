\name{read.file}
\alias{read.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Reads in presentation files
}
\description{
  Reads a presentation files transform it and delete unnecessary rows,
  this function was written in the context of a introductory R course in
  March 2015 and does not work with presentation files in general.
}
\usage{
read.file(file, skip, verbose = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    file names
}
  \item{skip}{
    number of rows skipped 
}
  \item{verbose}{
    prints out the file name while reading
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  data frame containing the cleaned data
}
\references{

}
\author{
  Mandy Vogel
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{read.files}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~read file }
\keyword{ ~presentation }% __ONLY ONE__ keyword per line
