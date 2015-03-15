\name{df.t.test}
\alias{df.t.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function which performs a two sample t-test on a grouped data frame
}
\description{
  Given a data frame and a given index vector, the data frame is split
  by this vector and a two sample t-test is performed on the colum col
  dependend on the column indcol. The result is a data frame containing
  group, means, test statistic, confidence interval, raw p-values,
  alternative and method for each level of group
}
\usage{
df.t.test(df, group, col, indcol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
    data frame containing the index vector for splitting the data frame;
    the numeric vector and the binary vector to pass through to t-test
}
  \item{group}{
    name of the columns along with the data frame should be splitted
}
  \item{col}{
    name of the numeric column as string
}
  \item{indcol}{
    name of the binary column defining the two groups for the t-test
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  data frame containing the following columns:
  \itemize{
    \item data - column names of the columns the t-test is performed on
    \item group - respective level of the group variable the data frame
    is split on
    \item mean.group.1 - mean of group.1 in t.test
    \item mean.group.2 - mean of group.2 in t.test
    \item name.test.stat - name of the test statistic
    \item conf.lower - lower limit of the confidence interval
    \item conf.upper - lower limit of the confidence interval
    \item pval - p value
    \item method - name of the method
    }
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
  \code{\link{t.test}},
}
\examples{
## example data
examp.data <- data.frame(group=gl(10,100),
                         values=rnorm(1000),
                         t.group=sample(letters[1:2],1000,replace=TRUE))
## example
df.t.test(examp.data,"group","values","t.group")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ t.test }
\keyword{ data frame }
