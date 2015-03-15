\name{read.file}
\alias{read.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.file(file, skip, verbose = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{skip}{
%%     ~~Describe \code{skip} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (file, skip, verbose = T) 
{
    if (verbose) 
        print(paste("read", file))
    tmp <- read.table(file, skip = skip, sep = "\t", header = T, 
        na.strings = c(" +", ""), fill = T)
    tmp <- tmp[!is.na(tmp$Subject), ]
    if (sum(str_detect(tmp[, 1], "CH|GA|IJ|Kj|RMK"))) 
        print(paste("id", tmp[3, 1]))
    if (sum(tmp$Stim.Type \%in\% c("hit", "incorrect")) == 0) 
        return(NULL)
    tmp <- lapply(tmp, function(x) {
        if (class(x) \%in\% c("character", "factor")) {
            x <- factor(gsub(" ", "", as.character(x)))
            return(x)
        }
        else {
            return(x)
        }
    })
    tmp <- as.data.frame(tmp)
    pause <- which(tmp$Event.Type == "Picture" & tmp$Code == 
        "Pause")
    if (length(pause) > 0) {
        drei <- which(tmp$Code == 3 & !is.na(tmp$Code))
        drei <- drei[drei > pause][1:2]
        if (pause + 1 < drei[1]) {
            tmp <- tmp[-(pause:drei[2]), ]
        }
    }
    tmp <- tmp[!(tmp$Event.Type \%in\% c("Pause", "Resume")), ]
    first.pic <- min(which(tmp$Event.Type == "Picture" & !is.na(tmp$Event.Type))) - 
        1
    tmp <- tmp[-(1:first.pic), ]
    last.pic <- min(which(tmp$Event.Type == "Picture" & !is.na(tmp$Event.Type) & 
        tmp$Code == "Fertig!" & !is.na(tmp$Code)))
    tmp <- tmp[-(last.pic:nrow(tmp)), ]
    zeilen <- which(tmp$Event.Type \%in\% c("Response"))
    zeilen <- sort(unique(c(zeilen, zeilen - 1)))
    zeilen <- zeilen[zeilen > 0]
    tmp <- tmp[zeilen, ]
    responses <- which(tmp$Code \%in\% c(1, 2))
    events <- responses - 1
    tmp$Type <- NA
    tmp$Type[responses] <- as.character(tmp$Event.Type[events])
    if (length(tmp$Type[responses]) != length(tmp$Event.Type[events])) {
        print(file)
    }
    tmp$Event.Code <- NA
    tmp$Event.Code[responses] <- as.character(tmp$Code[events])
    tmp$Time1 <- NA
    tmp$Time1[responses] <- tmp$Time[events]
    tmp$Stim.Type[responses] <- as.character(tmp$Stim.Type[events])
    tmp$Duration[responses] <- as.character(tmp$Duration[events])
    tmp$Uncertainty.1[responses] <- as.character(tmp$Uncertainty.1[events])
    tmp$ReqTime[responses] <- as.character(tmp$ReqTime[events])
    tmp$ReqDur[responses] <- as.character(tmp$ReqDur[events])
    tmp$Pair.Index[responses] <- as.character(tmp$Pair.Index[events])
    tmp$Stim.Type[responses] <- as.character(tmp$Stim.Type[events])
    tmp <- tmp[tmp$Event.Type == "Response" & !is.na(tmp$Type), 
        ]
    tmp <- tmp[tmp$Type == "Picture" & !is.na(tmp$Type), ]
    return(tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
