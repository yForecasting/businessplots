\name{timeline}
\alias{timeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Time line for events
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates a timeline with events based on an input csv file. The file has 4 column: month, year, milestone, status. The status has 4 options: Complete, On Target, At Riks, Critical.
}
\usage{
timeline(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ directory and name of the file
%%     ~~Describe \code{x} here~~
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
## Primary use
timeline("Timelinedata.csv")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~timeline }
\keyword{ ~planning }
