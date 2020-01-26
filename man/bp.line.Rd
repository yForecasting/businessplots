\name{bp.line}
\alias{bp.line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Business Plot: 1 line
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plots 1 line from a csv file: column 1 is the x axis, column 2 is the plotted variable.
}
\usage{
bp.line(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{csv-file where different column represent different variables. The values are separated by ';'
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
Yves
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Primary use
bp.line('YearReport.csv')
}

\keyword{ ~line }
\keyword{ ~line plot }
