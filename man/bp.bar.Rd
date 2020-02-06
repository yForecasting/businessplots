\name{bp.bar}
\alias{bp.bar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Business Plot: Simple Bar Graph
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plots 1 line from a csv file: column 1 is the horizontal axis, column 2 is the plotted variable.
The height of the bar can be given in column 2, or this can be summarized by the function.
}
\usage{
bp.bar(file, valuesgiven=TRUE, horbars=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{csv-file where different column represent different variables. The values are separated by ';'}
  \item{valuesgiven}{If set on TRUE, the summarized values are given in column 2 of the csv file. Otherwise this function summarizes the data. This is standard set on TRUE.}
  \item{horbars}{Horizental bar chart is default set to FALSE.}
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
Yves Sagaert
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
bp.bar('YearReport.csv')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~bar }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~bar plot }% __ONLY ONE__ keyword per line
