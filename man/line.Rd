% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/line.R
\name{line}
\alias{line}
\title{line plot function}
\usage{
line(
  file,
  x_column,
  y_column,
  symbol,
  line_type,
  auto_fit = TRUE,
  primary_color,
  secondary_color
)
}
\arguments{
\item{file}{A csv file with the source data}

\item{x_column}{Name of the column from the file you want to plot on the x-axis}

\item{y_column}{Name of the column from the file you want to plot on the y-axis}

\item{symbol}{Number of the symbol you want for the plot}

\item{line_type}{The type of line you want for the line plot}

\item{auto_fit}{Put false to start from 0, True shows the best fit.}

\item{primary_color}{First color for line plot}

\item{secondary_color}{Second color for line plot}
}
\value{
A line plot
}
\description{
Creates a line plot of the data.
}
\details{
This function creates a plot based on the format of the data
}
\examples{
  \dontrun{
     line("testdata.csv", 
"column_1", 
"column_2", 5, 2, TRUE, 
"#004D9A", 
"#002142")
  }


}
\author{
Emiel Creus
}
