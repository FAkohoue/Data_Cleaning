\name{arrange_and_join}
\alias{arrange_and_join}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rearrange and merge multi-environment trail data.
%%  ~~function to do ... ~~
}
\description{This function helps to rearrange and merge multi-environment trial data into a single file for downstream analyses.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arrange_and_join(output_path, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{output_path}{A path to indicate where the merged dataset should be saved.
%%     ~~Describe \code{output_path} here~~
}
\item{...}{List of the individual datasets to be rearranged and full joined.
%%     ~~Describe \code{...} here~~
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
\references{Félicien Akohoue, a Quantitative Genetics Researcher.
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
\examples{data1 <- data.frame(Env = c("E1", "E1", "E1"), Date = c(2022-01-01, 2022-01-02, 2022-01-03), Product = c("A", "B", "C"), Revenue = c(10, 5, 80))
data2 <- data.frame(Env = c("E2", "E2", "E2"), Date = c(2022-01-01, 2022-01-02, 2022-01-04), Product = c("A", "B", "D"), Revenue = c(100, 50, 200))
data3 <- data.frame(Env = c("E3", "E3", "E3"), Date = c(2022-01-01, 2022-01-03, 2022-01-04), Product = c("A", "C", "D"), Cost = c(50, 80, 150))

arrange_and_join("output.csv", data1, data2, data3)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
arrange_and_join <- function(output_path, ...){

  datasets <- list(...)  # Store all input datasets in a list

  # Get the unique column names from all datasets
  col_names <- unique(unlist(lapply(datasets, colnames)))

  # Reorder the columns of all datasets based on col_names
  for (i in seq_along(datasets)){
    missing_cols <- setdiff(col_names, colnames(datasets[[i]]))
    for (col in missing_cols) {
      datasets[[i]][[col]] <- NA  # Add missing columns with NAs
    }
    datasets[[i]] <- datasets[[i]][, col_names, drop = FALSE]
  }

  # Perform a full join on the rearranged datasets
  merged_data <- Reduce(function(x, y) merge(x, y, all = TRUE), datasets)

  print(merged_data)

  # Export the merged dataset to a CSV file
  write.csv(merged_data, file = output_path, row.names = FALSE)

  cat("Merged dataset exported to:", output_path, "\n")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
