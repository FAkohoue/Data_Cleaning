\name{describe_all}
\alias{describe_all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{describe_all for EDA
%%  ~~function to do ... ~~
}
\description{This function summarizes numeric data and produces histogramm and boxplot if specified.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
describe_all(data, path, hist = TRUE, boxplot = TRUE, unused = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data.frame or tibble containing the data. The first column is row ids or rownames.
%%     ~~Describe \code{data} here~~
}
  \item{path}{A string indicating the directory to save the output files to.
%%     ~~Describe \code{path} here~~
}
  \item{hist}{A logical indicating whether to generate histogram for each numeric column.
%%     ~~Describe \code{hist} here~~
}
  \item{boxplot}{A logical indicating whether to generate boxplot for each numeric column.
%%     ~~Describe \code{boxplot} here~~
}
  \item{unused}{Argument to remove warning.
%%     ~~Describe \code{unused} here~~
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
\examples{DS<-c(2,3,1,2,3,5,6,10,25,30,3,5,6,7,8,9,12,34,4,22,21,35,66,28)
PH<-c(100,98,87,89.9,109,111,104,105,95,96,92,91,88,34,67,77,65,66,34,57,112,109,115,92.5)
Group<-c("G1","G2","G3","G4","G2","G1","G3","G4","G4","G3","G2","G1","G1","G4","G3","G2","G1","G3","G4","G2","G2","G1","G3","G1")
dat<-data.frame(Group,DS,PH)

describe_all(data=dat,path="summary_stats_all",hist=FALSE,boxplot = FALSE)

## The function is currently defined as

describe_all <- function(data, path, hist = TRUE, boxplot = TRUE, unused = NULL) {
  # check input arguments
  stopifnot(is.data.frame(data), is.character(path))

  # create directory if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path)
  }

  # initialize empty list to hold summary statistics for each column
  summary_stats_all <- list()

  for (col in 2:length(data)) {

    # convert col to column name
    Trait <- names(data)[col]

    #Calculate summary statistics

    summary_stats <- data %>%
      summarise(
        Size = length(!!sym(Trait)),
        Mean = mean(!!sym(Trait), na.rm = TRUE),
        First_Quart = quantile(!!sym(Trait),0.25, na.rm = TRUE),
        Median = median(!!sym(Trait), na.rm = TRUE),
        Third_Quart = quantile(!!sym(Trait),0.25, na.rm = TRUE),
        Trimmed_Mean = mean(!!sym(Trait), trim = 0.1, na.rm = TRUE),
        SD = sd(!!sym(Trait), na.rm = TRUE),
        MAD = mad(!!sym(Trait), na.rm = TRUE),
        Min = min(!!sym(Trait), na.rm = TRUE),
        Max = max(!!sym(Trait), na.rm = TRUE),
        CV = sd(!!sym(Trait), na.rm = TRUE) / mean(!!sym(Trait), na.rm = TRUE),
        Range = max(!!sym(Trait), na.rm = TRUE) - min(!!sym(Trait), na.rm = TRUE),
        SE = std.error(!!sym(Trait), na.rm = TRUE),
        Skewness = skewness(!!sym(Trait), na.rm = TRUE),
        Kurtosis = kurtosis(!!sym(Trait), na.rm = TRUE)
      )

    # add column name as first row
    summary_stats <- cbind(Trait, summary_stats)

    # append summary statistics to list
    summary_stats_all[[Trait]] <- summary_stats

    #Build histograms with column name as plot name

        p1 <- ggplot(data = data, aes(x = !!sym(Trait))) +
          geom_histogram(color ="black", fill = "grey",binwidth = 1) + theme_classic() +
          labs(title = paste0("Histogram of ", Trait))

        # export graph
        p1 <- paste0(Trait, "_histogram.pdf")
        ggsave(p1, path = path)

    #Build boxplots with column name as plot name

        p2 <- ggplot(data = data, aes(x = !!sym(Trait))) +
          geom_boxplot(color = "black", fill = "grey") + theme_classic() +
          labs(title = paste0("Boxplot of ", Trait))

        # export graph
        p2 <- paste0(Trait, "_boxplot.pdf")
        ggsave(p2, path = path)

  }

  # combine summary statistics for all columns into a single data frame
  summary_stats_all <- do.call(rbind, summary_stats_all)

  # export summary statistics for all columns to a CSV file
  file_name_all <- paste0("all_summary_stats.csv")
  file_path_all <- file.path(path, file_name_all)
  write.csv(summary_stats_all, file_path_all, row.names = FALSE)
  # return NULL to indicate success
  NULL
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
