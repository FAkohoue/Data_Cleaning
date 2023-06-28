\name{describe_by_group}
\alias{describe_by_group}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{describe_by_group for EDA
%%  ~~function to do ... ~~
}
\description{This function summarizes numeric data and produces boxplots per group.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
describe_by_group(data, group, path, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data.frame or tibble containing the data.
%%     ~~Describe \code{x} here~~
}
  \item{group}{A string indicating the column name to group the data by.
  %%     ~~Describe \code{x} here~~
}
  \item{path}{A string indicating the directory to save the output files to.
  %%     ~~Describe \code{x} here~~
}
  \item{plot}{A logical indicating whether to generate boxplots for each numeric column.
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
\author{Félicien Akohoue, a Quantitative Genetics Researcher.
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
DS<-c(2,3,1,2,3,5,6,10,25,30,3,5,6,7,8,9,12,34,4,22,21,35,66,28)
PH<-c(100,98,87,89.9,109,111,104,105,95,96,92,91,88,34,67,77,65,66,34,57,112,109,115,92.5)
Group<-c("G1","G2","G3","G4","G2","G1","G3","G4","G4","G3","G2","G1","G1","G4","G3","G2","G1","G3","G4","G2","G2","G1","G3","G1")
dat<-data.frame(Group,DS,PH)

describe_by_group <- function(data, group, path, plot = TRUE) {
  # check input arguments
  stopifnot(is.data.frame(data), is.character(group), is.character(path))

  # create directory if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path)
  }
  # initialize empty list to hold summary statistics for each column
  summary_stats_by_group <- list()

  # convert group argument to a quosure
  group_quo <- enquo(group)

  # loop over each numeric column and calculate summary statistics
  for (col in 2:length(data)) {

    # convert col to column name
    Trait <- names(data)[col]

    # check if column exists in data
    if (Trait %in% colnames(data)) {

      # calculate summary statistics for each group
      summary_stats <- data %>%
        group_by(!!enquo(group)) %>%
        summarise(
          Size = length(!!sym(Trait)),
          Mean = mean(!!sym(Trait), na.rm = TRUE),
          First_Quart = quantile(!!sym(Trait), 0.25, na.rm = TRUE),
          Median = median(!!sym(Trait), na.rm = TRUE),
          Third_Quart = quantile(!!sym(Trait), 0.75, na.rm = TRUE),
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
      summary_stats_by_group[[Trait]] <- summary_stats

      if (plot) {
        # construct plot with column name as plot name
        p <- ggplot(data = data, aes(x = !!group_quo, y = !!sym(Trait), fill=!!group_quo)) +
          geom_boxplot() + theme_classic() + theme(legend.title = element_blank()) + xlab(" ") +
          labs(title = paste0("Boxplot of ", Trait))

        # export graph with column name as file name
        p <- paste0(Trait, "_boxplot.pdf")
        ggsave(p, path = path)
      }

    }

    else {
      # skip column if not found in data
      message(paste0("Column ", Trait, " not found in data."))
    }

  }

  # combine summary statistics for all columns into a single data frame
  summary_stats_by_group <- do.call(rbind, summary_stats_by_group)

  # rename the second column
  colnames(summary_stats_by_group)[2] <- "Group"

  # export summary statistics for all columns to a CSV file
  file_name_all1 <- paste0("all_summary_stats_by_group.csv")
  file_path_all1 <- file.path(path, file_name_all1)
  write.csv(summary_stats_by_group, file_path_all1, row.names = FALSE)

  # return NULL to indicate success
  NULL
}
