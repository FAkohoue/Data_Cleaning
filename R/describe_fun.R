#' Generate summary statistics, histogram and boxplots for each numeric column in a dataset.
#' @param data A data.frame or tibble containing the data. The first column is row ids or rownames.
#' @param path A string indicating the directory to save the output files to.
#' @param hist A logical indicating whether to generate histogram for each numeric column.
#' @param boxplot A logical indicating whether to generate boxplot for each numeric column.
#' @return NULL
#' @export
#'
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
