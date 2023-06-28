#' Calculate correlation and build plot among numeric columns in a dataset.
#' @param data A data.frame or tibble containing the data. The first column is row ids or rownames.
#' @param path A string indicating the directory to save the output files to.
#' @param calculate_cor A logical indicating whether to calculate phenotypic correlations among columns.
#' @param generate_plot A logical indicating whether to generate correlation plots among columns.
#' @param corMethod A string indicating which method should be used to calculate the correlation coefficients. Put NULL if calculate_cor = FALSE.
#' @return NULL
#' @export
#'
correlation <- function(data, path, calculate_cor = TRUE, generate_plot = TRUE, unused=NULL, corMethod=c("pearson","kendall","spearman")){

  # Check input arguments
  stopifnot(is.data.frame(data), is.character(path))

  # Create directory if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path)
  }

  # Calculate pairwise correlations and export to a CSV file

  if (calculate_cor) {
    # Extract numeric columns
    cols <- data[, sapply(data, is.numeric)]

    #Calculate correlations
    if (corMethod == "pearson"){
      cor_res1 <- cor(cols, method = "pearson")
      cor_file_path1 <- file.path(path, "Pearson_correlation.csv")
      write.csv(cor_res1, cor_file_path1, row.names = FALSE)
    }

    if (corMethod == "spearman"){
      cor_res2 <- cor(cols, method = "spearman")
      cor_file_path2 <- file.path(path, "Spearman_correlation.csv")
      write.csv(cor_res2, cor_file_path2, row.names = FALSE)
    }
    if (corMethod == "kendall") {
      cor_res3 <- cor(cols, method = "kendall")
      cor_file_path3 <- file.path(path, "Kendall_correlation.csv")
      write.csv(cor_res3, cor_file_path3, row.names = FALSE)
    }
  }

  # Generate correlation plot
  if (generate_plot) {

    #Extract numeric columns
    data <- data[, -1]

    # construct plot
    my_fn1 <- function(data, mapping, color = I("grey50"), sizeRange = c(1, 5), ...) {
      #get the x and y data to use the other code
      x <- GGally::eval_data_col(data, mapping$x)
      y <- GGally::eval_data_col(data, mapping$y)

      ct <- cor.test(x, y)
      sig <- symnum(
        ct$p.value, corr = FALSE, na = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 1),
        symbols = c("***", "**", "*", " ")
      )

      r <- unname(ct$estimate)
      rt <- sprintf(r, fmt = "%0.2f")[1]

      # since we can't print it to get the strsize, just use the max size range
      cex <- max(sizeRange)

      # helper function to calculate a useable size
      percent_of_range <- function(percent, range) {
        percent * diff(range) + min(range, na.rm = TRUE)
      }

      # plot the cor value
        ggally_text(
          label = as.character(rt),
          mapping = aes(),
          xP = 0.5, yP = 0.5,
          color = color,
          ...
        ) +
          # remove all the background stuff and wrap it with a dashed line
          theme(
            panel.background = element_rect(
              color = color,
              linetype = "longdash"
            ),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank()
          ) +
          theme_classic(
            base_size = 6,
            base_family = "",
            base_line_size = 6 / 22,
            base_rect_size = 6 / 22
          )
      }

      my_fn2 <- function(data, mapping, ...) {
        ggplot(data = data, mapping = mapping) +
          geom_point(color = I("black"), size = 0.5) +
          geom_smooth(method = "lm", color = I("red"), ...) +
          theme_bw(
            base_size = 6,
            base_family = "",
            base_line_size = 6 / 22,
            base_rect_size = 6 / 22
          )
      }

      my_fn3 <- function(data, mapping){
        ggplot(data = data, mapping = mapping) +
          geom_histogram(aes(y = ..density..), colour = 1, fill = "grey") +
          geom_density(aes(y = ..density..), colour = 4)+
          theme_bw(
            base_size = 6,
            base_family = "",
            base_line_size = 6 / 22,
            base_rect_size = 6 / 22
          )}

      p <- ggpairs(data,
                upper = list(continuous = wrap(my_fn1)),
                lower = list(continuous =   wrap(my_fn2, se = TRUE, size = 0.5)),
                diag = list(continuous = wrap(my_fn3)),
                axisLabels =  "show",
                labeller = "label_parsed",
                showStrips = FALSE)

      mytheme <- theme(strip.background = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"))
      pcor <- p + mytheme

      # export graph
      file_name2 <- paste0("cor.jpg")
      file_path2 <- file.path(path, file_name2)
      ggexport(pcor, file_path2)
      }

    # return NULL to indicate success
    NULL
}
