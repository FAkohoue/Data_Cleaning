arrange_and_join <- function(output_path, ...) {

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
