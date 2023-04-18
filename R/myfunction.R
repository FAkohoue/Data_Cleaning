#' Generate summary statistics and boxplots for each numeric column in a dataset, grouped by a specified column.
#'
#' @param data A data.frame or tibble containing the data.
#' @param group A string indicating the column name to group the data by.
#' @param path A string indicating the directory to save the output files to.
#' @param plot A logical indicating whether to generate boxplots for each numeric column.
#' @return NULL
#' @export
#' 
describe_by_group <- function(data, group, path,plot=TRUE){
  # check input arguments
  stopifnot(is.data.frame(data), is.character(group), is.character(path))
  
  # create directory if it doesn't exist
  if(!dir.exists(path)){
    dir.create(path)
  }
  
  # convert group argument to a quosure
  group_quo <- enquo(group)
  
  # loop over each numeric column and calculate summary statistics
  for (col in 2:length(data)){
    
    # convert col to column name
    col_name <- names(data)[col]
    
    # check if column exists in data
    if(col_name %in% colnames(data)){
      
      # calculate summary statistics for each group
      summary_stats_by_group <- data %>% 
        group_by(!!enquo(group)) %>% 
        summarise(
          n=length(group),
          Mean = mean(!!sym(col_name)),
          Median = median(!!sym(col_name)),
          Trimmed_Mean = mean(!!sym(col_name), trim = 0.1),
          SD = sd(!!sym(col_name)),
          Min = min(!!sym(col_name)),
          Max = max(!!sym(col_name)),
          CV = sd(!!sym(col_name))/mean(!!sym(col_name)),
          Range = max(!!sym(col_name)) - min(!!sym(col_name)),
          Skewness = skewness(!!sym(col_name)), 
          Kurtosis = kurtosis(!!sym(col_name))
        )
      
      # export result to a CSV file
      file_name1 <- paste0(col_name,".csv")
      file_path1 <- file.path(path, file_name1)
      write.csv(summary_stats_by_group, file_path1, row.names = FALSE)
    } 
    else {
      # skip column if not found in data
      message(paste0("Column ", col_name, " not found in data."))
    }
    
  }
  
  if(plot){
    #construct plot
    for (col in 2:length(data)){
      col_name <- names(data)[col]
      print(col)
      p <- ggplot(data = data, aes(x = !!group_quo, y = !!sym(col_name),fill=!!group_quo)) +
        geom_boxplot() +theme_classic()+theme(legend.title=element_blank())+xlab(" ")+
        labs(title = paste0("Boxplot of ", col_name))+stat_compare_means(method = "anova")
      
      # export graph
      file_name2<-paste0(col_name,".jpg")
      file_path2<-file.path(path, file_name2)
      ggexport(p,file_path2)
    }
    
  }
  # return NULL to indicate success
  NULL
}