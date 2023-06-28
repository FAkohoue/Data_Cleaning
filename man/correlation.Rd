\name{correlation}
\alias{correlation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pairwise correlation analysis for EDA
%%  ~~function to do ... ~~
}
\description{This function calculates correlation and builds plots among numeric columns in a dataset.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
correlation(data, path, calculate_cor = TRUE, generate_plot = TRUE, unused = NULL, corMethod = c("pearson", "kendall", "spearman"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data.frame or tibble containing the data. The first column is row ids or rownames.
%%     ~~Describe \code{data} here~~
}
  \item{path}{A string indicating the directory to save the output files to.
%%     ~~Describe \code{path} here~~
}
  \item{calculate_cor}{A logical indicating whether to calculate phenotypic correlations among columns.
%%     ~~Describe \code{calculate_cor} here~~
}
  \item{generate_plot}{A logical indicating whether to generate correlation plots among columns.
%%     ~~Describe \code{generate_plot} here~~
}
  \item{unused}{Argument to remove warning.
%%     ~~Describe \code{unused} here~~
}
  \item{corMethod}{A string indicating which method should be used to generate the correlation coefficient. Put NULL if calculate_cor = FALSE.
%%     ~~Describe \code{corMethod} here~~
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

correlation(data=dat, path="Summary_stats_corr", calculate_cor = TRUE, generate_plot = FALSE, unused = NULL, corMethod ="pearson")

## The function is currently defined as
function (data, path, calculate_cor = TRUE, generate_plot = TRUE,
    unused = NULL, corMethod = c("pearson", "kendall", "spearman"))
{
    stopifnot(is.data.frame(data), is.character(path))
    if (!dir.exists(path)) {
        dir.create(path)
    }
    if (calculate_cor) {
        cols <- data[, sapply(data, is.numeric)]
        if (corMethod == "pearson") {
            cor_res1 <- cor(cols, method = "pearson")
            cor_file_path1 <- file.path(path, "Pearson_correlation.csv")
            write.csv(cor_res1, cor_file_path1, row.names = FALSE)
        }
        if (corMethod == "spearman") {
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
    if (generate_plot) {
        data <- data[, -1]
        my_fn1 <- function(data, mapping, color = I("grey50"),
            sizeRange = c(1, 5), ...) {
            x <- GGally::eval_data_col(data, mapping$x)
            y <- GGally::eval_data_col(data, mapping$y)
            ct <- cor.test(x, y)
            sig <- symnum(ct$p.value, corr = FALSE, na = FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***",
                  "**", "*", " "))
            r <- unname(ct$estimate)
            rt <- sprintf(r, fmt = "\%0.2f")[1]
            cex <- max(sizeRange)
            percent_of_range <- function(percent, range) {
                percent * diff(range) + min(range, na.rm = TRUE)
            }
            ggally_text(label = as.character(rt), mapping = aes(),
                xP = 0.5, yP = 0.5, color = color, ...) + theme(panel.background = element_rect(color = color,
                linetype = "longdash"), axis.line = element_blank(),
                axis.ticks = element_blank(), axis.text.y = element_blank(),
                axis.text.x = element_blank()) + theme_classic(base_size = 6,
                base_family = "", base_line_size = 6/22, base_rect_size = 6/22)
        }
        my_fn2 <- function(data, mapping, ...) {
            ggplot(data = data, mapping = mapping) + geom_point(color = I("black"),
                size = 0.5) + geom_smooth(method = "lm", color = I("red"),
                ...) + theme_bw(base_size = 6, base_family = "",
                base_line_size = 6/22, base_rect_size = 6/22)
        }
        my_fn3 <- function(data, mapping) {
            ggplot(data = data, mapping = mapping) + geom_histogram(aes(y = ..density..),
                colour = 1, fill = "grey") + geom_density(aes(y = ..density..),
                colour = 4) + theme_bw(base_size = 6, base_family = "",
                base_line_size = 6/22, base_rect_size = 6/22)
        }
        p = ggpairs(data, upper = list(continuous = wrap(my_fn1)),
            lower = list(continuous = wrap(my_fn2, se = T, size = 0.5)),
            diag = list(continuous = wrap(my_fn3)), axisLabels = "show",
            labeller = "label_parsed", showStrips = F)
        mytheme = theme(strip.background = element_rect(colour = "black",
            fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold"))
        pcor = p + mytheme
        file_name2 <- paste0("cor.jpg")
        file_path2 <- file.path(path, file_name2)
        ggexport(pcor, file_path2)
    }
    NULL
  }
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
