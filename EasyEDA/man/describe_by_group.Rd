\name{describe_by_group}
\alias{describe_by_group}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{describe_by_group for EDA
%%  ~~function to do ... ~~
}
\description{This function summarizes numeric data and produces boxplots.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
describe_by_group(data, group, path, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{}{
  \code{data} A data.frame or tibble containing the data.
  \code{group} A string indicating the column name to group the data by.
  \code{path} A string indicating the directory to save the output files to.
  \code{plot} A logical indicating whether to generate boxplots for each numeric column.
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

describe_by_group(data=dat,group=dat$Group,plot = F,path="summary_stats")

## The function is currently defined as
function (x)
{
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
