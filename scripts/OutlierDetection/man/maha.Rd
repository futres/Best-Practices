% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Maha.R
\name{maha}
\alias{maha}
\title{Outlier detection using Mahalanobis Distance}
\usage{
maha(x, cutoff = 0.95, rnames = FALSE)
}
\arguments{
\item{x}{dataset for which outliers are to be found}

\item{cutoff}{Percentile threshold used for distance, default value is 0.95}

\item{rnames}{Logical value indicating whether the dataset has rownames, default value is False}
}
\value{
Outlier Observations: A matrix of outlier observations

Location of Outlier: vector of Sr. no. of outliers

Outlier probability: vector of (1-p value) of outlier observations
}
\description{
Takes a dataset and find its outliers using modelbased method
}
\details{
maha computes Mahalanibis distance an observation and based on the Chi square cutoff, labels an observation as outlier. Outlierliness of the labelled 'Outlier' is also reported based on its p vlaues. For bivariate data, it also shows the scatterplot of the data with labelled outliers.
}
\examples{
#Create dataset
X=iris[,1:4]
#Outlier detection
maha(X,cutoff=0.9)
}
\references{
Barnett, V. 1978. The study of outliers: purpose and model. Applied Statistics, 27(3), 242–250.
}
