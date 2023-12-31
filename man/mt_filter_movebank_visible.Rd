% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mt_filter_movebank_visible.R
\name{mt_filter_movebank_visible}
\alias{mt_filter_movebank_visible}
\alias{mt_movebank_visible}
\title{Identify records that are not outliers according to the logic used in movebank}
\usage{
mt_filter_movebank_visible(x)

mt_movebank_visible(x)
}
\arguments{
\item{x}{a move2 object}
}
\value{
\code{mt_movebank_visible}returns a logical vector indicating the records that are valid.\cr
\code{mt_filter_movebank_visible} returns a filtered \code{move2} object
}
\description{
\itemize{
\item \code{mt_filter_movebank_visible}: returns a \code{move2} object with all visible data, i.e., excluding all records marked
as outliers according to the logic used in movebank (See \emph{Details})
\item \code{mt_movebank_visible}: indicates with \code{TRUE} the visible records, and with \code{FALSE} those marked as outliers
according to the logic used in movebank (See \emph{Details})
}
}
\details{
These functions rely on the columns 'visible', 'algorithm_marked_outlier', 'import_marked_outlier',
'manually_marked_outlier', and/or 'manually_marked_valid'. All of them are expected to be logical. More details
can be found in the \href{http://vocab.nerc.ac.uk/collection/MVB/current/MVB000209/}{movebank vocabulary}
}
\examples{
\dontshow{if (parallel::detectCores() < 9) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
m <- mt_read(mt_example())
table(mt_movebank_visible(m))
mt_filter_movebank_visible(m)
\dontshow{\}) # examplesIf}
}
\seealso{
Other filter: 
\code{\link{mt_filter_per_interval}()},
\code{\link{mt_filter_unique}()}
}
\concept{filter}
