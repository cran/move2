% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/column_name.R
\name{mt_time_column}
\alias{mt_time_column}
\alias{mt_track_id_column}
\alias{mt_set_time_column}
\alias{mt_set_track_id_column}
\title{Get or set the name of the column containing the \code{track_id} and \code{time}}
\usage{
mt_time_column(x)

mt_track_id_column(x)

mt_set_time_column(x, value)

mt_set_track_id_column(x, value)
}
\arguments{
\item{x}{a \code{move2} object}

\item{value}{a character string of the new column name}
}
\value{
\code{mt_time_column} and \code{mt_track_id_column} return character string of the column name \cr
\code{mt_set_time_column} and \code{mt_set_track_id_column} return an updated \code{move2} object
}
\description{
\itemize{
\item \code{mt_time_column()} returns the name of the column containing the timestamps
\item \code{mt_track_id_column()} returns the name of the column containing the track ids
\item \code{mt_set_time_column()} set the column that should be used as time column
\item \code{mt_set_track_id_column()} set the column that should be used as track id column (the column has to be present in event and track table)
}
}
\details{
The set functions purely update the attribute containing the column name after checking the minimal requirements.

For \code{mt_set_track_id_column()} the column has to be present in event and track table, if this is not the case consider using \code{\link[=mt_track_id]{mt_track_id()}}.
}
\examples{
## getting the column names
mt_sim_brownian_motion() |> mt_time_column()
mt_sim_brownian_motion() |> mt_track_id_column()

## setting 'time' to a new column
x <- mt_sim_brownian_motion()
x$date <- as.Date("2020-1-1") + x$time * 3
x |> mt_time_lags()
x |>
  mt_set_time_column("date") |>
  mt_time_lags()
}
\seealso{
\code{\link[=mt_time]{mt_time()}} to retrieve or change timestamps from each record.\cr
\code{\link[=mt_track_id]{mt_track_id()}} to retrieve or change the track id from each record.
}
