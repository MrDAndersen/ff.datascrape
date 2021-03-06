% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_fantasyData.R
\name{scrape_fantasydata}
\alias{scrape_fantasydata}
\title{Scrape data from FantasyData}
\usage{
scrape_fantasydata(week = -1, position = c("QB", "RB", "WR", "TE", "K",
  "DST"))
}
\arguments{
\item{week}{The week that data will be scraped for. If omitted, season data
will be scraped.}

\item{position}{The player position to scrape data for. Has to be one of
\code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.}
}
\description{
Use this function to srape fantasy football projections from FantasyData
}
