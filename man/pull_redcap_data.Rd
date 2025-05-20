\name{pull_redcap_data}
\alias{pull_redcap_data}
\title{Get the latest data from RedCap!}
\usage{
pull_redcap_data(token, url, database)
}

\description{
The function is used to pull the data from RedCap for different cohorts
}

\arguments{
  \item{token}{
  Give the api token of the databse for which you want the data. If you don't have the token,
  if can be generated from redcap within the particular database given you have access to API
  for that RedCap project.
  }

  \item{url}{
  Mention the redcap url
  }

  \item{database}{
  Currently supports reconfig, aphasia, and consciousness.
  }
}


