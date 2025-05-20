\name{time_to_gose}
\alias{time_to_gose}
\title{Get GOSE status and time to that status}
\usage{
time_to_gose(data, database)
}

\description{
The function returns a dataframe with information required for time to models.
The first column gose_recov provides yes/no if the patients had any GOSE >= 4 during first 12 months after discharge
The second column is the GOSE status. 0=Dead, 1=GOSE Recovered, 2=WLST/did not recover in 360 days
The third column provides information about the time it takes to achive the status in second column
}

\arguments{
  \item{data}{
  Provide the raw redcap data obtained from pull_redcap_data function
  }

  \item{database}{
  Currently supports reconfig, aphasia, and consciousness. Default is consciousness
  }

}
