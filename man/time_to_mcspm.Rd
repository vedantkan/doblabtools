\name{time_to_mcspm}
\alias{time_to_mcspm}
\title{Get MCSm status and time to that status}
\usage{
time_to_mcspm(data, database)
}

\description{
The function returns a dataframe with information required for time to models.
The first column mcspm_recov provides yes/no if the patients had any assessment whose conscious status was either MCSm, MCSp or CS
The second column is the mcspm status. 0=Dead, 1=MCSm Recovered, 2=WLST/did not recover in 30 days
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
