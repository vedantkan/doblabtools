\name{demo_func}
\alias{demo_func}
\title{Get patient demographics data}
\usage{
demo_func(data, database = "consciousness")
}

\description{
Get the demographics data by supplying the raw redcap data. You can get the raw redcap data
by using pull_redcap_data function.
You will also need to spcifiy which which database the raw data belongs to as different database
had different column names
}

\arguments{
  \item{data}{
  Provide the raw redcap data obtained from pull_redcap_data function
  }

  \item{database}{
  Currently supports reconfig, aphasia, and consciousness. Default is consciousness
  }
}
