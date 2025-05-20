\name{crsr_func}
\alias{crsr_func}
\title{Get CRSR data}
\usage{
crsr_func(data, database, type)
}

\description{
Get the CRSR data along with consiousness status for each assessment by supplying the raw redcap data, the name of the database and optionally by supplying the type argument
}

\arguments{
  \item{data}{
  Provide the raw redcap data obtained from pull_redcap_data function
  }

  \item{database}{
  Currently supports reconfig, aphasia, and consciousness. Default is consciousness
  }

  \item{type}{
  Currently supports raw, yesno, all. Default is raw.
  raw returns crsr total with data, time and consciousness group of each assessment,
  yesno return binarized yes/no version if any assessment of the patients had crsr >= 8 during hospitalization
  all return both raw and yesno values.
  }
}
