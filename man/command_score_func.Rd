\name{command_score_func}
\alias{command_score_func}
\title{Get command score data}
\usage{
command_score_func(data, database, type)
}

\description{
Get the command score data for each assessment by supplying the raw redcap data, the name of the database and optionally by supplying the type argument
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
  raw returns command score data for each assessment,
  yesno return binarized yes/no version if any assessment of the patients had command score >= 4 during hospitalization
  all return both raw and yesno values.
  }
}
