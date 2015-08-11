\name{world.nations}
\alias{world.nations}
\docType{data}
\title{World nations borders}
\description{
  Geographic coordinates of national borders for the world. 
  This datasets can be used to draw geographic outlines 
  in maps.
}
\usage{
  data(world.nations)
}
\format{
  A list with two elements:

  \describe{
    \item{\code{table}}{A dataframe giving a table of world nations by 
        name, country code and region in the three columns respectively.}

    \item{\code{border}}{A dataframe with the coordinates of the borders 
        of each nation. Three columns: Column \code{"index"} is an indicator
        of the country the border point belongs to. This index is the line
        number of the country in \code{world.nations$table}. The columns
        \code{lon} and \code{lat} are the geographic longitude/latitude
        of the border points. The dataframe carries attributes for the 
        coordinate system. }
  }

}
\details{
}
\source{
}
\references{ 
}
\examples{
data(world.nations)
}
\keyword{datasets}
