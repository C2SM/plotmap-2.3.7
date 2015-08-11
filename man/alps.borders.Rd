\name{alps.borders}
\alias{alps.borders}
\alias{alps.rivers}
\alias{alps.lakes}
\docType{data}
\title{Geographic outlines of the alpine region}
\description{
  Dataframes with geographic data on borders, rivers and lakes in 
  the alpine region. These datasets can be used to draw geographic outlines 
  in maps using \code{lines}.
}
\usage{
  data(alps.borders)
  data(alps.rivers)
  data(alps.lakes)
}
\format{
  Data frames with 4 columns. Rows specify points of the border, rivers or lakes.
  \describe{
    \item{\code{type}}{The type of point. 
            type="LG" : Landesgrenzen, borders, 
            type="RV" : Fluesse, rivers,
            type="LK" : Seen, lakes
          }
        \item{\code{lon}}{Longitude}
    \item{\code{lat}}{Latitude}
  }
}
\details{
    }
\source{
  Source: modified data. Original:(c) all vector data by AND Data Solutions, B.V. (borders and lakes); CIA World
  Data Bank II (http://www.evl.uic.edu/pape/data/WDB/)(rivers).

}
\references{
  
}
\examples{
data(alps.borders)
data(alps.rivers)
data(alps.lakes)
}
\keyword{datasets}
