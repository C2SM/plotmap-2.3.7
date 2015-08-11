\name{swiss.catch.expl}
\alias{swiss.catch.expl}
\docType{data}
\title{ Catchment data in Switzerland}
\description{
  An example dataset for demonstration of \code{plot.poly.map}. The dataset
  comprises areas and polygon lines for a selection of 50 hydrological 
  catchments in Switzerland.
}
\usage{data(swiss.catch.expl)}
\format{
  The format is a list with two elements:

 $ area   : A vector with the areas of the catchments in square kms.
            The length of the vector corresponds to the number of 
            catchments (polygons).

 $ polygon: A list with the catchment coordinates. Each list element is
            a list with elements lon and lat comprising vectors with 
            the geographic longitude and latitude respectively. The order
            of catchments is the same like in element \code{area}. This
            list element has geographical coordinate attributes attached.
        
}
\details{
  
}
\source{
  Polygons of catchments are from University of Berne and the Swiss 
  Federal Office for the Environment. 
}
\references{
  
}
\examples{
data(swiss.catch.expl)
}
\keyword{datasets}
