\name{swiss.border}
\alias{swiss.border}
\alias{swiss.rivers}
\alias{swiss.lakes}
\docType{data}
\title{ Geographic outlines of Switzerland}
\description{
  Dataframes with geographic data on borders, rivers and lakes in 
  Switzerland. These datasets can be used to draw geographic outlines 
  in maps using \code{lines}.
}
\usage{
  data(swiss.border)
  data(swiss.rivers)
  data(swiss.lakes)
}
\format{
  Data frames with 4 columns. Rows specify points of the border, rivers or lakes.
  \describe{
    \item{\code{type}}{The type of point. 
            type="LG" : Landesgrenze, border, 
            type="RV" : Fluesse, rivers,
            type="LK" : Seen, lakes
          }
    \item{\code{name}}{A name of the point:
         Names of \code{swiss.border}:
            name="CH" polygon of swiss border including Liechtenstein
            name="LI" border of Liechtenstein to Switzerland
         Names of \code{swiss.rivers}:
            name="RE" Rhein
            name="IN" Inn
            name="RE" Rhein
            name="TH" Thur
            name="LT" Lindt
            name="LI" Limmat
            name="RS" Reuss
            name="AA" Aare
            name="SA" Sahne
            name="RO" Rhone
            name="TC" Ticino
         Names of \code{swiss.lakes}:
            name="BO" Bodensee
            name="UN" Untersee
            name="WA" Walensee
            name="ZR" Zuerichsee
            name="VW" Vierwaldstettersee
            name="ZU" Zugersee
            name="NC" Neuenburgersee
            name="BI" Bielersee
            name="MU" Murtensee
            name="GR" Lac Gruyere
            name="TU" Tunersee
            name="BZ" Brienzersee
            name="LE" Genfersee
            name="LM" Lago Maggiore
            name="LL" Lago Lugano
            name="CO" Lago Colmo
         }
    \item{\code{chx}}{The easting value in swiss coordinates}
    \item{\code{chy}}{The northing value in swiss coordinates}
  }
}
\details{
    An attribute named \code{"grid.type"} with value \code{"swisscors"}
    accompanies the data objects which makes it more easy to work with
    coordinate transformations of package \pkg{geocors}.   
}
\source{
  Source unclear. Data obtained by Christian Haeberli
}
\references{
  
}
\examples{
data(swiss.border)
data(swiss.rivers)
data(swiss.lakes)
}
\keyword{datasets}
