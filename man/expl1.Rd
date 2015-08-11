\name{expl1}
\alias{expl1}
\alias{expl2}
\alias{expl3}
\alias{expl4}
\alias{expl5}
\alias{expl6}
\alias{expl7}
\alias{expl8}
\alias{expl9}
\alias{expl10}
\docType{data}
\title{ Example datasets }
\description{
  Datasets to illustrate plotting with plot.map. All the fields
  are associated with attributes specifying the coordinate systems and
  coordinates of the grid.

  expl1: A gridded analysis of 72-hour precipitation (mm) in 
      Switzerland on August 20-22. Conventional lon-lat grid.

  expl2: A climate change scenario of summer-time changes in
      heavy precipitation as simulated by a regional climate model
      for Europe. On a rotated lon-lat grid.

  expl3: A climate change scenario of winter-time changes in
      heavy precipitation as simulated by a regional climate model
      for Europe. On a rotated lon-lat grid.

  expl4: Statistical significance of increase in \code{expl3}. Same grid
      structure. 

  expl5: Statistical significance of decrease in \code{expl3}. Same grid
      structure. 


  expl6: Monthly precipitation in Switzerland in Oct 2005. Percentage of
      the norm value (1961-1990). Gridded on a 2x2 km longitude-latitude
      grid. Grid is filled with NAs outside the swiss border.

  expl7: Temperature Bias of a General Circulation Model. A global field
      with a resolution of 45/16 (i.e. 2.8125 degrees) resolution
      in longitude-latitude coordinates.

  expl8: Instataneous precipitation rate in Switzerland on 21. August 
      2005 18 UTZ, as measured by the swiss precipitation radar network. 
      The field is given on a one-kilometer grid in the Swiss geographic 
      coordinate system.

  expl9: Mean precipitation rate in the alpine region in 1999. 
      The field is given on a twenty-kilometer grid.

  expl10: Station coordinates to create an example plot using 
       plot.alps.map (empty plot with a star for each station position).
}
\usage{
  data(expl1)
  data(expl2)
}
\format{
  Formats are similar but slightly different between datasets

  \code{expl1} is a 241x103 matrix of values. Attribute "lon" 
          gives geographical longitude, attribute "lat" gives latitudes 
          (in degrees).

  \code{expl2} is a 90x95 matrix of values. Attribute "lon" 
          gives geographical longitude in rotated coordinates, attribute 
          "lat" gives "rotated" latitudes (in degrees). Attributes "plon" 
          and "plat" are the coordinates of the north pole of the 
          coordinate system in geographic longitue/latitude coordinates.

  \code{expl3} same as \code{expl2}.

  \code{expl4, expl5} same as \code{expl2} but the array is composed of
          0 (not significant) and 1 (significant at 5%) only.

  \code{expl6} is a 241x103 matrix of values. Attribute "lon" 
          gives geographical longitude, attribute "lat" gives latitudes 
          (in degrees).

  \code{expl7} is a 128x64 matrix representing a grid in ordinary
          longitude-latitude coordinates spanning the entire globe. 

  \code{expl8} is a 367x239 matrix representing a grid in swiss
          coordinates. 

  \code{expl9} is a 64x40 matrix representing a grid in lonlat
          coordinates. 

  \code{expl10} is a 10251x3 matrix representing the coordinates of 
          stations to create a station plot (code,lon,lat).

}
\details{

}
\source{
  Examples from Chrigel's works. expl7 was contributed by David Masson. 
  expl9 and expl10 from Frei and Schar 1998 adapted by Francesco Isotta.
}
\references{
  
}
\examples{
data(expl1)
dim(expl1)
attr(expl1,"lon")
attr(expl1,"lat")
attr(expl1,"grid.type")

data(expl2)
dim(expl2)
attributes(expl2)
}
\keyword{datasets}
