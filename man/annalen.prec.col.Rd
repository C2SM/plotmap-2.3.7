\name{annalen.prec.col}
\alias{annalen.prec.col}
\alias{wb.prec.col}
\alias{wb.temp.col}
\alias{wb.sun.col}
\title{ Color tables added to built-in R tables }
\description{
  Color tables used for Witterungsbericht and Annalen of MeteoSwiss.
  \code{annalen.prec.col} for precip in Annalen,
  \code{wb.prec.col} for precipitation anomalies in Witterungsbericht,
  \code{wb.temp.col} for temperature anomalies in Witterungsbericht,
  \code{wb.sun.col} for sunshine duration anomalies in Witterungsbericht,
}
\usage{
annalen.prec.col()
wb.prec.col()
wb.temp.col()
wb.sun.col()
}
\arguments{
}
\details{
   Color codes provided by special event zbi
}
\value{
  An array of colors that can be used in color plotting.
}
\references{ }
\author{ Christoph Frei }
\note{  }
\seealso{ \code{\link{rainbow}}, 
          \code{\link{heat.colors}},
          \code{\link{cool.colors}},
          \code{\link{warm2cool.colors}},
          \code{\link{cm.colors}},
          \code{\link{terrain.colors}},
          \code{\link{topo.colors}},
          \code{\link{rainbow10}},
          \code{\link{rainbow9}} }
\examples{
demo.palette(n=9,ch.col=c("annalen.prec.col()", 
                       "wb.prec.col()",
                       "wb.temp.col()", 
                       "wb.sun.col()"))

}
\keyword{ color }% at least one, from doc/KEYWORDS
