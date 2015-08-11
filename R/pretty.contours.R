`pretty.contours` <-
function(...) {

  levs <- pretty(...)
  levs <- levs[-c(1,length(levs))]

  return(levs)
}
