`warm2cool.colors` <-
function (n, middle="#FFFFFF") {
# a sequence of colors from full red to light yellow, white, 
# light cyan to full blue. n is the number of colors to return.
# If n is uneven the middle color is white
    n <- as.integer(n[1])
    if (n > 0) {
        j <- n%/%2
        c(if (j > 0) heat.colors(j),
          if (2*j < n) c(middle),
          if (j > 0) cool.colors(j))
    }
    else character(0)
}

