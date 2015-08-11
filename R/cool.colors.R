`cool.colors` <-
function (n) {
# a sequence of cold colors from light cyan to full blue.
# n is the number of colors to return.
    if ((n <- as.integer(n[1])) > 0) {
        j <- n%/%4
        i <- n - j
        c(if (j > 0) hsv(h = 3/6, 
            s = seq(to = 1 - 1/(2 * j), from = 1/(2 * j), length = j), 
            v = 1),rainbow(i, start = 3/6, end = 4/6))
    }
    else character(0)
}

