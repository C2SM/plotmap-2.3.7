`cmsaf.col` <- 
function () {
    data(cmsaf.cols)
    rgb(red=cmsaf.cols[,"r"],
        green=cmsaf.cols[,"g"],
        blue=cmsaf.cols[,"b"],maxColorValue=255)
}


