### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx",
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
               outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           grid::pushViewport(grid::viewport(width=grid::unit(1, "npc") -
                              grid::unit(1, "lines"), x=0, just="left"))
           grid::grid.text(sprintf("help(\"%s\")", nameEx()),
                           x=grid::unit(1, "npc") + grid::unit(0.5, "lines"),
                           y=grid::unit(0.8, "npc"), rot=90,
                           gp=grid::gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
## at least one package changes these via ps.options(), so do this
## before loading the package.
## Use postscript as incomplete files may be viewable, unlike PDF.
## Choose a size that is close to on-screen devices, fix paper
grDevices::ps.options(width = 7, height = 7, paper = "a4", reset = TRUE)
grDevices::postscript("geosphere-Ex.ps")

assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
options(warn = 1)
options(pager = "console")
library('geosphere')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("alongTrackDistance")
### * alongTrackDistance

flush(stderr()); flush(stdout())

### Name: alongTrackDistance
### Title: Along Track Distance
### Aliases: alongTrackDistance
### Keywords: spatial

### ** Examples

alongTrackDistance(c(0,0),c(90,90),c(80,80))



cleanEx(); nameEx("antipode")
### * antipode

flush(stderr()); flush(stdout())

### Name: antipode
### Title: Antipodes
### Aliases: antipode isAntipodal
### Keywords: spatial

### ** Examples

antipode(rbind(c(5,52), c(-120,37), c(-60,0), c(0,70)))
isAntipodal(c(0,0), c(180,0))



cleanEx(); nameEx("bearing")
### * bearing

flush(stderr()); flush(stdout())

### Name: bearing
### Title: Bearing
### Aliases: bearing
### Keywords: spatial

### ** Examples

bearing(c(0,0),c(90,90))



cleanEx(); nameEx("brngRhumb")
### * brngRhumb

flush(stderr()); flush(stdout())

### Name: brngRhumb
### Title: Rhumbline bearing
### Aliases: brngRhumb
### Keywords: spatial

### ** Examples

brngRhumb(c(0,0),c(90,90))



cleanEx(); nameEx("crossTrackDistance")
### * crossTrackDistance

flush(stderr()); flush(stdout())

### Name: crossTrackDistance
### Title: Cross Track Distance
### Aliases: crossTrackDistance
### Keywords: spatial

### ** Examples

crossTrackDistance(c(0,0),c(90,90),c(80,80))



cleanEx(); nameEx("crossingParallel")
### * crossingParallel

flush(stderr()); flush(stdout())

### Name: crossingParallels
### Title: Crossing parellels
### Aliases: crossingParallels
### Keywords: spatial

### ** Examples

crossingParallels(c(5,52), c(-120,37), 40)



cleanEx(); nameEx("destPoint")
### * destPoint

flush(stderr()); flush(stdout())

### Name: destPoint
### Title: Destination given bearing and distance, when following a great
###   circle
### Aliases: destPoint
### Keywords: spatial

### ** Examples

p <- c(5,52)
d <- destPoint(p,30,10000)

#final bearing, when arriving at endpoint: 
finalBearing(d, p)




cleanEx(); nameEx("destPointRhumb")
### * destPointRhumb

flush(stderr()); flush(stdout())

### Name: destPointRhumb
### Title: Destination along a rhumb line
### Aliases: destPointRhumb
### Keywords: spatial

### ** Examples

destPointRhumb(c(0,0), 30, 100000, r = 6378137)



cleanEx(); nameEx("distCosine")
### * distCosine

flush(stderr()); flush(stdout())

### Name: distCosine
### Title: 'Law of cosines' great circle distance
### Aliases: distCosine
### Keywords: spatial

### ** Examples

distCosine(c(0,0),c(90,90))



cleanEx(); nameEx("distHaversine")
### * distHaversine

flush(stderr()); flush(stdout())

### Name: distHaversine
### Title: 'Havesine' great circle distance
### Aliases: distHaversine
### Keywords: spatial

### ** Examples

distHaversine(c(0,0),c(90,90))



cleanEx(); nameEx("distRhumb")
### * distRhumb

flush(stderr()); flush(stdout())

### Name: distRhumb
### Title: Distance along a 'rhumb line'
### Aliases: distRhumb
### Keywords: spatial

### ** Examples

distRhumb(c(0,0),c(90,90))



cleanEx(); nameEx("distVincentyEllipsoid")
### * distVincentyEllipsoid

flush(stderr()); flush(stdout())

### Name: distVincentyEllipsoid
### Title: 'Vincenty' (ellipsoid) great circle distance
### Aliases: distVincentyEllipsoid
### Keywords: spatial

### ** Examples

distVincentyEllipsoid(c(0,0),c(90,90))
# on a 'Clarke 1880' ellipsoid
distVincentyEllipsoid(c(0,0),c(90,90), a=6378249.145, b=6356514.86955, f=1/293.465)



cleanEx(); nameEx("distVincentySphere")
### * distVincentySphere

flush(stderr()); flush(stdout())

### Name: distVincentySphere
### Title: 'Vincenty' (sphere) great circle distance
### Aliases: distVincentySphere
### Keywords: spatial

### ** Examples

distVincentySphere(c(0,0),c(90,90))



cleanEx(); nameEx("finalBearing")
### * finalBearing

flush(stderr()); flush(stdout())

### Name: finalBearing
### Title: Final bearing
### Aliases: finalBearing
### Keywords: spatial

### ** Examples

bearing(c(0,0),c(90,90))
finalBearing(c(0,0),c(90,90))



cleanEx(); nameEx("greatCircle")
### * greatCircle

flush(stderr()); flush(stdout())

### Name: greatCircle
### Title: Intersecting radials
### Aliases: greatCircle
### Keywords: spatial

### ** Examples

greatCircle(c(5,52), c(-120,37), n=36)



cleanEx(); nameEx("greatCircleIntermediate")
### * greatCircleIntermediate

flush(stderr()); flush(stdout())

### Name: greatCircleIntermediat
### Title: Intermediate points on a great circle
### Aliases: greatCircleIntermediate
### Keywords: spatial

### ** Examples

greatCircleIntermediate(c(5,52), c(-120,37), n=10)



cleanEx(); nameEx("greatCircleIntersect")
### * greatCircleIntersect

flush(stderr()); flush(stdout())

### Name: greatCircleIntersect
### Title: Intersections of two great circles
### Aliases: greatCircleIntersect
### Keywords: spatial

### ** Examples

p1 <- c(5,52); p2 <- c(-120,37); p3 <- c(-60,0); p4 <- c(0,70)
greatCircleIntersect(p1,p2,p3,p4)



cleanEx(); nameEx("midPoint")
### * midPoint

flush(stderr()); flush(stdout())

### Name: midPoint
### Title: Mid-point
### Aliases: midPoint
### Keywords: spatial

### ** Examples

midPoint(c(0,0),c(90,90))  



cleanEx(); nameEx("polePoint")
### * polePoint

flush(stderr()); flush(stdout())

### Name: polePoint
### Title: Highest latitude on a great circle
### Aliases: polePoint
### Keywords: spatial

### ** Examples

polePoint(c(5,52),30)



cleanEx(); nameEx("radialIntersect")
### * radialIntersect

flush(stderr()); flush(stdout())

### Name: radialIntersect
### Title: Intersecting radials
### Aliases: radialIntersect
### Keywords: spatial

### ** Examples

radialIntersect(c(10,0), 10, c(-10,0), 10)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
