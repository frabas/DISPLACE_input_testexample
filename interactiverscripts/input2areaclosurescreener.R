
 # CALLED FROM DISPLACE when dyn_pop_sce.option(Options::area_closure_screener)

 args <- commandArgs(trailingOnly = TRUE)

##args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
    print("No arguments supplied to input2AreaClosureScreener. Take igraph 411 and tstep 0")
 igraph   <- 411
 tstep    <- 0
 }else{
   igraph   <- args[1]
   tstep    <- args[2]
}




## CALLED FROM DISPLACE if dyn_pop_sce.option(Options::area_closure_screener)

 general <- list()

 general$application           <- "DanishFleet"

 if(.Platform$OS.type == "unix") {
  general$main_path         <- file.path("~","ibm_vessels",paste("DISPLACE_input_", general$application, sep=''))
  general$igraph            <- 411
  }
 if(.Platform$OS.type == "windows") {
    general$main_path         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
    general$igraph          <- 411
  }


 # first, load the graph
  coord <- read.table(file=file.path(general$main_path, "graphsspe",
             paste("coord", igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  coord <- cbind(coord, 1:nrow(coord))
  colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')

 # then, load the current closed nodes settings and restrict the graph to it
  current      <- read.table (file=file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_DanishFleet","graphsspe","metier_closure_a_graph411_month1.dat"), header=FALSE)
  coord_in_pol <- coord[coord [,"pt_graph"] %in% (as.numeric(current[,2])+1), ]  # caution: +1 because offset in c++ input file

  # attach a grid to it
  xrange <-  range(coord_in_pol[,'x'])
  yrange <-  range(coord_in_pol[,'y'])

  resx <- resy <- 3/60

  library(vmstools)
  grd <- createGrid(xrange,yrange,resx=resx,resy=resy,type="SpatialGrid",exactBorder=TRUE)

  # Grid all tacsatSweptArea data
  # Convert all tacsat poins first to SpatialPoints
  coords <- SpatialPoints(cbind(x=as.numeric(as.character(coord_in_pol[,'x'])),y=as.numeric(as.character(coord_in_pol[,'y']))))
  idx <- over(coords, grd)
  coord_in_pol <- cbind(coord_in_pol, idx)

  # Remove records that are not in the study area
  coord_in_pol <- coord_in_pol[!is.na(coord_in_pol[,"idx"]),]

  # Add midpoint of gridcell to dataset
  coord_in_pol <- cbind(coord_in_pol, CELL_LONG=coordinates(grd)[coord_in_pol[,"idx"],1], CELL_LATI=coordinates(grd)[coord_in_pol[,"idx"],2])

  # a quick check
  plot(coord_in_pol[,"x"], coord_in_pol[,"y"], col=coord_in_pol[,"idx"])


  # finally, create one screened_polygon_nodes.dat and corresponding scenario file per grid cell!!
  # TO DO....



