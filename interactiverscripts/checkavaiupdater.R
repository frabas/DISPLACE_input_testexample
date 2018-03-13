

 # TO CHECK THE OUTCOME OF DISPLACE dyn_pop_sce.option(Options::avai_updater_on)

 general <- list()

 general$application           <- "testexample"

 if(.Platform$OS.type == "unix") {
  general$main_path         <- file.path("~","ibm_vessels",paste("DISPLACE_input_", general$application, sep=''))
  }
 if(.Platform$OS.type == "windows") {
    general$main_path         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
    general$igraph         <- 56
  }


  # load a graph
 coord <- read.table(file=file.path(general$main_path, "graphsspe",
           paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
 coord <- as.matrix(as.vector(coord))
 coord <- matrix(coord, ncol=3)
 colnames(coord) <- c('x', 'y', 'dist')
 #plot(coord[,1], coord[,2])
 coord <- cbind(coord, 1:nrow(coord)) # keep track of the idx_node

 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 stockId <- 1
 szgroup <- 4

 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 # ...Research survey input file:
obj <- read.table(file.path(general$main_path, paste("popsspe_", general$application, sep=''),
                                  "static_avai", "displace_input_for_data_merger_7.dat"), sep=" ", header=TRUE)


#  ...avaifieldupdater output files
obj2 <- read.table(file.path(general$main_path, paste("popsspe_", general$application, sep=''),
                                  "static_avai", paste(stockId,"spe_full_avai_szgroup_nodes_semester1_updated.dat", sep="")), sep=" ", header=TRUE, row.names=NULL)
colnames(obj2) <- c("idx_node", "avai", "szgroup")
obj2$szgroup <- rep(0:13, length =nrow(obj2))
obj2$x <- coord[as.numeric(as.character(obj2$idx_node))+1, "x"]
obj2$y <- coord[as.numeric(as.character(obj2$idx_node))+1, "y"]


#  ...initial displace input avai files
obj3 <- read.table(file.path(general$main_path, paste("popsspe_", general$application, sep=''),
                                  "static_avai", paste(stockId,"spe_full_avai_szgroup_nodes_semester1.dat", sep="")), sep=" ", header=TRUE, row.names=NULL)
colnames(obj3) <- c("idx_node", "avai")
obj3$szgroup <- rep(0:13, length =nrow(obj3))
obj3$x <- coord[as.numeric(as.character(obj3$idx_node))+1, "x"]
obj3$y <- coord[as.numeric(as.character(obj3$idx_node))+1, "y"]



 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

# plot the data and compare
subobj <- obj[obj$StockId==stockId,]
library(maps)
plot(subobj$ShootLon, subobj$ShootLat, cex=sqrt(subobj [,paste("nb_indiv.",szgroup,sep='')] *4/max(subobj [,paste("nb_indiv.",szgroup,sep='')])), pch=16, col="green")
map(add=TRUE, col=grey(0.4))



subobj2 <-  obj2[obj2$szgroup==szgroup,]
points(subobj2$x, subobj2$y, cex=sqrt(subobj2$avai *4/max(subobj2$avai)), col="red")

subobj3 <-  obj3[obj3$szgroup==szgroup,]
points(subobj3$x, subobj3$y, cex=sqrt(subobj3$avai *4/max(subobj3$avai)), col="blue")


 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

 # basic check
     sum(subobj2$avai)  # should return 1
     sum(subobj3$avai)  # should return 1


