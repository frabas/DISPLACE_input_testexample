


 # CALLED FROM DISPLACE when dyn_pop_sce.option(Options::avai_updater_on)

 args <- commandArgs(trailingOnly = TRUE)

##args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
    print("No arguments supplied to input2avaiupdater.r Take pop 0 and tstep 0 and sce baseline and simu simu2")
 pop   <-0
 tstep <-0
 sce   <- "avaiupdating"
 sim   <- "simu2"
 igraph <- 56
 }else{
   pop   <- args[1]
   tstep <- args[2]
   sce   <- args[3]
   sim   <- args[4]
   igraph <- args[5]
}



 general <- list()

 general$application           <- "testexample"
 general$igraph                <- igraph
 print(general$application)

 if(.Platform$OS.type == "unix") {
  general$main_path         <- file.path("~","ibm_vessels",paste("DISPLACE_input_", general$application, sep=''))
  general$output_path         <- file.path("~","DISPLACE_outputs",general$application)
  }
 if(.Platform$OS.type == "windows") {
    general$main_path         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
    general$output_path         <- file.path("C:","DISPLACE_outputs", general$application)
  }


 ###------------------------------------------###
 ### LOAD EXTERNAL SURVEY OR COMMERCIAL DATA ####
 ###------------------------------------------###
 obj <- data.frame(Survey=NA, Year=c(2014,2014), ShootLon=c(18.79,11.0450), ShootLat=c(55.8667,54.0875),
                      Stock="COD.2224", StockId=10, nb_indiv.0=100, nb_indiv.1=10, nb_indiv.2=1,
                      nb_indiv.3=1, nb_indiv.4=1, nb_indiv.5=1, nb_indiv.6=1, nb_indiv.7=1, nb_indiv.8=1, nb_indiv.9=1,
                      nb_indiv.10=1, nb_indiv.11=1, nb_indiv.12=1, nb_indiv.13=1)  # caution: THE FORMAT IS NOT FLEXIBLE AT ALL

 
 # ...or a more realistic input file:
 if(TRUE) obj <- read.table(file.path(general$main_path, paste("popsspe_", general$application, sep=''), 
                                  "static_avai", "input_file_for_displace_merger.csv"), sep=";", header=TRUE)
 


 ###------------------------------------------###
 ### ... OR FEEDBACK WITH DISPLACE OUTCOMES ####
 ###------------------------------------------###
 #...or the one from the previous catches in the simu:
 # BUT CAUTION ONLY VALID TO INFORM THE BIGGER SZGROUPS...
   # 1. Use sqlite
 #   library(DBI)
 #   library(RSQLite)
 #   con <- dbConnect(RSQLite::SQLite(), file.path(general$output_path, sce, 
 #                                        paste(general$application, "_", sim, "_out", ".db", sep='')))
 #   dbListTables(con)
 #   head(dbReadTable(con, "VesselLogLike"))
 #   head(dbReadTable(con, "VesselDef"))
 #   head(dbReadTable(con, "VesselLogLikeCatches"))

 #   res <- dbSendQuery(con, "SELECT TStep,SUM(Catches) FROM VesselLogLike JOIN VesselDef ON Id = VesselId JOIN VesselLogLikeCatches ON RowId = LoglikeId WHERE Nationality = 'ITA' GROUP BY TStep")
 #   dbFetch(res, n= -1)
 #   dbClearResult(res)
  
  # 2. Use output text file:
  filename <- file.path(general$output_path,  sce,
         paste("vmslikefpingsonly_", sim,".dat", sep=''))
  suppressWarnings(
   obj <- read.table(filename, sep=" ", header=FALSE)
   )
   
  #x <-count.fields(filename, sep=" ", skip=2)
  #incorrect <- which(x != 23)
  
  obj <- obj[,1:23]
  colnames (obj) <-  c("tstep","Survey","start_trip_tstep","ShootLon","ShootLat","course","cumfuelcons","state",
                       "StockId","nb_indiv.0","nb_indiv.1","nb_indiv.2","nb_indiv.3","nb_indiv.4","nb_indiv.5",
                       "nb_indiv.6","nb_indiv.7","nb_indiv.8","nb_indiv.9",
                       "nb_indiv.10","nb_indiv.11","nb_indiv.12","nb_indiv.13")
    # note that combining vessel_name and start_trip_tstep will give a trip id.
    # note that catches here are landings+discards
    # note that szgroup_xx in vmslikefpingsonly_xx.dat are actually in weight, not in numbers (TODO: adapt)
     obj$Year <- "2015"
     obj$Stock <- "a_stock"
  
  # keep the last three months info only  
  obj <- obj[as.numeric(as.character(obj$start_trip_tstep)) > (as.numeric(as.character(tstep)) - (745*3)),]  
    
  idx_zeros <- which( apply(obj[,paste("nb_indiv.", 0:13, sep="")], 1, sum) ==0) 
  obj <- obj[-idx_zeros, ] # assume 0s like absence of targetting, then get rid of them 
  obj <- obj[complete.cases(obj), ] # debug for c++

  
 # because we lack of info on smaller fish, retireve from the existing...
 obj <- obj[,c("Survey", "Year", "ShootLon", "ShootLat", "Stock", "StockId", paste("nb_indiv.", 0:13, sep=""))]  

 coord <- read.table(file=file.path(general$main_path, "graphsspe",
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
 coord <- as.matrix(as.vector(coord))
 coord <- matrix(coord, ncol=3)
 coord <- cbind(coord, 1:nrow(coord))
 colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
 
 for (st in unique(obj$StockId)){
      a_semester <- ceiling(as.numeric(tstep) / 4382 ) %% 2
      existing_avai_this_sp <- read.table(file.path(general$main_path, paste("popsspe_", general$application, sep=''), 
                                  "static_avai", paste(st, "spe_full_avai_szgroup_nodes_semester",a_semester,".dat", sep="")),
                                   sep=" ", header=TRUE)
      existing  <- cbind(unique(existing_avai_this_sp[,1]), matrix(existing_avai_this_sp[,-1], ncol=14, byrow=T)) # reshape in wide
      existing_non_null_small_fish <- existing [existing[,2] >1e-5 & existing[,3] >1e-5, ]
      colnames(existing_non_null_small_fish) <- c("pt_graph",
                       "nb_indiv.0","nb_indiv.1","nb_indiv.2","nb_indiv.3","nb_indiv.4","nb_indiv.5",
                       "nb_indiv.6","nb_indiv.7","nb_indiv.8","nb_indiv.9",
                       "nb_indiv.10","nb_indiv.11","nb_indiv.12","nb_indiv.13")
      
      obj <- rbind.data.frame (obj, 
               cbind.data.frame(Survey="existing", Year="2015",
                                ShootLon=coord[existing_non_null_small_fish[,"pt_graph"], "x"],
                                ShootLat=coord[existing_non_null_small_fish[,"pt_graph"], "y"], 
                                Stock="a_stock", StockId=st,
                                existing_non_null_small_fish[,-1]
                                )
               )
  }




  # TO DO:
  # FOR MSPTOOLS COUPLING. 
  # CALL LGCP-MSPTOOLS APPLIED ON DISPLACE HAUL BY HAUL OUTPUT.
  # LGCP-MSPTOOLS OUTPUT A FIELD EVERY t CONVERTED IN AN INPUT FILE FOR THE AVAIFIELDUPDATER 
  # (REMEMBER CONVERTING AGE CATEGORIES, IF ANY, INTO LENGTH GROUPS)
  #...
  
  

 # DO NOT ALTER FOLLOWING THIS LINE-------------------
 obj <- obj[,c("Survey", "Year", "ShootLon", "ShootLat", "Stock", "StockId", paste("nb_indiv.", 0:13, sep=""))]   # caution: THE FORMAT IS NOT FLEXIBLE AT ALL

 # make lighter data for the testing purpose....
 obj <-  obj[obj$StockId %in% pop,]
 
 if(nrow(obj)==0) stop(paste("This StockId ", pop, " is not in the displace_input_for_data_merger.csv file!!"))
 
 write.table(obj, file= file.path( general$main_path, paste("popsspe_", general$application, sep=''), "static_avai",
             paste("displace_input_for_data_merger_",tstep,".dat", sep='')), sep=" ", row.names=FALSE, quote=FALSE)   ## CAUTION file is .dat and sep is white space




