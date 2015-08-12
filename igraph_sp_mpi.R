###### Setting up libs ##########
#Function to check whether package is installed
  is.installed <- function(mypkg){
    is.element(mypkg, installed.packages()[,1])
  }
dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)

library(doMPI)

if (!is.installed("igraph")){
  install.packages("igraph", dependencies=TRUE, Sys.getenv("R_LIBS_USER"), repos = "http://cran.uni-muenster.de/")
}
library(igraph)
if (!is.installed("plyr")){
  install.packages("plyr", dependencies=TRUE, Sys.getenv("R_LIBS_USER"), repos = "http://cran.uni-muenster.de/")
}
library(plyr)

####### Define functions #######

idf <- function(edge_id, edge_cnts, edge_freq_sum) {
  frq <- cnt$freq[cnt$x[edge_id]]
  idf <- log(n/frq)
  return(idf)
}

norm_weights <- function(weights) {
  max_w <- max(weights)
  return(sapply(weights, inv_x, max_w))
}

inv_x <- function(x, max_x) {
  return(1 + max_x - x)
}

####### Parameters #######

topK <- 100 # paths per node

####### Load graph #######
message("loading data")
graph_file <- gzfile("data/eta-dataset-joern-hess.data.txt.dbpgraph.gz")
input <- read.csv(graph_file, header=F, sep = "", stringsAsFactors=F)
g <- graph.data.frame(input[,1:2], directed=TRUE)
weights <- input[,3:3]

####### Create MPI cluster ######
message("register MPI cluster")
cl <- startMPIcluster()
registerDoMPI(cl)
message(sprintf('Parallel time using doMPI on %d workers', getDoParWorkers()))

####### Start real computation #######

#Weighting edges
message("computing edge weights")
cnt <- count(weights)
n <- sum(cnt$freq)
#weights_idf <- foreach (x=weights, .combine='c') %dopar% {idf(x, cnt, n)}
weights_idf <- sapply(weights, idf, c(cnt, n))
weights_idf <- norm_weights(weights_idf)
edge_file <- gzfile("data/edge_weights_idf.data.gz")
write.csv(weights_idf, file=edge_file)
E(g)$weight <- weights_idf
message("Edge weights written to data/edge_weights_idf.data.gz")

#Shortest path
message("Vertex count ", length(V(g)) )
name <- V(g)$name

foreach (i=1:(length(V(g))-1), .inorder=FALSE, .packages=c("igraph","plyr")) %dopar% {
      n1 <- V(g)[i]
      fname <- sprintf("data/all_shortest_paths_%s.gz", V(g)[i]$name)
      #if (file.exists(fname) || file.exists(sprintf("data/all_shortest_paths_%s", V(g)[i]$name)) ) {
      #  message("Skipped starting node ", i)
      #}
      #else {
       val <- shortest.paths(g, n1, to=V(g), mode="all", weights=NULL)
       rs <- cbind.data.frame(V(g)[i]$name, name, t(val))
       rs <- head(arrange(rs, rs[[3]]), n = topK)
       write.table(rs, file=gzfile(fname), quote=F, row.names=F, col.names=F)
       #write(rs, file=gzfile(fname), append=FALSE) #ncolumns=3)
       message("Finished starting node ", i)
      #}
  }


message("Finished all")

####### Cleanup #######
# Shutdown the cluster and quit
closeCluster(cl)
mpi.quit()
