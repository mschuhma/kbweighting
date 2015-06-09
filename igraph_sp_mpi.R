# This example comes from an old version of the Wikipedia article on
# bootstrapping, which actually included R code.  The task being executed
# in the foreach loop executes very quickly, but it does a lot of trials.
# That makes it a good example of the need for the chunkSize option.

###### Setting up libs ##########
#Function to check whether package is installed
  is.installed <- function(mypkg){
    is.element(mypkg, installed.packages()[,1])
  }
dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)

library(doMPI)
if (!is.installed("igraph")){
  install.packages("igraph", dependencies=TRUE, Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}
library(igraph)
if (!is.installed("plyr")){
  install.packages("plyr", dependencies=TRUE, Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}
library(plyr)

###### #Loading data ########
graph_file <- "data/eta-dataset-joern-hess.data.txt.dbpgraph.sample"

#weighting functions
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
  return(max_x - x)
}


####### #Load graph
message("loading data")
input <- read.csv(graph_file, header=F, sep = "", stringsAsFactors=F)
g <- graph.data.frame(input[,1:2], directed=TRUE)
weights <- input[,3:3]


#Weighting edges
message("computing edge weights")
cnt <- count(weights)
n <- sum(cnt$freq)
weights_idf <- sapply(weights, idf, c(cnt, n))
weights_idf <- norm_weights(weights_idf)
write.csv(weights_idf, file="data/edge_weights_idf.data")
E(g)$weight <- weights_idf
message("edge weights written to data/edge_weights_idf.data")


###########################################################
# Create and register an MPI cluster
#message("register MPI cluster")
#cl <- startMPIcluster()
#registerDoMPI(cl)
message(sprintf('Parallel time using doMPI on %d workers', getDoParWorkers()))


######## Shortest path ##########

#foreach (i in seq(1, length(V(g))-1, 1)) {
#  foreach (j in seq(i+1, length(V(g)), 1)) %dopar% {

rs <- foreach (i=1:length(V(g))-1, .inorder=FALSE, .packages="igraph") %dopar% {
       foreach (j=i+1:length(V(g)), .inorder=FALSE, .packages="igraph") %dopar% {
         n1 <- V(g)[i]
         n2 <- V(g)[j]
         if (length(n1$name)*length(n2$name) > 0) {
           val <- shortest.paths(g, v=n1, to=n2, mode="all");
           r <- c(n1, n2, n1$name, n2$name, val)
           write(r, file="data/all_shortest_paths.data", append=TRUE, ncolumns=5)
         }
      }
  }

message("done")
# Shutdown the cluster and quit
#closeCluster(cl)
#mpi.quit()
