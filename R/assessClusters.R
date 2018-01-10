#' assessClusters
#'
#' @param y the feature matrix for assessing clusters
#' @param maxCluster the maximum number of clusters allowed
#'
#' @return groups, the hierarchical cluster each time series group is assigned
#' @export
#'
#' @examples
#'
#' groups <- assessClusters(y)
#'
assessClusters <- function(y, maxCluster){

  hc <- hclust(dist(y, method = "euclidean"), method="ward.D")

  groups <- cutree(hc,maxCluster)

  return(groups)

}
