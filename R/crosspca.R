
#' Find a mapping of columns from cols into refcols that most preserves similarity.
#'
#'
#' @param cols matrix
#' @param refcols matrix nrow(cols)==nrow(refcol), ncol(cols)>=ncol(refcols)
#' @return mapping perm of cols to refcols s.t. cols[,perm[j]] ~ refcols[,j] j in seq_len(refcols)
#'
#' @examples
#'
#' x <- USArrests
#' perm <- c(2,3,4,1)
#' x2 <- x[,perm]
#' mp <- crosspca:::mapcols(x2,x) # should be inverse permutation.
#'
#' @importFrom lpSolve lp.assign
#'
#' @export
mapcols <- function(cols,refcols) {
  nc <- ncol(cols)
  nr <- ncol(refcols)
  if(nc<nr) {
    stop("nc<nr in mapcols")
  }
  if(nrow(cols)!=nrow(refcols)) {
    stop("nrows don't match in mapcols")
  }
  if((nc<=1)||(nrow(cols)<=0)) {
    return(seq_len(nc))
  }
  matchBonus <- matrix(data=0,nrow=nc,ncol=nc)
  for(i in seq_len(nc)) {
    ci <- sum(cols[,i]^2)
    for(j in seq_len(nr)) {
      rj <- sum(refcols[,j]^2)
      matchBonus[i,j] <- (sum(cols[,i]*refcols[,j])^2)/(ci*rj)
    }
  }
  soln <- lpSolve::lp.assign(matchBonus,direction='max')
  res <- vapply(seq_len(nr),function(i) { which.max(soln$solution[,i])},
                numeric(1))
  res
}



#' @title compute principal projection simulating out of sample data.
#' @description compute principal projection simulating out of sample data.
#'
#' @param x matrix to perfrom principal components analysis on.
#' @param k integer number of components to estimate.
#' @param crossplan list of lists which have train and app slots, where app is a partition of seq_len(nrow(x)).
#' @param center logical controls centering of data.
#' @param scale. logical controls scaling of data.
#' @return training (simulated out of sample projected) matrix and projection.
#'
#'
#' @importFrom vtreat kWayCrossValidation
#'
#'
#' @examples
#'
#'
#' set.seed(23552)
#' splitPlan <- vtreat::kWayCrossValidation(nrow(USArrests),3,NULL,NULL)
#' dc <- xprcomp(USArrests,k=2,crossplan=splitPlan,center=TRUE,scale.=TRUE)
#'
#' @export
#'
xprcomp <- function(x,k,crossplan,center,scale.) {
  if(missing(x)) {
    stop("xprcomp missing argument x")
  }
  if(missing(k)) {
    stop("xprcomp missing argument k")
  }
  if(missing(crossplan)) {
    stop("xprcomp missing argument crossplan")
  }
  if(missing(center)) {
    stop("xprcomp missing argument center")
  }
  if(missing(scale.)) {
    stop("xprcomp missing argument scale.")
  }
  x <- as.matrix(x)
  k <- min(k,nrow(x),ncol(x))
  subk <-  min(2*k,nrow(x),ncol(x))
  p <- sprcomp(x,k,center,scale.) # overall operator
  res <- matrix(nrow(x)*k,nrow=nrow(x),ncol=k)
  colnames(res) <- colnames(p$rotation)
  rownames(res) <- rownames(x)
  mappings <- vector(length(crossplan),mode='list')
  sii <- 1
  for(si in crossplan) {
    traini <- si$train
    appi <- si$app
    pi <- sprcomp(x[traini,],subk,center,scale.)
    predi <-  predict.sprcomp(pi,x[appi,])
    colsel <- mapcols(pi$rotation,p$rotation)
    mappings[[sii]] <- colsel
    res[appi,] <- predi[,colsel]
    sii <- sii + 1
  }
  list(train=res,p=p,mappings=mappings)
}
