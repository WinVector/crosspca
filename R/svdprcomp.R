


# try to choose a nice sign correction
chooseSignCorrection <- function(x) {
  if((length(x)<=0) || all(x==0)) {
    return(1)
  }
  # first try: make sum(x) positive
  v <- sign(sum(x))
  if(v!=0) {
    return(v)
  }
  # next try: make most non-zero positions positive
  v <- sign(mean(sign(x)))
  if(v!=0) {
    return(v)
  }
  # last try: make first non-zero position positive
  v <- sign(x[min(which(x!=0))])
  if(v!=0) {
    return(v)
  }
  # give up
  1
}

# see http://stats.stackexchange.com/questions/87307/predict-only-the-first-n-principal-components-in-a-pca-analysis


#' @title compute principal components using efficient svd.
#' @description use svd package to speed up calculation.
#'
#' @param x matrix to perfrom principal components analysis on.
#' @param k integer number of components to estimate.
#' @param center logical controls centering of data.
#' @param scale. logical controls scaling of data.
#' @return svd based prcomp only up to k singular values.
#'
#' @examples
#'
#' p <- sprcomp(USArrests,k=2,center=TRUE,scale.=TRUE)
#' predict(p,newdata=USArrests)
#'
#' @export
#' @importFrom svd propack.svd
#'
sprcomp <- function(x,k,center,scale.) {
  if(missing(x)) {
    stop("sprcomp missing argument x")
  }
  if(missing(k)) {
    stop("sprcomp missing argument k")
  }
  if(missing(center)) {
    stop("sprcomp missing argument center")
  }
  if(missing(scale.)) {
    stop("sprcomp missing argument scale.")
  }
  x <- as.matrix(x)
  k <- min(k,nrow(x),ncol(x))
  cent <- FALSE
  scal <- FALSE
  if(center || scale.) {
    xs <- scale(x,center=center,scale=scale.)
    if(center) {
      cent <- attr(xs,"scaled:center")
    }
    if(scale.) {
      scal <- attr(xs,"scaled:scale")
    }
  } else {
    xs <- x
  }
  if(requireNamespace('svd',quietly = TRUE)) {
    # this call should only compute the first k singular values.
    svd <- svd::propack.svd(xs,neig=k)
  } else {
    svd <- svd(xs,nu=0,nv=k)
  }
  rot <- svd$v
  sv <- svd$d
  rownames(rot) <- colnames(x)
  colnames(rot) <- paste0('PC',seq_len(ncol(rot)))
  # standardize signs a bit
  for(i in seq_len(ncol(rot))) {
    rot[,i] <- rot[,i] * chooseSignCorrection(rot[,i])
  }
  p <- list(sdev=sv/sqrt(nrow(x)-1),
       rotation=rot,
       center=cent,scale=scal)
  class(p) <- 'sprcomp'
  p
}

#' @title apply principal component projection/rotation.
#' @description applies rotation returned by sprcomp to new data.
#'
#' @param object sprcomp object.
#' @param newdata new matrix to apply to.
#' @param ... not used, declared to match S3 object signature.
#' @return projected data.
#'
#'
#' @examples
#'
#' p <- sprcomp(USArrests,k=2,center=TRUE,scale.=TRUE)
#' predict(p,newdata=USArrests)
#'
#' @export
predict.sprcomp <- function(object,newdata,...) {
  if(missing(object)) {
    stop("predict.sprcomp missing argument object")
  }
  if(class(object)!='sprcomp') {
    stop("predict.sprcomp argument object must be of class sprcomp")
  }
  if(missing(newdata)) {
    stop("predict.sprcomp missing argument newdata")
  }
  args <- list(...)
  if(length(args)!=0) {
    nm <- setdiff(paste(names(args),collapse=", "),'')
    nv <- length(args)-length(nm)
    stop(paste("unexpected arguments",nm,"(and",nv,"unexpected values)"))
  }
  ns <- scale(as.matrix(newdata),
              center=object$center,
              scale=object$scale)
  ns %*% object$rotation
}

