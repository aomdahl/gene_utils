#long_dat <- data.frame("key1" = c("a", "b", "c","a", "b", "c","a", "b", "c"),"key2" = c("a", "a", "a", "b", "b", "b","c", "c", "c"), "values" = 1:9)

listToMatrix <- function(long_dat, symmetric = TRUE)
{
  possible.keys <- unique(c(long_dat[,1],long_dat[,2]))
  out.mat <- matrix(NA, nrow = length(possible.keys), ncol = length(possible.keys))
  rownames(out.mat) <- possible.keys
  colnames(out.mat) <- possible.keys
  for(i in 1:nrow(long_dat))
  {
    r <- long_dat[i,1]
    c <- long_dat[i,2]
    out.mat[r,c] <- long_dat[i,3]
  }
  if(symmetric & any(is.na(out.mat)))
  {
    #fill in missing
    for(i in 1:nrow(out.mat))
    {
      for(j in 1:ncol(out.mat))
      {
        if(is.na(out.mat[i,j]))
        {
          out.mat[i,j] <- out.mat[j,i]
        }
      }
    }
  }
  if(symmetric)
  {
    if(!isSymmetric(out.mat))
    {
      message("Matrix doesn't appear to be symmetric. Sorry pal.")
    }
  }
  return(out.mat)
}

