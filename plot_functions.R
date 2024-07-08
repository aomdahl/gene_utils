#R plots data viz.
pacman::p_load(data.table, tidyr, dplyr, ggplot2, stringr, argparse, cowplot, ashr)
orderFactors <- function(f.mat, dist = "euclidean")
{
  #Get the correlation of each factor pairwise
  if(dist == "r2")
  {
    r2 <- cor(t(f.mat))^2
    #try with sparse thing
    hclust(as.dist(1-r2))$order
  }
  else
  {
    d <- dist(f.mat)
    h <- hclust(d)
    return(h$order)
  }
}
#plotFactors(apply(reg.run$V, 2, function(x) x/norm(x, "2")),trait_names = names, title = expression(paste("Leared factors: ", lambda, "=36.7")))
#toy <- matrix(rnorm(30), nrow = 10, ncol = 3)
#trait_names <- paste0("Trait",1:10)
#plotFactors(toy, trait_names, title = "test")
plotFactors <- function(F, trait_names, title, cluster = "y", order = NULL, abbrev = FALSE, colors = NULL, blacked_out = FALSE)
{
  new_names <- c(seq(1,ncol(F)), "trait")
  if(cluster == "y")
  {
    ordering <- orderFactors(F)
  }else if (cluster == "n" | cluster == "none"){
    ordering <- 1:nrow(F)
  } else if(cluster == "both")
  {
    o <- clusterBothWays(F)
    ordering <- o$x
    
  }else
  {
    message("cluster argument unrecognized. No clustering will occur.")
    ordering <- 1:nrow(F)
  }
  if(!is.null(order))
  {
    ordering <- order
  }
  factors_nn <- data.frame(F) %>% mutate("trait" = factor(trait_names, levels = trait_names[ordering]) )
  names(factors_nn) <- new_names
  nn <- tidyr::pivot_longer(factors_nn, cols = seq(1:ncol(F)), names_to = "x", values_to="value") %>%  arrange(value)
  
  if(cluster == "both")
  {
    nn$x <- factor(as.numeric(nn$x), levels=o$y)
  }else
  {
    nn$x <- as.factor(as.numeric(nn$x))
  }
  p <- ggplot(nn, aes(x, trait, fill= value)) + geom_tile(color = "gray") +  
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") + xlab("Factors") + theme_minimal(15) + ggtitle(title)
  #blacked out plot
  if(blacked_out)
  {
    nn$value <- sapply(nn$value, function(x) if(x == 0) {NA}else{x})
    p <- ggplot(nn, aes(x, trait, fill= value)) + geom_tile(color = "gray") + 
      scale_fill_gradient2(low="blue",mid = "white", high="red",na.value="black")+ xlab("Factors") + theme_minimal(15) + ggtitle(title)
  }

  if(ncol(F) > 10)
  {
    p <- p + scale_x_discrete(guide = guide_axis(n.dodge=2))
  }
  
  if(abbrev)
  {
    p <- p + scale_y_discrete(label=function(x) abbreviate(x, minlength=30))
    
  }
  if(!any(is.null(colors)))
  {
     p <- p + theme(axis.text.y = element_text(colour=colors[ordering]))
  }
  
  return(p)
}

plotLoadings <- function(L, snps, title)
{
  new_names <- c(seq(1,ncol(L)), "SNP")
  loadings <- data.frame(L) %>% mutate("SNP" = snps)
  names(loadings) <- new_names
  
  n <- tidyr::pivot_longer(loadings, cols = seq(1:ncol(L)), names_to = "x") %>%  arrange(value)
  n$x <- as.factor(as.numeric(n$x))
  p <- ggplot(n, aes(x, SNP, fill= value)) + geom_tile() +  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    xlab("Loadings") +  theme_minimal(15) + theme(axis.text.y=element_blank(), axis.ticks.y = element_blank())  + ggtitle(title)
  return(p)
}

plotWeights <- function(weights)
{
  weights = data.frame(weights)
  ggplot(data= weights, aes(x = 1:length(weights), y = weights)) + geom_point() + theme_minimal(15) + xlab("Latent component") + ylab("PVE")
}

plotFactorsBarplot <- function(F, trait_names, title, cluster = T, t_order = NA, colors = NA)
{
  new_names <- c(seq(1,ncol(F)), "trait")
  if(!is.na(t_order))
  {
    ordering <- t_order
  }else if(cluster) {
    ordering <- orderFactors(F)
  }else{
    ordering <- 1:nrow(F)
  }
  factors_nn <- data.frame(F) %>% mutate("trait" = factor(trait_names, levels = trait_names[ordering]) )
  names(factors_nn) <- new_names
  if(any(!is.na(colors)))
  {
    factors_nn$colors = colors
  }
  nn <- tidyr::pivot_longer(factors_nn, cols = seq(1:ncol(F)), names_to = "x") %>%  arrange(value)
  nn$x <- as.factor(as.numeric(nn$x))
  nn$factors <- paste0("F", nn$x)
  if(any(is.na(colors))){
    p <- ggplot(nn, aes(x = trait, y = value)) + geom_bar(stat='identity', fill  = "skyblue") + facet_wrap(~factors) + 
      theme_minimal(15) + theme(axis.text.x=element_blank()) + xlab("GWAS traits") + ylab("Factor value")
  }else
  {  p <- ggplot(nn, aes(x = trait, y = value, fill = colors)) + geom_bar(stat='identity') + facet_wrap(~factors) + 
    theme_minimal(15) + theme(axis.text.x=element_blank()) + xlab("GWAS traits") + ylab("Factor value") + labs(fill = "Group")
  }

  
  return(p + ggtitle(title))
}

clusterBothWays <- function(datin)
{
  t <- heatmap(datin)
  cormat <- datin[t$rowInd, t$colInd]
  ordery = t$colInd
  orderx = t$rowInd
  list("x" = orderx, 'y'=ordery)
}

reorder_cormat <- function(cormat, type="hclust"){
  # Use correlation between variables as distance
  if(type == "heatmap.default")
  {
    b <- clusterBothWays(cormat)
    ordery = b$y
    orderx = b$x
  } else{
    dd <- as.dist((1-(cormat^2)))
    hc <- hclust(dd)
    orderx <- hc$order
    ordery <- hc$order
    cormat <-cormat[orderx, ordery]
  }
  return(list("cormat" = cormat, "order_y" = ordery, "order_x" = orderx))
}
#plotCorrelationHeatmap(rg,typin ="heatmap.default", title = "Heatmap of genetic correlation (rg)",p.vals = p)

#plotCorrelationHeatmap(as.matrix(fg.cov),set.order  = plot.order$order, title="Finngen gcov_int")
toy.cormat <- diag(10); toy.cormat[2:5,2:5]<- rnorm(16); toy.cormat[9:10,9:10]<- rnorm(4);
diag(toy.cormat) <- 1
plotCorrelationHeatmap <- function(cormat, typin = "hclust", colors = NA, title = "",
                                   p.vals = NA, col_limit = c(-1.0001,1.0001), 
                                   show.nums = FALSE, as.FDR = TRUE, drop.labels = FALSE, 
                                   ret.order = FALSE, set.order =FALSE, font_size=15, print.nums = NA)
{
  suppressMessages(library(ggplot2))
  if(!all(is.na(print.nums)) & !show.nums)
  {
    message("Warning- you are passing in values to print, and not printing them out.")
  }
  order_dat <- list()
  if(typin != "None" & length(set.order) == 1)
  {
    order_dat <- reorder_cormat(cormat, type = typin)
    cormat <- order_dat$cormat
  } else if(typin == "maxFirst"|typin=="max_first")
  {
    message("This option doesn't work yet, not sure why.")
    order <- getMaxFirstOrder(cormat)
    return(plotCorrelationHeatmap(cormat[order,],typin = "None"))
    message("returned....")
  }else if(length(set.order) > 1)
  {
    order_dat$order_x <- set.order$x
    order_dat$order_y <- set.order$y
    cormat <- cormat[set.order$x,set.order$y]
  }
  else
  {
    order_dat$order_x <- 1:nrow(cormat)
    order_dat$order_y <- 1:ncol(cormat)
  }

  if(all(col_limit != c(-1.0001,1.0001)))
  {
    message("Beware- adjusting the max and min colors!")
  }
  melted_cormat <- reshape2::melt(cormat)
  if(!all(is.na(p.vals))) #specify the p-values on it...
  {
    melted_alphas <- reshape2::melt(p.vals[order_dat$order_x, order_dat$order_y])
    #because alpha can be a bit difficult, just going to binarize
    fdr <- p.adjust(melted_alphas$value, method = "fdr")
    alpha_bin <- ifelse(fdr > 0.05,0 ,1)
    if(show.nums)
    {
      print("doing this...")
      p <- ggplot(data = melted_cormat %>% mutate("opacity" = alpha_bin, "fdr" = fdr, "pval"=melted_alphas$value), aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile() +scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                          midpoint = 0, limit = col_limit) + theme_classic(font_size) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("") + ylab("") + labs(fill = "R")
    }else
    {
      p <- ggplot(data = melted_cormat %>% mutate("opacity" = alpha_bin, "fdr" = fdr), aes(x=Var1, y=Var2, fill=value, alpha = fdr,)) + 
        geom_tile() +scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                          midpoint = 0, limit = col_limit) + theme_classic(font_size) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("") + ylab("") + labs(fill= "R")
    }
  
    
    
   
    ggplot(data = melted_cormat %>% mutate("opacity" = alpha_bin, "logp" = -log10(melted_alphas$value)), 
           aes(x=Var1, y=Var2, fill=value, color = as.factor(opacity))) + 
      geom_raster() +scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                        midpoint = 0, limit = col_limit) + theme_classic(font_size) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("") + ylab("") + labs("fill" = "R")
    
    
    
    
  }else{
    p <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile() +scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                        midpoint = 0, limit = col_limit) + theme_classic(font_size) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("") + ylab("")
  }


  
  if(!any(is.na(colors)))
  {
    colu <- colors #$color
    #print(colors[order_dat$order_y,])
    #print(colors[order_dat$order_x,])
    p <- p + theme(axis.text.y = element_text(colour=colu[order_dat$order_y]), 
                                              axis.text.x = element_text(colour=colu[order_dat$order_x])) 
  }
  if(all(is.na(p.vals)) & show.nums)
  {
    if(!all(is.na(print.nums)))
    {
      
      melted_values <- reshape2::melt(print.nums[order_dat$order_x, order_dat$order_y])
        print("printing the numbers this...")
        p <- ggplot(data = melted_cormat %>% mutate("scores"=melted_values$value), aes(x=Var1, y=Var2, fill=value)) + 
          geom_tile() +scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                            midpoint = 0, limit = col_limit) + theme_classic(font_size) + 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("") + ylab("") + labs(fill = "R") + 
          geom_text(aes(label=round(scores, digits = 2)))
      }else
    {
      p <- p + geom_text(aes(label=round(value, digits = 2)))
    }
    
  }
  if(!all(is.na(p.vals)) & show.nums)
  {
    if(as.FDR){
      p <- p + labs(alpha = "FDR") + geom_text(aes(label=round(fdr, digits = 2)))
    }else
    {
      p <- p + labs(alpha = "FDR") + geom_text(aes(label=round(pval, digits = 2)))
    }
    
      # scale_alpha(range = c(0, (max(-log10(p.vals)) +1))) 
      #scale_alpha_continuous(range = c(0, (max(-log10(p.vals)) +1)))
  }
  p <- p+ ggtitle(title)
  if(drop.labels)
  {
    #p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
    #              axis.text.x=element_blank(),axis.text.y=element_blank(),
    #              axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
    p <- p + theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
  }
  if(ret.order)
  {
    p <- list("plot"=p, "order" = list("x"=order_dat$order_x, "y" = order_dat$order_y ))
  }
  return(p)
}

cleanNames <- function(cnall){
  gsub(cnall, pattern = "_\\(.*\\)", replacement = "") %>% 
    gsub(.,pattern = "_automated_reading", replacement = "") %>% 
    gsub(.,pattern = "_", replacement = " ") %>% 
    gsub(.,pattern = ",", replacement = "")
}

#### Visualization tool to order so the lead factor matches between 2 setups:

#Select the next highest correlation spot that hasn't been use
#i column with the issue
#j is the row with the issue.
getNextHighestOption <- function(cor.mat, i, curr.top.index)
{
  cor.mat[,i][cor.mat[,i] >= cor.mat[curr.top.index,i]] <- -1
  which.max(cor.mat[,i])
}


##For entries that get the same cell assigned, pick the biggest.
#cor.mat- matrix of correlations
#max.list- the maximum (row) index corresponding to each column.
correctConflictedEntries <- function(cor.mat, max.list)
{
  max.list <-unlist(max.list)
  iter.count <- 1
  if(any(duplicated(max.list)))
  {
    while(any(duplicated(max.list)))
    {
      first.dup = max.list[duplicated(max.list)][1] #which row is duplicated
      #print("First issue:")
      #print(first.dup)
      copy= which(max.list == first.dup) #Which columns is it duplicated with
      copy.j = copy[2] #second incidence of the copy (column)
      copy.i = copy[1] #first column with the copy
      violating.row = first.dup
      if(cor.mat[violating.row, copy.j] > cor.mat[violating.row, copy.i])
      {
        max.list[copy.j] <- violating.row
        max.list[copy.i] <- getNextHighestOption(cor.mat, copy.i, max.list[copy.i])
        
      } else #if(sqrd.mat[max.curr, i] <= sqrd.mat[max.curr, competing.entry])
      {
        max.list[copy.j] <- getNextHighestOption(cor.mat, copy.j, max.list[copy.j])
      }
      if(iter.count == nrow(cor.mat))
      {
        return(max.list)
      }
      iter.count <- iter.count + 1
      
    }
  }
  max.list
}

getMaxFirstOrder <- function(cormat)
{
  sqrd.mat <- cormat^2
  order.max <- apply(sqrd.mat, 2, function(x) which.max(x))
  sqrd.mat <- t(sqrd.mat)
  max.entries <- list()
  for(i in 1:ncol(sqrd.mat))
  {
    max.curr <- which.max(sqrd.mat[,i])
    if(max.curr %in% unlist(max.entries))
    {
      #compare them both
      competing.entry = which(max.entries == max.curr)
      if(sqrd.mat[max.curr, i] > sqrd.mat[max.curr, competing.entry])
      {
        max.entries[[i]] <- max.curr
        max.entries[[competing.entry]] <- getNextHighestOption(sqrd.mat, competing.entry, max.entries[[competing.entry]])
        
      } else #if(sqrd.mat[max.curr, i] <= sqrd.mat[max.curr, competing.entry])
      {
        max.entries[[i]] <- getNextHighestOption(sqrd.mat, i, max.curr)
      }
      max.entries <- correctConflictedEntries(sqrd.mat, max.entries)
    } else
    {
      max.entries[[i]] <- max.curr
    }
  }
  entries <- 1:length(max.entries)
  inverted.entries <- list()
  for(i in 1:length(max.entries))
  {
    inverted.entries[[max.entries[i]]] <- i
  }
  inverted.entries <- unlist(inverted.entries)
  null.indices <- which(sapply(inverted.entries, is.null))
  inverted.entries
  
}
###PLots for evaluating the gleaner output directly:
plotByFactorNumber <- function(factor.vals, factor.name,  flip_coord = TRUE, div_factor=1)
{
  p <- ggplot(factor.vals %>% filter(abs(!!sym(factor.name)) > (mean(abs(!!sym(factor.name)))/div_factor)), aes(x = reorder(Traits,!!sym(factor.name)), y= !!sym(factor.name))) + geom_bar(stat= "identity") + xlab("Traits") + theme_bw()
  if(flip_coord)
  {
    return(p + coord_flip())
  }
  return(p)
}
plotSNPsByFactorNumber <- function(factor.vals, factor.name, ret_snps=FALSE)
{
  top.snps <- factor.vals %>% slice_max(abs(!!sym(factor.name)), n=10)
  print(unlist(top.snps[,1]))
  if(ret_snps)
  {
    return(unlist(top.snps[,1]))
  }
  ggplot(factor.vals %>% slice_max(abs(!!sym(factor.name)), n=10), aes(x = reorder(SNPs,!!sym(factor.name)), y= !!sym(factor.name))) + geom_bar(stat= "identity") + xlab("SNP") + coord_flip() + theme_bw()
}
