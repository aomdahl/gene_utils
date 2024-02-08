makeScatterGrid <- function(df, npcs, group_name)
{
  library(cowplot)
  library(ggplot2)
  library(rlang)
  pc.plot <- list()
  for(i in 1:npcs)
  {
    for(j in 1:npcs)
    {
      pc.plot[[paste0(i,"_",j)]] <- ggplot(df, aes(x=!!sym(paste0("PC", j)), y=!!sym(paste0("PC", i)), col = !!sym(group_name))) + 
        geom_point() + theme_bw() + 
        theme(legend.position="none", axis.title.x=element_blank(),axis.title.y=element_blank())
      if(i==1 & j == 1)
      {
        first <- ggplot(df, aes(x=!!sym(paste0("PC", j)), y=!!sym(paste0("PC", i)), col = !!sym(group_name))) + 
          geom_point()  + theme(legend.position="bottom") + guides(colour = guide_legend(nrow = 1))
      }
      #Make diagonal entries blank with the PC name on it
      if(i==j)
      {
        pc.plot[[paste0(i,"_",j)]] <- ggplot() +  annotate("text", x = 1,  y = 1,
                                                           size = 20, label = paste0("PC", i)) + theme_void()
      }
    }
  }
  leg <- get_legend(first)
  main.plot <- cowplot::plot_grid(plotlist=pc.plot, nrow = 5, ncol = 5)
  plot_grid(main.plot, leg, nrow = 2,rel_heights = c(1,0.1))
  
}

