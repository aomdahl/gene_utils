#Finally making code SNPippets for creating some plots, references, etc. That are useful.

#histograms on a grid
plot.grid <- list()
x <- plieo.count.by.factor[[1]]$gwas_sig

snps <- plieo.count.by.factor[[1]]$ids
test <- t.test(x = x, y = (sig_counts %>% filter(!(ids %in% plieo.count.by.factor[[1]]$ids)))$gwas_sig,alternative = "greater")

plot.grid[[1]] <- ggplot(data = sig_counts %>% mutate("factor" = ifelse(ids %in% snps, "F1", "other")), aes(x = gwas_sig, fill = factor)) + geom_density(position = "identity", alpha= 0.5) + theme_minimal(15)  + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) + 
    ggtitle("vs all SNPs")  +  scale_fill_discrete(labels = c("F1", "other Factor")) + annotate(geom = "text", x = 6, y = 2, label = paste("p = ", round(test$p.value, digits = 3))) +  theme(legend.position = "none") 


for(i in 2:15)
{
  y <- plieo.count.by.factor[[i]]$gwas_sig
  test <- t.test(x = x, y = y, alternative = "greater")
  
  overlap <- sum( plieo.count.by.factor[[i]]$ids %in% snps) / five.perc
  
  plot.grid[[i]] <- ggplot(data = joint.tab.plieo %>% filter(factor %in% c("F1", paste0("F",i))), aes(x = gwas_sig, fill = factor)) + geom_histogram(position = "identity", alpha= 0.5) + theme_minimal(15) + scale_fill_discrete(labels = c("F1", "other Factor")) + 
    ggtitle( paste0("F",i)) + 
    theme(axis.title.x=element_blank(),axis.title.y=element_blank()) + 
    annotate(geom = "text", x = 12, y = 200, label = paste("p = ", round(test$p.value, digits = 3))) #+
    #annotate(geom = "text", x = 12, y = 150, label = round(overlap, digits = 2), color = "red")
  legend <- get_legend(plot.grid[[i]])

       plot.grid[[i]] <- plot.grid[[i]] +  theme(legend.position = "none") 
}

