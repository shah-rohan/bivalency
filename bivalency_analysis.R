library(tidyverse)

load("data_loaded.RData")

naive_hmd$Gene = as.character(naive_hmd$Gene)
primed_hmd$Gene = as.character(primed_hmd$Gene)
npc_hmd$Gene = as.character(npc_hmd$Gene)

tss_range = seq(-3, 3, by = 0.05)

metagene = function(dataset, range, genes){
  dataset_names = names(dataset)
  summ_df = data.frame(dist = range)
  
  for (i in dataset_names){
    summ_df[,i] = as.numeric(dataset[[i]] %>% filter(Gene %in% genes) %>% select(-1) %>% summarize(across(.fns = mean)))
  }
  
  return(summ_df)
}

metagene_plot = function(summ_df){
  summ_plt = summ_df %>% gather("mod", "hmd", -dist)
  plot_summary = ggplot(data = summ_plt, aes(x = dist, y = hmd, color = mod)) + geom_line()
  print(plot_summary)
}