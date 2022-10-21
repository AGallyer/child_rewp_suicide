library(tidyverse)
library(here)
library(patchwork)
library(gghighlight)

list_of_files <- list.files(here("pca_fig_files", "TRAIN"), pattern = ".txt")

read_data <- function(file){
      condition <- substring(file, first = 30, last = 32)
      pca_factor <- substring(file, first = 35, last = 41)
      data <- read_delim(here('pca_fig_files', 'TRAIN', file), 
                         col_names = c("Fp1", "Fp2", "F7", "F3", "Fz", "F4", 
                                       "F8", "FCz", "FC5", "FC1", "FC2", "FC6",
                                       "FT10", "T7", "C3", "Cz", "C4", "T8", 
                                       "TP9", "CP5", "CP1", "CP2", "CP6", "TP10", 
                                       "P7", "P3", "Pz", "P4", "P8",
                                       "O1", "Oz", "O2"))
      if (condition == 'LOS'){
            condition <- 'Loss'
      }
      
      if (condition == 'WIN'){
            condition <- 'Win'
      }
      
      data$condition <- rep(condition, times = length(data$Cz))
      data$pca_factor <- rep(pca_factor, times = length(data$Cz))
      data2 <- select(data, condition, pca_factor, 'voltage' = Cz)
      return(data2)
}
df1 <- list_of_files %>% 
      map_df(read_data)

ms <- ms <- rep(c(-200:799), 120)

df1$ms <- ms

#plot supplemental figure

study1 <- ggplot(df1, aes(x = ms, y = voltage)) + geom_line(aes(linetype = condition, color = pca_factor), size = 1.5) +
      gghighlight(pca_factor == 'TF03SF1', keep_scales = TRUE,
                  unhighlighted_params = list(colour = NULL, alpha = 0.15), use_direct_label = TRUE
                  ) + 
      geom_rect(xmin = 250, xmax=350, ymin = -Inf, ymax=Inf,
                alpha=.02, color = NA, fill = "grey") +
      theme_classic()+ theme(text = element_text(size = 24))

#Study 2 Wave 1
list_of_files <- list.files(here("pca_fig_files", "ipanda1"), pattern = ".txt")

read_data <- function(file){
   condition <- substring(file, first = 45, last = 47)
   pca_factor <- substring(file, first = 50, last = 56)
   data <- read_delim(here('pca_fig_files', 'ipanda1', file), 
                      col_names = FALSE)
   if (condition == 'LOS'){
      condition <- 'Loss'
   }
   
   if (condition == 'WIN'){
      condition <- 'Win'
   }
   
   data$condition <- rep(condition, times = length(data$X32))
   data$pca_factor <- rep(pca_factor, times = length(data$X32))
   data2 <- select(data, condition, pca_factor, 'voltage' = X32)
   return(data2)
}

df2<- list_of_files %>% 
   map_df(read_data)

ms <- rep(seq(-200, 799, length.out = 1024), 160)

df2$ms <- ms

study2w1 <- ggplot(df2, aes(x = ms, y = voltage)) + geom_line(aes(linetype = condition, color = pca_factor), size = 1.5) +
   gghighlight(pca_factor == 'TF07SF1', keep_scales = TRUE,
               unhighlighted_params = list(colour = NULL, alpha = 0.15), use_direct_label = TRUE) + 
   geom_rect(xmin = 250, xmax=350, ymin = -Inf, ymax=Inf,
             alpha=.01, color = NA, fill = "grey") +
   theme_classic()+ theme(text = element_text(size = 24))

#Study 2 Wave 2
list_of_files <- list.files(here("pca_fig_files", "ipanda2"), pattern = ".txt")

read_data <- function(file){
   condition <- substring(file, first = 45, last = 47)
   pca_factor <- substring(file, first = 50, last = 56)
   data <- read_delim(here('pca_fig_files', 'ipanda2', file), 
                      col_names = FALSE)
   if (condition == 'LOS'){
      condition <- 'Loss'
   }
   
   if (condition == 'WIN'){
      condition <- 'Win'
   }
   
   data$condition <- rep(condition, times = length(data$X32))
   data$pca_factor <- rep(pca_factor, times = length(data$X32))
   data2 <- select(data, condition, pca_factor, 'voltage' = X32)
   return(data2)
}

df3<- list_of_files %>% 
   map_df(read_data)

ms <- rep(seq(-200, 799, length.out = 1024), 114)

df3$ms <- ms

study2w2 <- ggplot(df3, aes(x = ms, y = voltage)) + geom_line(aes(linetype = condition, color = pca_factor), size = 1.5) +
   gghighlight(pca_factor == 'TF06SF1', keep_scales = TRUE,
               unhighlighted_params = list(colour = NULL, alpha = 0.15), use_direct_label = TRUE) + 
   geom_rect(xmin = 250, xmax=350, ymin = -Inf, ymax=Inf,
             alpha=.01, color = NA, fill = "grey") +
   theme_classic()+ theme(text = element_text(size = 24))

#Create patchwork figure
suppfig <- study1/study2w1/study2w2

jpeg(file = "gallyersupfig2.jpg", width = 20, height = 10, units = "in", 
     res = 800)
suppfig + plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'A') 
dev.off()