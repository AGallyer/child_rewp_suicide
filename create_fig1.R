# Load packages
library(tidyverse)
library(ggthemes)
library(here)
library(haven)
#Study 1 Figure===================================================================
list_of_files <- list.files(here("text_files", "TRAIN"), pattern = ".txt")

read_data <- function(x){
      file_name <- x
      id <- as.numeric(str_replace_all(file_name, "[:alpha:]", ""))
      condition <- substr(file_name, 7, 9)
      data <- read_table2(x, col_names = c("Fp1", "Fp2", "F7", "F3", "Fz", "F4", 
                                           "F8", "FCz", "FC5", "FC1", "FC2", "FC6", 
                                           "FT10", "T7", "C3", "Cz", "C4", "T8", 
                                           "TP9", "CP5", "CP1", "CP2", "CP6", "TP10", 
                                           "P7", "P3", "Pz", "P4", "P8", 
                                           "O1", "Oz", "O2"))
      data$id <- rep(id, times = length(data$Cz))
      data$condition <- rep(condition, times = length(data$Cz))
      data2 <- select(data, id, condition, everything())
      return(data2)
}

setwd(here("text_files", "TRAIN"))

df1 <- list_of_files %>% 
      map_df(read_data)

setwd(here())

train <- read_sav("TRAIN_suicide.sav")

train_clean <- train %>% 
      filter(Exclude == 0)

train_clean$ideation[train_clean$cdi_9 < 1] <- 0
train_clean$ideation[train_clean$cdi_9 > 0] <- 1
train_clean$ideation[is.na(train_clean$cdi_9)] <- NA

study1_data <- train_clean %>% 
      select(id, ideation) %>% 
      full_join(df1) %>% 
      filter(!is.na(condition))

ms <- rep(c(-200:799), 534)

study1_data$ms <- ms

graph_data <- study1_data %>% 
      select(id, ms, ideation, condition, FCz) %>% #Can also do Cz
      group_by(ms, ideation, condition) %>% 
      summarize("FCz_ga" = mean(FCz)) %>% 
      pivot_wider(id_cols = ms, names_from = c(ideation, condition), 
                  values_from = FCz_ga) %>% 
      mutate('0_DIF' = `0_WIN` - `0_LOS`, 
             '1_DIF' = `1_WIN` - `1_LOS`) %>% 
   pivot_longer(cols = `0_LOS`:`1_DIF`, names_to = c('ideation', 'condition'), 
                names_sep = '_', values_to = 'FCz_ga')

graph_data$ideation[graph_data$ideation == 0] <- "No SI"
graph_data$ideation[graph_data$ideation == 1] <- "Recent SI"

graph_data$condition <- factor(graph_data$condition, levels = c("WIN", "LOS", "DIF"))
#Graph data
fig <- ggplot(graph_data, aes(x = ms, y = FCz_ga, color = condition)) + geom_rect(xmin = 250, xmax=350, ymin = -Inf, ymax=Inf, alpha=.02, color = NA, fill = "grey") + geom_line(lwd = 2) 

jpeg(file = "gallyersfig2.jpg", width = 20, height = 10, units = "in", 
     res = 800)
fig + theme_classic() + scale_x_continuous(breaks = c(-200, -100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) + 
      scale_y_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25), limits = c(-5, 25)) + 
      labs(x = "Time (ms)", y = expression(paste("Mean Amplitude (", mu, "V)")), color = "Condition") + 
      scale_color_manual(labels = c("Win", "Loss", "Win - Loss"), values = c("#008348", "#98002e", "#0000FF")) + 
      theme(text = element_text(size = 24), axis.text.x = element_text(angle=45, hjust=1), panel.spacing = unit(2, "lines")) + facet_wrap(ideation ~ .) + coord_fixed(ratio = 25) 
dev.off()
