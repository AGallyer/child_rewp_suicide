#Load packages
library(tidyverse)
library(haven)
library(afex)
library(DataExplorer)
library(psych)
library(emmeans)
library(ggthemes)
library(readxl)
library(TOSTER)
library(Matching)
library(patchwork)

set.seed(2971)

# Study 1 -----------------------------------------------------------------

# Import data
train <- read_sav("TRAIN_suicide.sav")

# Explore data
glimpse(train)

plot_missing(train) 

train_clean <- train %>% 
      filter(Exclude == 0 & !duplicated(id))

plot_missing(train_clean)# very little missing

train_clean$ideation[train_clean$cdi_9 < 1 & train_clean$SI_KSADS != 1] <- 0
train_clean$ideation[train_clean$cdi_9 > 0 | train_clean$SI_KSADS == 1] <- 1
train_clean$ideation[is.na(train_clean$cdi_9) & is.na(train_clean$SI_KSADS)] <- NA

train_clean <- train_clean %>% 
      dplyr::select(-cdi_9) %>% 
      mutate("diff_rewp" = win - loss)

train_clean$ideation <- factor(train_clean$ideation, 
       levels = c(0, 1), 
       labels = c("No Ideation", "Ideation"))

plot_intro(train_clean)# 85.82% of rows complete
plot_missing(train_clean)# highest missing is 10.91% for household income 
plot_histogram(train_clean)
# Reformat to long for analyses
long_data <- train_clean %>% 
      gather(key = "outcome", value = "microvolts", win:loss)
long_data$outcome <- as.factor(long_data$outcome)
#Analyze Data
## 1) 2X2 condition X ideation
model <- aov_car(microvolts ~ outcome*ideation + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = long_data, factorize = FALSE)
summary(model)

model$anova_table # Main effect of win vs. loss, but not ideation or interaction

emmeans(model, specs = pairwise ~ outcome)

## 2) 2X2 condition X ideation + age
model <- aov_car(microvolts ~ outcome*ideation*age + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = long_data, factorize = FALSE)
summary(model)

model$anova_table # No effects, including for win vs. loss

## 3) Analysis 1, but in males
male_train <- train_clean %>% 
  filter(gender == 2) %>% 
  gather(key = "outcome", value = "microvolts", win:loss)

male_train$outcome <- as.factor(male_train$outcome)

model <- aov_car(microvolts ~ outcome*ideation + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = male_train, factorize = FALSE)
summary(model)

model$anova_table# Win vs. loss main effect. No other significant

emmeans(model, specs = pairwise ~ outcome)

## 4) Analysis 2, but in males
model <- aov_car(microvolts ~ outcome*ideation*age + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = male_train, factorize = FALSE)
summary(model)

model$anova_table# again, nothing significant.

## 5) Analysis 1, but in females
female_train <- long_data %>% 
      filter(gender == 1)

model <- aov_car(microvolts ~ outcome*ideation + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = female_train, factorize = FALSE)
summary(model)

model$anova_table# Main effect of win vs. loss. Nothing else

emmeans(model, specs = pairwise ~ outcome)

## 6) Analysis 2, but in females
model <- aov_car(microvolts ~ outcome*ideation*age + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = female_train, factorize = FALSE)
summary(model)

model$anova_table# No effects again

## 7) Analysis 1, but with non-PCA RewP
long_data <- train_clean %>% 
      gather(key = "outcome", value = "microvolts", FCz_Win_Avg:FCz_Loss_Avg)

long_data$outcome <- as.factor(long_data$outcome)

model <- aov_car(microvolts ~ outcome*ideation + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = long_data, factorize = FALSE)
summary(model)

model$anova_table# Same, main effect of win vs. loss but nothing else. 

emmeans(model, specs = pairwise ~ outcome)

## 8) Analysis 1, but w/ matched samples
ideation_people <- filter(train_clean, ideation == "Ideation")
plot_missing(ideation_people)# No missing data for those with ideation.
# Genmatch doesn't work with missing, so will exclude potential controls with missing data
complete_obs <- train_clean %>% 
  dplyr::select(-HouseholdIncome, everything()) %>% 
  drop_na()
treat <- as.character(complete_obs$ideation)
treat[treat == "No Ideation"] <- "0"
treat[treat == "Ideation"] <-  "1"
treat <- as.numeric(treat)
match_variables <- cbind(complete_obs$age, complete_obs$gender, complete_obs$race,
                         complete_obs$match_MDD, complete_obs$match_anx, 
                         complete_obs$match_depsx, complete_obs$match_anxsx, 
                         complete_obs$match_adhd, complete_obs$match_cp, 
                         complete_obs$match_hyp, complete_obs$match_opp)

# genout <- GenMatch(Tr = treat, X = match_variables, M = 2, pop.size = 10000,
#                    verbose = TRUE, replace = FALSE, ties = FALSE)#
# 
# save(genout, file = "genout.RData")

load("genout.RData")

matchobs <- as.matrix(genout$matches[, 2])
ideation_obs <- as.matrix(base::unique(genout$matches[, 1]))
filter_obs <- rbind(matchobs, ideation_obs)


match_data <- complete_obs %>% 
      slice(filter_obs[, 1])

Lmatch_data <- match_data %>% 
      gather(key = "outcome", value = "microvolts", win:loss)
Lmatch_data$outcome <- as.factor(Lmatch_data$outcome)

model <- aov_car(microvolts ~ outcome*ideation + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = Lmatch_data, factorize = FALSE)
summary(model)

model$anova_table # Main effect of wins vs. loss, but not ideation or interaction

emmeans(model, specs = pairwise ~ outcome)

## 9) Analysis 1, but add income covariate
model <- aov_car(microvolts ~ outcome*ideation*HouseholdIncome + Error(factor(id)/(outcome)),
                 contrast = contr.sum, data = long_data, factorize = FALSE)
summary(model)

model$anova_table # Main effect of win vs. loss, but not an effect of ideation

emmeans(model, specs = pairwise ~ outcome)

## 10) Equivalence Tests
equivalence_study1 <- train_clean %>% 
      filter(!is.na(diff_rewp)) %>% 
      group_by(ideation) %>% 
      summarize("mean_rewp" = mean(diff_rewp), 
                "sd_rewp" = sd(diff_rewp), 
                "n" = n())# 237 no ideation, 27 ideation

TOSTtwo(m1 = as.numeric(equivalence_study1[1, 2]), 
        m2 = as.numeric(equivalence_study1[2, 2]), 
        sd1 = as.numeric(equivalence_study1[1, 3]), 
        sd2 = as.numeric(equivalence_study1[2, 3]), 
        n1 = as.numeric(equivalence_study1[1, 4]), 
        n2 = as.numeric(equivalence_study1[2, 4]), 
        low_eqbound_d = -0.24, high_eqbound_d = 0.24, var.equal = FALSE, 
        alpha = 0.05, plot = TRUE, verbose = TRUE)# No evidence effect is = 0

TOSTtwo(m1 = as.numeric(equivalence_study1[1, 2]), 
        m2 = as.numeric(equivalence_study1[2, 2]), 
        sd1 = as.numeric(equivalence_study1[1, 3]), 
        sd2 = as.numeric(equivalence_study1[2, 3]), 
        n1 = as.numeric(equivalence_study1[1, 4]), 
        n2 = as.numeric(equivalence_study1[2, 4]), 
        low_eqbound_d = -0.60, high_eqbound_d = 0.60, var.equal = FALSE, 
        alpha = 0.05, plot = TRUE, verbose = TRUE)# Evidence effect is = zero


## Descriptives for table 1
study1_table <- train_clean %>%
      filter(!is.na(win)) %>% 
      group_by(ideation) %>% 
      summarise("win_mean" = mean(win), 
                "win_sd" = sd(win), 
                "loss_mean" = mean(loss),
                "loss_sd" = sd(loss), 
                "rewp_mean" = mean(diff_rewp), 
                "rewp_sd" = sd(diff_rewp), 
                'n' = n())

study1_table <- study1_table %>% 
      dplyr::select(-ideation) %>% 
      round(digits = 2)
## Plot data
p1 <- ggplot(train_clean, aes(x = ideation, y = diff_rewp, color = as.factor(ideation), fill = as.factor(ideation))) +
      geom_point(position = position_jitter(h = 0, w = 0.2), size = 1.5) + geom_violin(alpha = 0.2) +
      theme_classic() + 
      scale_color_few(labels = c("No SI", "Recent SI")) + 
      theme(text = element_text(size = 24), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
      scale_fill_few(labels = c("No SI", "Recent SI"), guide = FALSE) + 
      stat_summary(fun = mean, color = "red", size = 3, geom = "point", aes(group = ideation), show.legend = FALSE) + 
      scale_x_discrete(labels = c("No SI", "Recent SI")) + 
      scale_y_continuous(limits = c(-25, 25)) + 
      labs(color = "Ideation History", y = expression(paste(Delta, "RewP")), x = "")

jpeg(file = "gallyerfig2.jpg", width = 10, height = 12, units = "in", 
     res = 800)
p1
dev.off()

# Study 2 -----------------------------------------------------------------

# Import data
ipanda_clean <- read_sav("ipanda_suicide.sav")

ipanda_clean <- mutate(ipanda_clean, "w1_diff_rewp" = w1_win - w1_loss, 
                       "w2_diff_rewp" = w2_win - w2_loss, 
                       "w1_area_rewp" = w1_win_average - w1_loss_average, 
                       "w2_area_rewp" = w2_win_average - w2_loss_average)
#Explore data
glimpse(ipanda_clean)
describe(ipanda_clean)
plot_missing(ipanda_clean)
wave12 <- filter(ipanda_clean, wave_data == 3)
table(ipanda_clean$w1_ideation, ipanda_clean$w1_lifetime_si)
table(ipanda_clean$w2_ideation, ipanda_clean$w2_lifetime_si)
# Wave 1 Analyses ---------------------------------------------------------
## Prepare Data for Analyses
long_data <- ipanda_clean %>% 
      gather(key = "condition", value = "microvolts", w1_loss:w1_win)

long_data$w1_ideation <- factor(long_data$w1_ideation, 
                             levels = c(0, 1), 
                             labels = c("No Ideation", "Ideation"))
long_data$condition <- as.factor(long_data$condition)

## 1) 2 X 2 Condition X ideation
model <- aov_car(microvolts ~ condition*w1_ideation + Error(factor(ID)/(condition)), 
                 contrast = contr.sum, data = long_data, factorize = FALSE)

summary(model)

model$anova_table# Main effect of condition, not for ideation or interaction

emmeans(model, specs = pairwise ~ condition)

## 2) 2 X 2 condition X ideation + age
model <- aov_car(microvolts ~ condition*w1_ideation*w1_age + Error(factor(ID)/(condition)), 
                 contrast = contr.sum, data = long_data, factorize = FALSE)

summary(model)

model$anova_table # Main effect of age, all else is not significant. 

emmeans(model, specs = pairwise ~ condition)

## 3) 1, but with non-PCA RewP
long_data <- ipanda_clean %>% 
      gather(key = "condition", value = "microvolts", w1_win_average:w1_loss_average)

long_data$w1_ideation <- factor(long_data$w1_ideation, 
                             levels = c(0, 1), 
                             labels = c("No Ideation", "Ideation"))

model <- aov_car(microvolts ~ condition*w1_ideation + Error(factor(ID)/(condition)), 
                 contrast = contr.sum, data = long_data, factorize = FALSE)

summary(model)

model$anova_table# Main effect of win vs. loss, no main effect of SI or interaction

emmeans(model, specs = pairwise ~ condition)

## 4) 1, but adding income as covariate
long_data <- ipanda_clean %>% 
  gather(key = "condition", value = "microvolts", w1_loss:w1_win)

long_data$w1_ideation <- factor(long_data$w1_ideation, 
                                levels = c(0, 1), 
                                labels = c("No Ideation", "Ideation"))
long_data$condition <- as.factor(long_data$condition)

model <- aov_car(microvolts ~ condition*w1_ideation*w1_income + Error(factor(ID)/(condition)), 
                 contrast = contr.sum, data = long_data, factorize = FALSE)

summary(model)

model$anova_table# Main effect of condition, not for ideation or interaction. Interaction between income and condition

emmeans(model, specs = pairwise ~ condition)

## 5) 1, with matched samples
ideation_people <- ipanda_clean %>% 
  filter(w1_ideation == 1) %>% 
  dplyr::select(ID, w1_age, race, w1_ideation, w1_dep, w1_anx, w1cdi_tot, w1_anxsx, 
                w1_adhd, w1_cp, w1_hyp, w1_opp)

plot_missing(ideation_people)# Two missing data for anxiety and depression.


control_people <- ipanda_clean %>% 
  filter(w1_ideation == 0) %>% 
  dplyr::select(ID, w1_age, race, w1_ideation, w1_dep, w1_anx, w1cdi_tot, w1_anxsx, 
                w1_adhd, w1_cp, w1_hyp, w1_opp)

plot_missing(control_people)# Some missing data, with race (3.02%) being highest


complete_obs <- ipanda_clean %>% 
  dplyr::select(ID, w1_age, race, w1_ideation, w1_dep, w1_anx, w1cdi_tot, w1_anxsx, 
                w1_adhd, w1_cp, w1_hyp, w1_opp, w1_loss, w1_win) %>% 
  drop_na()

table(complete_obs$w1_ideation)# 46 with ideation 248 controls available for controls

plot_missing(complete_obs)# no missing data, as needed for matching procedure


treat <- complete_obs$w1_ideation

match_variables <- cbind(complete_obs$w1_age, complete_obs$race,
                         complete_obs$w1_dep, complete_obs$w1_anx, 
                         complete_obs$w1cdi_tot, complete_obs$w1_anxsx, 
                         complete_obs$w1_adhd, complete_obs$w1_cp, 
                         complete_obs$w1_hyp, complete_obs$w1_opp)

# genout2 <- GenMatch(Tr = treat, X = match_variables, M = 2, pop.size = 10000,
#                    verbose = TRUE, replace = FALSE, ties = FALSE)#
# 
# saveRDS(genout2, "genout2.RData")

genout2 <- readRDS("genout2.RData")

matchobs <- as.matrix(base::unique(genout2$matches[, 2]))
ideation_obs <- as.matrix(base::unique(genout2$matches[, 1]))
filter_obs <- rbind(matchobs, ideation_obs)

match_data <- complete_obs %>% 
  slice(filter_obs[, 1])

Lmatch_data <- match_data %>% 
  gather(key = "outcome", value = "microvolts", w1_loss:w1_win)

Lmatch_data$outcome <- as.factor(Lmatch_data$outcome)

model <- aov_car(microvolts ~ outcome*w1_ideation + Error(factor(ID)/(outcome)),
                 contrast = contr.sum, data = Lmatch_data, factorize = FALSE)

summary(model)# Only main effect of win vs. loss

model$anova_table
## 6) T1 Equivalence test
equivalence_study2w1 <- ipanda_clean %>% 
      filter(!is.na(w1_diff_rewp)) %>% 
      group_by(w1_ideation) %>% 
      summarize("mean_rewp" = mean(w1_diff_rewp), 
                "sd_rewp" = sd(w1_diff_rewp), 
                "n" = n())# 265 no ideation, 49 ideation

TOSTtwo(m1 = as.numeric(equivalence_study2w1[1, 2]), 
        m2 = as.numeric(equivalence_study2w1[2, 2]), 
        sd1 = as.numeric(equivalence_study2w1[1, 3]), 
        sd2 = as.numeric(equivalence_study2w1[2, 3]), 
        n1 = as.numeric(equivalence_study2w1[1, 4]), 
        n2 = as.numeric(equivalence_study2w1[2, 4]), 
        low_eqbound_d = -0.27, high_eqbound_d = 0.27, var.equal = FALSE, 
        alpha = 0.05, plot = TRUE, verbose = TRUE)# No evidence effect is = 0

TOSTtwo(m1 = as.numeric(equivalence_study2w1[1, 2]), 
        m2 = as.numeric(equivalence_study2w1[2, 2]), 
        sd1 = as.numeric(equivalence_study2w1[1, 3]), 
        sd2 = as.numeric(equivalence_study2w1[2, 3]), 
        n1 = as.numeric(equivalence_study2w1[1, 4]), 
        n2 = as.numeric(equivalence_study2w1[2, 4]), 
        low_eqbound_d = -0.60, high_eqbound_d = 0.60, var.equal = FALSE, 
        alpha = 0.05, plot = TRUE, verbose = TRUE)# Evidence effect is = zero


# Wave 2 Analyses ---------------------------------------------------------
long_data2 <- ipanda_clean %>% 
      gather(key = "condition", value = "microvolts", w2_loss:w2_win)

long_data2$w2_ideation <- factor(long_data2$w2_ideation, 
                             levels = c(0, 1), 
                             labels = c("No Ideation", "Ideation"))
long_data2$condition <- as.factor(long_data2$condition)

## 1) 2 X 2 Condition X ideation
model <- aov_car(microvolts ~ condition*w2_ideation + Error(factor(ID)/(condition)), 
                 contrast = contr.sum, data = long_data2, factorize = FALSE)

summary(model)

model$anova_table# Main effect of condition, nothing for ideation or interaction

emmeans(model, specs = pairwise ~ condition)

## 2) 2 X 2 condition X ideation + age
model <- aov_car(microvolts ~ condition*w2_ideation*w2_age + Error(factor(ID)/(condition)), 
                 contrast = contr.sum, data = long_data2, factorize = FALSE)

summary(model)

model$anova_table # No significant effects 

emmeans(model, specs = pairwise ~ outcome)

## 3) 1, but with non-PCA RewP
long_data2 <- ipanda_clean %>% 
      gather(key = "condition", value = "microvolts", w2_win_average:w2_loss_average)

long_data2$w2_ideation <- factor(long_data2$w2_ideation, 
                                levels = c(0, 1), 
                                labels = c("No Ideation", "Ideation"))

model <- aov_car(microvolts ~ condition*w2_ideation + Error(factor(ID)/(condition)), 
                 contrast = contr.sum, data = long_data2, factorize = FALSE)

summary(model)

model$anova_table# Main effect of condition, no effect of SI or interaction

emmeans(model, specs = pairwise ~ condition)

## 4) 1, but adding income as covariate
long_data2 <- ipanda_clean %>% 
  gather(key = "condition", value = "microvolts", w2_loss:w2_win)

long_data2$w2_ideation <- factor(long_data2$w2_ideation, 
                                levels = c(0, 1), 
                                labels = c("No Ideation", "Ideation"))
long_data2$condition <- as.factor(long_data2$condition)

model <- aov_car(microvolts ~ condition*w2_ideation*w2_income + Error(factor(ID)/(condition)), 
                 contrast = contr.sum, data = long_data2, factorize = FALSE)

summary(model)

model$anova_table# Main effect of condition, nothing else significant

emmeans(model, specs = pairwise ~ condition)

## 5) 1, but with matched samples
ideation_people <- ipanda_clean %>% 
  filter(w2_ideation == 1) %>% 
  dplyr::select(ID, w2_age, race, w2_ideation, w2_dep, w2_anx, w2cdi_tot, w2_anxsx, 
                w2_adhd, w2_cp, w2_hyp, w2_opp)

plot_missing(ideation_people)# A few missing Race variable


control_people <- ipanda_clean %>% 
  filter(w2_ideation == 0) %>% 
  dplyr::select(ID, w2_age, race, w2_ideation, w2_dep, w2_anx, w2cdi_tot, w2_anxsx, 
                w2_adhd, w2_cp, w2_hyp, w2_opp)

plot_missing(control_people)# Some missing data, with race (2.65%) being highest


complete_obs <- ipanda_clean %>% 
  dplyr::select(ID, w2_age, race, w2_ideation, w2_dep, w2_anx, w2cdi_tot, w2_anxsx, 
                w2_adhd, w2_cp, w2_hyp, w2_opp, w2_loss, w2_win) %>% 
  drop_na()

table(complete_obs$w2_ideation)# 31 with ideation 214 controls available for controls

plot_missing(complete_obs)# no missing data, as needed for matching procedure


treat <- complete_obs$w2_ideation

match_variables <- cbind(complete_obs$w2_age, complete_obs$race,
                         complete_obs$w2_dep, complete_obs$w2_anx, 
                         complete_obs$w2cdi_tot, complete_obs$w2_anxsx, 
                         complete_obs$w2_adhd, complete_obs$w2_cp, 
                         complete_obs$w2_hyp, complete_obs$w2_opp)

# genout3 <- GenMatch(Tr = treat, X = match_variables, M = 2, pop.size = 10000,
#                    verbose = TRUE, replace = FALSE, ties = FALSE)#
# 
# saveRDS(genout3, "genout3.RData")

genout3 <- readRDS("genout3.RData")

matchobs <- as.matrix(base::unique(genout3$matches[, 2]))
ideation_obs <- as.matrix(base::unique(genout3$matches[, 1]))
filter_obs <- rbind(matchobs, ideation_obs)

match_data <- complete_obs %>% 
  slice(filter_obs[, 1])

Lmatch_data <- match_data %>% 
  gather(key = "outcome", value = "microvolts", w2_loss:w2_win)

Lmatch_data$outcome <- as.factor(Lmatch_data$outcome)

model <- aov_car(microvolts ~ outcome*w2_ideation + Error(factor(ID)/(outcome)),
                 contrast = contr.sum, data = Lmatch_data, factorize = FALSE)

summary(model)# Only main effect of win vs. loss

model$anova_table
## 6) T2 Equivalence test
equivalence_study2w2 <- ipanda_clean %>% 
      filter(!is.na(w2_diff_rewp) & !is.na(w2_ideation)) %>% 
      group_by(w2_ideation) %>% 
      summarize("mean_rewp" = mean(w2_diff_rewp), 
                "sd_rewp" = sd(w2_diff_rewp), 
                "n" = n())# 222 no ideation, 33 ideation


TOSTtwo(m1 = as.numeric(equivalence_study2w2[1, 2]), 
        m2 = as.numeric(equivalence_study2w2[2, 2]), 
        sd1 = as.numeric(equivalence_study2w2[1, 3]), 
        sd2 = as.numeric(equivalence_study2w2[2, 3]), 
        n1 = as.numeric(equivalence_study2w2[1, 4]), 
        n2 = as.numeric(equivalence_study2w2[2, 4]), 
        low_eqbound_d = -0.27, high_eqbound_d = 0.27, var.equal = FALSE, 
        alpha = 0.05, plot = TRUE, verbose = TRUE)# No evidence effect is = 0

TOSTtwo(m1 = as.numeric(equivalence_study2w2[1, 2]), 
        m2 = as.numeric(equivalence_study2w2[2, 2]), 
        sd1 = as.numeric(equivalence_study2w2[1, 3]), 
        sd2 = as.numeric(equivalence_study2w2[2, 3]), 
        n1 = as.numeric(equivalence_study2w2[1, 4]), 
        n2 = as.numeric(equivalence_study2w2[2, 4]), 
        low_eqbound_d = -0.60, high_eqbound_d = 0.60, var.equal = FALSE, 
        alpha = 0.05, plot = TRUE, verbose = TRUE)# Evidence effect is = zero


# W1 RewP -> W2 SI --------------------------------------------------------
logit <- glm(w2_ideation ~ w1_diff_rewp, data = ipanda_clean, 
             family = "binomial")

summary(logit)# W1 diff RewP does not predict W2 ideation

exp(cbind(OR = coef(logit), confint(logit)))

logit2 <- glm(w2_ideation ~ w1_diff_rewp + w1_age, data = ipanda_clean, 
              family = "binomial")

summary(logit2)# age but not rewp predicts w2 ideation

exp(cbind(OR = coef(logit2), confint(logit2)))


study2_table1 <- ipanda_clean %>%
      filter(!is.na(w1_win)) %>% 
      group_by(w1_ideation) %>% 
      summarise("w1_win_mean" = mean(w1_win, na.rm = TRUE), 
                "w1_win_sd" = sd(w1_win, na.rm = TRUE), 
                "w1_loss_mean" = mean(w1_loss, na.rm = TRUE),
                "w1_loss_sd" = sd(w1_loss, na.rm = TRUE), 
                "w1_rewp_mean" = mean(w1_diff_rewp, na.rm = TRUE), 
                "w1_rewp_sd" = sd(w1_diff_rewp, na.rm = TRUE), 
                'n' = n())

study2_table1 <- round(study2_table1, digits = 2)

study2_table2 <- ipanda_clean %>%
      filter(!is.na(w2_win)) %>% 
      group_by(w2_ideation) %>% 
      summarise("w2_win_mean" = mean(w2_win, na.rm = TRUE), 
"w2_win_sd" = sd(w2_win, na.rm = TRUE), 
"w2_loss_mean" = mean(w2_loss, na.rm = TRUE),
"w2_loss_sd" = sd(w2_loss, na.rm = TRUE), 
"w2_rewp_mean" = mean(w2_diff_rewp, na.rm = TRUE), 
"w2_rewp_sd" = sd(w2_diff_rewp, na.rm = TRUE), 
'n' = n())

round(study2_table2, digits = 2)
#Plot data
## Wave 1 plot
graph_data <- ipanda_clean %>% 
      dplyr::filter(!is.na(w1_ideation))

p2 <- ggplot(graph_data, aes(x = w1_ideation, y = w1_diff_rewp, color = as.factor(w1_ideation), fill = as.factor(w1_ideation))) +
      geom_point(position = position_jitter(h = 0, w = 0.2), size = 1.5) + geom_violin(alpha = 0.2) +
      theme_classic() +
      scale_color_few(labels = c("No SI", "Recent SI")) +
      theme(text = element_text(size = 24), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
      scale_fill_few(labels = c("No SI", "Recent SI"), guide = FALSE) +
      stat_summary(fun = mean, color = "red", size = 3, geom = "point", aes(group = w1_ideation), show.legend = FALSE) +
      scale_x_discrete(labels = c("No SI", "Recent SI")) +
      scale_y_continuous(limits = c(-15, 25)) +
      labs(color = "Ideation History", y = expression(paste(Delta, "RewP")), x = "")

#Wave 2 plot
graph_data <- ipanda_clean %>% 
      filter(!is.na(w2_ideation))

p3 <- ggplot(graph_data, aes(x = w2_ideation, y = w2_diff_rewp, color = as.factor(w2_ideation), fill = as.factor(w2_ideation))) +
      geom_point(position = position_jitter(h = 0, w = 0.2), size = 1.5) + geom_violin(alpha = 0.2) +
      theme_classic() +
      scale_color_few(labels = c("No SI", "Recent SI")) +
      theme(text = element_text(size = 24), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      scale_fill_few(labels = c("No SI", "Recent SI"), guide = FALSE) +
      stat_summary(fun = mean, color = "red", size = 3, geom = "point", aes(group = w2_ideation), show.legend = FALSE) +
      scale_x_discrete(labels = c("No SI", "Recent SI")) +
      scale_y_continuous(limits = c(-15, 25)) +
      labs(color = "Ideation History", y = expression(paste(Delta, "RewP")), x = "")


jpeg(file = "gallyerfig3.jpg", width = 10, height = 12, units = "in", 
     res = 800)
p2 + p3 + 
      plot_annotation(tag_levels = "A")
dev.off()

# Demographics tables
study1_demo <- train_clean %>% 
  filter(!is.na(diff_rewp)) %>% 
  group_by(ideation) %>% 
  summarize("mean_age" = mean(age), 
            "sd_age" = sd(age),
            "n" = n(), 
            "females" = sum(gender == 1),
            "female_percent" = females/n, 
            "white" = sum(race == 1), 
            "white_percent" = white/n, 
            "median_income" = median(HouseholdIncome, na.rm = TRUE))

study21_demo <- ipanda_clean %>% 
  filter(!is.na(w1_diff_rewp)) %>% 
  group_by(w1_ideation) %>% 
  summarize("mean_age" = mean(w1_age), 
            "sd_age" = sd(w1_age),
            "n" = n(), 
            "white" = sum(race == 5, na.rm = TRUE), 
            "white_percent" = white/n, 
            "median_income" = median(w1_income, na.rm = TRUE), 
            'mean_income' = mean(w1_income, na.rm = TRUE))

study22_demo <- ipanda_clean %>% 
  filter(!is.na(w2_diff_rewp)) %>% 
  group_by(w2_ideation) %>% 
  summarize("mean_age" = mean(w2_age), 
            "sd_age" = sd(w2_age),
            "n" = n(), 
            "white" = sum(race == 5, na.rm = TRUE), 
            "white_percent" = white/n, 
            "median_income" = median(w2_income, na.rm = TRUE), 
            'mean_income' = mean(w2_income, na.rm = TRUE))


# Response to Reviewers Analyses ------------------------------------------
t.test(w1_income ~ w1_ideation, data = ipanda_clean, na.rm = TRUE, 
       var.equal = TRUE)

t.test(w2_income ~ w2_ideation, data = ipanda_clean, na.rm = TRUE, 
       var.equal = TRUE)

wave1_only <- ipanda_clean %>% 
  filter(wave_data == 1)

t.test(wave1_only$w1_income, wave12$w1_income, na.rm = TRUE, var.equal = TRUE)

sum((ipanda_clean$w1_ideation == 1) & (ipanda_clean$w2_ideation == 1), 
    na.rm = TRUE)# 18 reported SI at both time points

sum((ipanda_clean$w1_ideation == 0) & (ipanda_clean$w2_ideation == 1), 
    na.rm = TRUE)

ipanda_clean$w1_ideation <- factor(ipanda_clean$w1_ideation, 
                               levels = c(0, 1), 
                               labels = c("No Ideation", "Ideation"))

train_join <- train_clean %>% 
  dplyr::select(gender, ideation, win, loss) %>% 
  mutate('study' = 0)

ipanda_join <- ipanda_clean %>% 
  dplyr::select('gender' = sex, 'ideation' = w1_ideation, 'win' = w1_win, 
                'loss' = w1_loss) %>% 
  mutate('study' = 1)

all_data <- rbind(train_join, ipanda_join)

all_data$id <- c(1:nrow(all_data))

all_data <- all_data %>% 
  mutate('gender' = gender - 1) %>% 
  filter(gender != 2)

all_data$gender <- factor(all_data$gender, 
                                   levels = c(0, 1), 
                                   labels = c("female", "male"))

long_all_data <- all_data %>% 
  pivot_longer(win:loss, names_to = 'condition', values_to = 'microvolts')


library(lme4)
library(lmerTest)
library(emmeans)
library(equatiomatic)

random_model <- lmerTest::lmer(microvolts ~ ideation + gender + condition + gender*ideation + 
                       ideation*condition + gender*condition + 
                         gender*ideation*condition + (1 | study), 
                       data = long_all_data)

summary(random_model)

jpeg(file = "supplementfig1.jpg", width = 6, height = 6, units = "in", 
     res = 800)

emmip(random_model, gender ~ ideation | condition, CIs = FALSE) + theme_classic() + theme(text = element_text(size = 18)) 

dev.off()

emmip(random_model, gender ~ condition)

emmeans(random_model, pairwise ~ condition | gender)

