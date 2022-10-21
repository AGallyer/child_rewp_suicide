library(tidyverse)
library(compute.es)
library(paramtest)
library(pwr)

set.seed(9001)

mes(0, .6, 1, 1, 237, 27)# proves that if diff between wins and losses is 0 and .6 for our two groups respectively, with SD = 1 for both, then cohen d = +-0.6

mes(0, .6, 1, 1, 265, 49)# proves that if diff between wins and losses is 0 and .6 for our two groups respectively, with SD = 1 for both, then cohen d = +-0.6

mes(0, .6, 1, 1, 222, 33)# proves that if diff between wins and losses is 0 and .6 for our two groups respectively, with SD = 1 for both, then cohen d = +-0.6

#Reminder, t-test where dv = wins - losses, and IV = ideation, is equivalent to condition * ideation interaction in anova

#Simulation function
t_func <- function(simNum, N1, N2, d){
      nosi_group <- rnorm(N1, 0, 1)
      si_group <- rnorm(N2, d, 1)
      
      t <- t.test(nosi_group, si_group, var.equal = TRUE)
      stat <- t$statistic
      p <- t$p.value
      
      return(c(t = stat, p = p, sig = (p < 0.05)))
      
}

#Study 1 Power Analysis
power_ttest <- run_test(t_func, n.iter = 10000, output = 'data.frame', 
                        N1 = 237, N2 = 27, d = 0.6)

results(power_ttest) %>% 
   summarize(power = mean(sig))# 0.8412 power

pwr.t2n.test(n1 = 237, n2 = 27, d = 0.6)#0.8372

#Study 2 Wave 1 Power Analysis
power_ttest <- run_test(t_func, n.iter = 10000, output = 'data.frame', 
                        N1 = 265, N2 = 49, d = 0.6)

results(power_ttest) %>% 
   summarize(power = mean(sig))# 0.9725

pwr.t2n.test(n1 = 265, n2 = 49, d = 0.6)#0.9703

#Study 2 Wave 2 Power Analysis
power_ttest <- run_test(t_func, n.iter = 10000, output = 'data.frame', 
                        N1 = 222, N2 = 33, d = 0.6)

results(power_ttest) %>% 
   summarize(power = mean(sig))#0.8958

pwr.t2n.test(n1 = 222, n2 = 33, d = 0.6)#0.8932
