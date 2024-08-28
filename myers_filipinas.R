#Índice Combinado de Myers de Preferencia Digital
library(tidyverse)

terminal_digit <- seq(0,9,1)
pop_10a <- c(5794442, 4735734, 4706488, 4371722, 4382456,
             4534455, 3926253, 4028469, 3767867, 3780227)
pop_20a <- c(4144526, 3243767, 3200533, 2962601, 2973683,
             3158357, 2623463, 2672365, 2438758, 2503677)
weight1 <- seq(1,10,1)
weight2 <- seq(9, 0, -1)


data <- cbind(terminal_digit, pop_10a, pop_20a, weight1, weight2)
view(data)
data <- as.data.frame(data)

myer_tab <-data %>% 
  mutate(across(c(terminal_digit, pop_10a,
                  pop_20a, weight1, weight2), as.numeric)) %>% 
  mutate(number = ((pop_10a*weight1)+ (pop_20a*weight2))) %>% 
  mutate(pct_dis = (round(number/sum(number), digits=4)*100)) %>% 
  mutate(dev_pct_10 = abs(pct_dis -10))

myer_index <- (sum(myer_tab$dev_pct_10)/2)
print(myer_index)

library(writexl)

writexl::write_xlsx(myer_tab, path="myer_tab_philipines.xlsx")





