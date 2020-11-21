library(SASxport)
library(dplyr)
data_dir <- "./data"
setwd(data_dir)

# Examination
# Important: BMI
# Jessie

# Diet
# Fat, suagr, carbonhydrate, sodium
# Yuxin

# Laboratory
# Runting

# Tasks: data cleaning, data visualization

# drop RIDEXPRG
# merge DMDEDUC2, DMDEDUC3
# get rid of na value in DMDMARTL
demo <- read.xport("DEMO_J.XPT")
demo_clean <- demo %>% select(DMDEDUC2, DMDEDUC3, DMDMARTL, INDFMIN2, INDFMPIR, 
                              INDHHIN2, RIAGENDR, RIDAGEYR, RIDEXPRG, RIDRETH1, 
                              RIDRETH3)
summary(demo_clean)

# Missing DR1_300, DR1_320Z, DR1_330Z
diet <- read.xport("DR1TOT_J.XPT")
diet_clean <- diet %>% select(DBD100, DR1BWATZ, 
                              DR1TALCO, DR1TB12A)
summary(diet_clean)
summary(diet)
round(cor(diet), 2)
diet_nona <- diet %>% filter()

diabetes <- read.xport("DIQ_J.XPT")
sugar <- diet %>% select(SEQN, DR1TSUGR)
result <- diabetes %>% select(SEQN, DIQ010)
test <- inner_join(sugar, result, by = "SEQN") %>% filter(is.na(DR1TSUGR) != TRUE & is.na(DIQ010) != TRUE) %>%
        filter(DIQ010 == 1 | DIQ010 == 2)

cor(test$DR1TSUGR, test$DIQ010)
rcorr(as.matrix(test$DR1TSUGR, test$DIQ010))

chisq.test(table(test$DR1TSUGR, test$DIQ010))

library(ltm)
biserial.cor(test$DR1TSUGR, test$DIQ010)