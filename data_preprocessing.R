library(SASxport)
library(dplyr)
library(tidyverse)

data_dir <- "./data"
setwd(data_dir)

# Diet
# Fat, suagr, carbonhydrate, sodium
# Yuxin
# read in datasets
diet_1 <- read.xport('DR1TOT_J.XPT')
diet_2 <- read.xport('DR2TOT_J.XPT')
dat_t <- read.xport("DIQ_J.XPT")


# select useful columns and rename column names
diet_1 <- diet_1 %>% select(SEQN, DR1TCARB, DR1TSUGR, DR1TTFAT, DR1TSODI) %>% rename('carbonhydrate1' = 'DR1TCARB', 'total_sugar1' = 'DR1TSUGR', 'total_fat1' = 'DR1TTFAT', 'sodium1' = 'DR1TSODI')
diet_2 <- diet_2 %>% select(SEQN, DR2TCARB, DR2TSUGR, DR2TTFAT, DR2TSODI) %>% rename('carbonhydrate2' = 'DR2TCARB', 'total_sugar2' = 'DR2TSUGR', 'total_fat2' = 'DR2TTFAT', 'sodium2' = 'DR2TSODI')
dat_t <- dat_t %>% select(SEQN, DIQ010) %>% rename('diabetes' = 'DIQ010') %>% drop_na()

# check that within diet_1 and diet_2, each row with na will have exactly four na
# diet_1_na <- rowSums(is.na(diet_1))
# diet_2_na <- rowSums(is.na(diet_2))

# use the following 2 lines of code to test if each row with na has exactly 4 na.
# turns out that this is true for both diet_1 and diet_2
# which(0 < diet_1_na  & diet_1_na < 4 )
# which(0 < diet_2_na  & diet_2_na < 4 )

# now we can go ahead and full_join diet_1 and diet_2
dat <- full_join(diet_1, diet_2, by = 'SEQN')

# and check the na
# dat_na <- rowSums(is.na(dat))


# 1220 lines lack data from day 1
# 2202 lines lack data from day 2
# 1209 lines lack data from both days
# 2213 lines lack data from one day

# thus we can say that:
# 11 people only lack data from day 1,
# 993 people only lack data from day 2
# 1209 people lack data for both days
# tested using which(dat_na > 0,3,4,7,8)


# I decide to use the mean value of the two days if data from both date exist,
# use the value of one day if only data from that day exist,
# and delete the rows with all empty values except for id column.

dat <- dat %>% mutate(carbonhydrate = ifelse((!is.na(carbonhydrate1)) & (!is.na(carbonhydrate2)), (carbonhydrate1 + carbonhydrate2)/2.0, ifelse(is.na(carbonhydrate1), carbonhydrate2, carbonhydrate1)))
dat <- dat %>% mutate(total_sugar = ifelse((!is.na(total_sugar1)) & (!is.na(total_sugar2)), (total_sugar1 + total_sugar2)/2.0, ifelse(is.na(total_sugar1), total_sugar2, total_sugar1)))
dat <- dat %>% mutate(total_fat = ifelse((!is.na(total_fat1)) & (!is.na(total_fat2)), (total_fat1 + total_fat2)/2.0, ifelse(is.na(total_fat1), total_fat2, total_fat1)))
dat <- dat %>% mutate(sodium = ifelse((!is.na(sodium1)) & (!is.na(sodium2)), (sodium1 + sodium2)/2.0, ifelse(is.na(sodium1), sodium2, sodium1)))

dat <- dat %>% select(SEQN, carbonhydrate, total_sugar, total_fat, sodium) %>% drop_na()

# now dat only has 7495 rows, less than the original 8704 rows by 1209 rows
# that's exactly the expected number of empty rows

dat <- inner_join(dat,dat_t, by = 'SEQN')
dat <- dat %>% filter(diabetes == 1 | diabetes == 2)


# Examination
# Important: BMI
# Jessie
# Tasks: data cleaning, data visualization

# drop RIDEXPRG
# merge DMDEDUC2, DMDEDUC3
# get rid of na value in DMDMARTL
demo <- read.xport("DEMO_J.XPT")
demo_clean <- demo %>% select(SEQN, DMDEDUC2, DMDEDUC3, DMDMARTL, INDFMIN2, INDFMPIR, 
                              INDHHIN2, RIAGENDR, RIDAGEYR, RIDRETH1, RIDRETH3) %>%
                        rename(highest_edu1 = DMDEDUC2, highest_edu2 = DMDEDUC3, 
                               marital_status = DMDMARTL, total_family_income = INDFMIN2,
                               income_vs_poverty = INDFMPIR, household_income = INDHHIN2,
                               gender = RIAGENDR,  age = RIDAGEYR, race = RIDRETH1,
                               race_non_Hispanic_Asian= RIDRETH3)
# If DMDEDUC2 has value, use values from DMDEDUC2, else use value from DMDEDUC3
demo_clean <- demo_clean %>% mutate(highest_edu = ifelse(is.na(highest_edu1) == FALSE, highest_edu1, highest_edu2)) %>%
                              select(-c(highest_edu1, highest_edu2))

# Assgin marital status with values 77-Refused, 99-Don't Know, and NA to 0-Not Specified
demo_clean <- demo_clean %>% mutate(marital_status = 
                                     ifelse(marital_status == 77 | marital_status == 99 | is.na(marital_status) == TRUE, 0, marital_status))
# Drop any rows with NULL values in total_family_income, income_vs_poverty, household_income, highest_edu
demo_clean <- demo_clean %>% filter(is.na(total_family_income) != TRUE 
                                    & is.na(income_vs_poverty) != TRUE 
                                    & is.na(household_income) != TRUE 
                                    & is.na(highest_edu) != TRUE)
summary(demo_clean)
# Join demographics data with other data and drop any rows with NULL values
# 5690 observation left
data <- left_join(demo_clean, dat, by = "SEQN") %>% drop_na()

# Add BMI to the cleaned data and drop any rows with NULL values
# 5631 observation left
exam <- read.xport("BMX_J.XPT")
data <- left_join(data, exam %>% select(SEQN, BMXBMI), by = "SEQN") %>% 
                    rename(BMI = BMXBMI) %>% 
                    drop_na()


# Laboratory
# Runting

# Tasks: data cleaning, data visualization

# # drop RIDEXPRG
# # merge DMDEDUC2, DMDEDUC3
# # get rid of na value in DMDMARTL
# demo <- read.xport("DEMO_J.XPT")
# demo_clean <- demo %>% select(DMDEDUC2, DMDEDUC3, DMDMARTL, INDFMIN2, INDFMPIR, 
#                               INDHHIN2, RIAGENDR, RIDAGEYR, RIDEXPRG, RIDRETH1, 
#                               RIDRETH3)
# summary(demo_clean)
# 
# # Missing DR1_300, DR1_320Z, DR1_330Z
# diet <- read.xport("DR1TOT_J.XPT")
# diet_clean <- diet %>% select(DBD100, DR1BWATZ, 
#                               DR1TALCO, DR1TB12A)
# summary(diet_clean)
# summary(diet)
# round(cor(diet), 2)
# diet_nona <- diet %>% filter()
# 
# diabetes <- read.xport("DIQ_J.XPT")
# sugar <- diet %>% select(SEQN, DR1TSUGR)
# result <- diabetes %>% select(SEQN, DIQ010)
# test <- inner_join(sugar, result, by = "SEQN") %>% filter(is.na(DR1TSUGR) != TRUE & is.na(DIQ010) != TRUE) %>%
#         filter(DIQ010 == 1 | DIQ010 == 2)
# 
# cor(test$DR1TSUGR, test$DIQ010)
# rcorr(as.matrix(test$DR1TSUGR, test$DIQ010))
# 
# chisq.test(table(test$DR1TSUGR, test$DIQ010))
# 
# library(ltm)
# biserial.cor(test$DR1TSUGR, test$DIQ010)