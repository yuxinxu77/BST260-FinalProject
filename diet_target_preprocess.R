library(dplyr)
library(tidyverse)
library(SASxport)
data_dir <- './data'
setwd(data_dir)

# read in datasets
dat1 <- read.xport('DR1TOT_J.XPT')
dat2 <- read.xport('DR2TOT_J.XPT')
dat_t <- read.xport("DIQ_J.XPT")


# select useful columns and rename column names
dat1 <- dat1 %>% select(SEQN, DR1TCARB, DR1TSUGR, DR1TTFAT, DR1TSODI) %>% rename('carbonhydrate1' = 'DR1TCARB', 'total_sugar1' = 'DR1TSUGR', 'total_fat1' = 'DR1TTFAT', 'sodium1' = 'DR1TSODI')
dat2 <- dat2 %>% select(SEQN, DR2TCARB, DR2TSUGR, DR2TTFAT, DR2TSODI) %>% rename('carbonhydrate2' = 'DR2TCARB', 'total_sugar2' = 'DR2TSUGR', 'total_fat2' = 'DR2TTFAT', 'sodium2' = 'DR2TSODI')
dat_t <- dat_t %>% select(SEQN, DIQ010) %>% rename('diabetes' = 'DIQ010') %>% drop_na()

# check that within dat1 and dat2, each row with na will have exactly four na
# dat1_na <- rowSums(is.na(dat1))
# dat2_na <- rowSums(is.na(dat2))

# use the following 2 lines of code to test if each row with na has exactly 4 na.
# turns out that this is true for both dat1 and dat2
# which(0 < dat1_na  & dat1_na < 4 )
# which(0 < dat2_na  & dat2_na < 4 )

# now we can go ahead and full_join dat1 and dat2
dat <- full_join(dat1, dat2, by = 'SEQN')

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