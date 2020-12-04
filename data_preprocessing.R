library(SASxport)
library(dplyr)
library(tidyverse)
library(ggcorrplot)
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
diet_1 <- diet_1 %>% select(SEQN, DR1TKCAL, DR1TCARB, DR1TSUGR, DR1TTFAT, DR1TSODI) %>% rename('energy1' = 'DR1TKCAL', 'carbonhydrate1' = 'DR1TCARB', 'total_sugar1' = 'DR1TSUGR', 'total_fat1' = 'DR1TTFAT', 'sodium1' = 'DR1TSODI')
diet_2 <- diet_2 %>% select(SEQN, DR2TKCAL, DR2TCARB, DR2TSUGR, DR2TTFAT, DR2TSODI) %>% rename('energy2' = 'DR2TKCAL', 'carbonhydrate2' = 'DR2TCARB', 'total_sugar2' = 'DR2TSUGR', 'total_fat2' = 'DR2TTFAT', 'sodium2' = 'DR2TSODI')
dat_t <- dat_t %>% select(SEQN, DIQ010) %>% rename('diabetes' = 'DIQ010') %>% drop_na()

# check that within diet_1 and diet_2, each row with na will have exactly 5 na
# diet_1_na <- rowSums(is.na(diet_1))
# diet_2_na <- rowSums(is.na(diet_2))

# use the following 2 lines of code to test if each row with na has exactly 5 na.
# turns out that this is true for both diet_1 and diet_2
# which(0 < diet_1_na  & diet_1_na < 5 )
# which(0 < diet_2_na  & diet_2_na < 5 )

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

dat <- dat %>% mutate(energy = ifelse((!is.na(energy1)) & (!is.na(energy2)), (energy1 + energy2)/2.0, ifelse(is.na(energy1), energy2, energy1)))
dat <- dat %>% mutate(carbonhydrate = ifelse((!is.na(carbonhydrate1)) & (!is.na(carbonhydrate2)), (carbonhydrate1 + carbonhydrate2)/2.0, ifelse(is.na(carbonhydrate1), carbonhydrate2, carbonhydrate1)))
dat <- dat %>% mutate(total_sugar = ifelse((!is.na(total_sugar1)) & (!is.na(total_sugar2)), (total_sugar1 + total_sugar2)/2.0, ifelse(is.na(total_sugar1), total_sugar2, total_sugar1)))
dat <- dat %>% mutate(total_fat = ifelse((!is.na(total_fat1)) & (!is.na(total_fat2)), (total_fat1 + total_fat2)/2.0, ifelse(is.na(total_fat1), total_fat2, total_fat1)))
dat <- dat %>% mutate(sodium = ifelse((!is.na(sodium1)) & (!is.na(sodium2)), (sodium1 + sodium2)/2.0, ifelse(is.na(sodium1), sodium2, sodium1)))

dat <- dat %>% select(SEQN, energy, carbonhydrate, total_sugar, total_fat, sodium) %>% drop_na()

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
demo_clean <- demo %>% select(SEQN, DMDEDUC2, DMDMARTL, INDFMIN2, INDFMPIR, 
                              INDHHIN2, RIAGENDR, RIDAGEYR, RIDRETH1, RIDRETH3) %>%
                        rename(highest_edu = DMDEDUC2, 
                               marital_status = DMDMARTL, total_family_income = INDFMIN2,
                               income_vs_poverty = INDFMPIR, household_income = INDHHIN2,
                               gender = RIAGENDR,  age = RIDAGEYR, 
                               race = RIDRETH3)
# If DMDEDUC2 has value, use values from DMDEDUC2, else use value from DMDEDUC3
#demo_clean <- demo_clean %>% mutate(highest_edu = ifelse(is.na(highest_edu1) == FALSE, highest_edu1, highest_edu2)) %>%
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

# Add HDL_Cholesterol to the cleaned data and drop any rows with NULL values
# 5205 observation left
data$SEQN <- as.numeric(data$SEQN)
lab3 <- read.xport("HDL_J.XPT")
lab3$SEQN <- as.numeric(lab3$SEQN)
data <- left_join(data, lab3 %>% select(SEQN, LBDHDD), by = "SEQN") %>% 
  rename(HDL_Cholesterol = LBDHDD) %>% 
  drop_na()

#revalue "no diabetes" from 2 to 0
data$diabetes[data$diabetes==2]<-0

#linear regression
fit <- lm(diabetes ~ marital_status+total_family_income+income_vs_poverty+
household_income+gender+age+race+race_non_Hispanic_Asian+highest_edu+energy+carbonhydrate+
total_sugar+total_fat+sodium+diabetes+BMI+HDL_Cholesterol, data=data)
summary(fit)


data$diabetes <- factor(data$diabetes)
mylogit <- glm(diabetes ~ marital_status+total_family_income+income_vs_poverty+
                 household_income+gender+age+race+race_non_Hispanic_Asian+highest_edu+energy+carbonhydrate+
                 total_sugar+total_fat+sodium+diabetes+BMI+HDL_Cholesterol, data = data, family = "binomial")
summary(mylogit)
#trying scatter plot
data$diabetes <- as.numeric(data$diabetes)
ggplot(data, aes(x=age, y=BMI, color = diabetes)) + geom_point()
#correlation plot
r <- cor(data, use="complete.obs")
ggcorrplot(r)

#revalue "no diabetes" from 1 to 0， and “diabetes” from 2 to 1
data$diabetes[data$diabetes==1] <- 0
data$diabetes[data$diabetes==2] <- 1


#pie chart
pie_chart<- function(diab, cat, level) {
  pie_dat <- as.data.frame(table(data %>% filter(diabetes ==diab)%>%select(cat)))%>%rename(cat = Var1)
  pie_dat $ cat<- level
  ggplot(pie_dat, aes(x="", y=Freq, fill=cat)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)
}

cats <- c("hightest_edu", "marital_status", "gender", "race")
level_edu <- c("Less than 9th grade","9-11th grade (Includes 12th grade with no diploma)",
               "High school graduate/GED or equivalent",	
               "Some college or AA degree",
               "College graduate or above",
               "Refused",
               "Don't Know")
level_marital <- c("Married", "Widowed", "Divorced", "Separated", "Never married",
                   "Living with partner", "Refused")	
level_gender <- c("Male", "Female")

level_race <- c("Mexican American",		
                "Other Hispanic",	
                "Non-Hispanic White",	
                "Non-Hispanic Black",	
                "Non-Hispanic Asian",	
                "Other Race - Including Multi-Racial")

pie_chart(0, "highest_edu", level_edu[-6])
pie_chart(1,"highest_edu", level_edu)
pie_chart(0, "marital_status", level_marital[-7])
pie_chart(1, "marital_status", level_marital)
pie_chart(0, "gender", level_gender)
pie_chart(1, "gender", level_gender)
pie_chart(0, "race", level_race)
pie_chart(1, "race", level_race)



# back to back histogram: age
level_age <- c("[0-10)","[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)","[60-70)", "[70-80)","[80-90)", "[90-100)")
d_age <- data %>% select(age, diabetes) %>% 
  mutate(age_tag = case_when(
    age < 10 ~ level_age[1],
    age >= 10 & age < 20 ~ level_age[2],
    age >= 20 & age < 30 ~ level_age[3],
    age >= 30 & age < 40 ~ level_age[4],
    age >= 40 & age < 50 ~ level_age[5],
    age >= 50 & age < 60 ~ level_age[6],
    age >= 60 & age < 70 ~ level_age[7],
    age >= 70 & age < 80 ~ level_age[8],
    age >= 80 & age < 90 ~ level_age[9])) %>% 
  group_by(age_tag, diabetes) %>% 
  summarise(percentage = n()) %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

# plot
age <- d_age %>% 
  ggplot(aes(x = age_tag, y = percentage, group = diabetes, fill = diabetes)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = level_age) +
  # another trick!
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, 0.1), 
                     labels = abs(seq(-1 , 1, 0.1))) +
  labs(x = "Age (years)", y = "Percentage", title = "Age-Diabetes Distribution Comparison") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Has Diabetes", "No Diabetes"),
                    labels=c("Has Diabetes", "No Diabetes")) 

print(age)

# back to back histogram: energy
level_energy <- c("[0-1000)","[1000-2000)", "[2000-3000)", "[3000-4000)", "[4000-5000)", "[5000-6000)","[6000-7000)", "[7000-8000)","[8000-9000)")
d_energy <- data %>% select(energy, diabetes) %>% 
  mutate(energy_tag = case_when(
    energy < 1000 ~ level_energy[1],
    energy >= 1000 & energy < 2000 ~ level_energy[2],
    energy >= 2000 & energy < 3000 ~ level_energy[3],
    energy >= 3000 & energy < 4000 ~ level_energy[4],
    energy >= 4000 & energy < 5000 ~ level_energy[5],
    energy >= 5000 & energy < 6000 ~ level_energy[6],
    energy >= 6000 & energy < 7000 ~ level_energy[7],
    energy >= 7000 & energy < 8000 ~ level_energy[8],
    energy >= 8000 & energy < 9000 ~ level_energy[9])) %>% 
  group_by(energy_tag, diabetes) %>% 
  summarise(percentage = n()) %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

# plot
energy <- d_energy %>% 
  ggplot(aes(x = energy_tag, y = percentage, group = diabetes, fill = diabetes)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = level_energy) +
  # another trick!
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, 0.1), 
                     labels = abs(seq(-1 , 1, 0.1))) +
  labs(x = "Energy (kcal)", y = "Percentage", title = "Daily Energy Intake - Diabetes Distribution Comparison") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Has Diabetes", "No Diabetes"),
                    labels=c("Has Diabetes", "No Diabetes")) 

print(energy)

# back to back histogram: carbonhydrate
level_carb <- c("[0-100)","[100-200)", "[200-300)", "[300-400)", "[400-500)", "[500-600)","[600-700)", "[700-800)","[800-900)", "[900-1000)", "[1000-1100)")
d_carb <- data %>% select(carbonhydrate, diabetes) %>% 
  mutate(carb_tag = case_when(
    carbonhydrate < 100 ~ level_carb[1],
    carbonhydrate >= 100 & carbonhydrate < 200 ~ level_carb[2],
    carbonhydrate >= 200 & carbonhydrate < 300 ~ level_carb[3],
    carbonhydrate >= 300 & carbonhydrate < 400 ~ level_carb[4],
    carbonhydrate >= 400 & carbonhydrate < 500 ~ level_carb[5],
    carbonhydrate >= 500 & carbonhydrate < 600 ~ level_carb[6],
    carbonhydrate >= 600 & carbonhydrate < 700 ~ level_carb[7],
    carbonhydrate >= 700 & carbonhydrate < 800 ~ level_carb[8],
    carbonhydrate >= 800 & carbonhydrate < 900 ~ level_carb[9],
    carbonhydrate >= 900 & carbonhydrate < 1000 ~ level_carb[10],
    carbonhydrate >= 1000 & carbonhydrate < 1100 ~ level_carb[11])) %>% 
  group_by(carb_tag, diabetes) %>% 
  summarise(percentage = n()) %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

# plot
carb <- d_carb %>% 
  ggplot(aes(x = carb_tag, y = percentage, group = diabetes, fill = diabetes)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = level_carb) +
  # another trick!
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, 0.1), 
                     labels = abs(seq(-1 , 1, 0.1))) +
  labs(x = "Carbonhydrate (gm)", y = "Percentage", title = "Daily Carbonhydrate Intake - Diabetes Distribution Comparison") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Has Diabetes", "No Diabetes"),
                    labels=c("Has Diabetes", "No Diabetes")) 

print(carb)


# back to back histogram: sugar
level_sugar <- c("[0-100)","[100-200)", "[200-300)", "[300-400)", "[400-500)", "[500-600)","[600-700)", "[700-800)","[800-900)", "[900-1000)")
d_sugar <- data %>% select(total_sugar, diabetes) %>% 
  mutate(sugar_tag = case_when(
    total_sugar < 100 ~ level_sugar[1],
    total_sugar >= 100 & total_sugar < 200 ~ level_sugar[2],
    total_sugar >= 200 & total_sugar < 300 ~ level_sugar[3],
    total_sugar >= 300 & total_sugar < 400 ~ level_sugar[4],
    total_sugar >= 400 & total_sugar < 500 ~ level_sugar[5],
    total_sugar >= 500 & total_sugar < 600 ~ level_sugar[6],
    total_sugar >= 600 & total_sugar < 700 ~ level_sugar[7],
    total_sugar >= 700 & total_sugar < 800 ~ level_sugar[8],
    total_sugar >= 800 & total_sugar < 900 ~ level_sugar[9],
    total_sugar >= 900 & total_sugar < 1000 ~ level_sugar[10])) %>% 
  group_by(sugar_tag, diabetes) %>% 
  summarise(percentage = n()) %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

# plot
sugar <- d_sugar %>% 
  ggplot(aes(x = sugar_tag, y = percentage, group = diabetes, fill = diabetes)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = level_sugar) +
  # another trick!
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, 0.1), 
                     labels = abs(seq(-1 , 1, 0.1))) +
  labs(x = "Total Sugar (gm)", y = "Percentage", title = "Daily Sugar Intake - Diabetes Distribution Comparison") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Has Diabetes", "No Diabetes"),
                    labels=c("Has Diabetes", "No Diabetes")) 

print(sugar)

# back to back histogram: fat
level_fat <- c("[0-50)","[50-100)", "[100-150)", "[150-200)", "[200-250)", "[250-300)","[300-350)", "[350-400)")
d_fat <- data %>% select(total_fat, diabetes) %>% 
  mutate(fat_tag = case_when(
    total_fat < 50 ~ level_fat[1],
    total_fat >= 50 & total_fat < 100 ~ level_fat[2],
    total_fat >= 100 & total_fat < 150 ~ level_fat[3],
    total_fat >= 150 & total_fat < 200 ~ level_fat[4],
    total_fat >= 200 & total_fat < 250 ~ level_fat[5],
    total_fat >= 250 & total_fat < 300 ~ level_fat[6],
    total_fat >= 300 & total_fat < 350 ~ level_fat[7],
    total_fat >= 350 & total_fat < 400 ~ level_fat[8])) %>% 
  group_by(fat_tag, diabetes) %>% 
  summarise(percentage = n()) %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

# plot
fat <- d_fat %>% 
  ggplot(aes(x = fat_tag, y = percentage, group = diabetes, fill = diabetes)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = level_fat) +
  # another trick!
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, 0.1), 
                     labels = abs(seq(-1 , 1, 0.1))) +
  labs(x = "Total Fat (gm)", y = "Percentage", title = "Daily Fat Intake - Diabetes Distribution Comparison") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Has Diabetes", "No Diabetes"),
                    labels=c("Has Diabetes", "No Diabetes")) 

print(fat)

# back to back histogram: sodium
level_sodium <- c("[0-2000)","[2000-4000)", "[4000-6000)", "[6000-8000)", "[8000-10000)", "[10000-12000)","[12000-14000)", "[14000-16000)","[16000-18000)", "[18000-20000)", "[20000-22000)", "[22000-24000)", "[24000-26000)")
d_sodium <- data %>% select(sodium, diabetes) %>% 
  mutate(sodium_tag = case_when(
    sodium < 2000 ~ level_sodium[1],
    sodium >= 2000 & sodium < 4000 ~ level_sodium[2],
    sodium >= 4000 & sodium < 6000 ~ level_sodium[3],
    sodium >= 6000 & sodium < 8000 ~ level_sodium[4],
    sodium >= 8000 & sodium < 10000 ~ level_sodium[5],
    sodium >= 10000 & sodium < 12000 ~ level_sodium[6],
    sodium >= 12000 & sodium < 14000 ~ level_sodium[7],
    sodium >= 14000 & sodium < 16000 ~ level_sodium[8],
    sodium >= 16000 & sodium < 18000 ~ level_sodium[9],
    sodium >= 18000 & sodium < 20000 ~ level_sodium[10],
    sodium >= 20000 & sodium < 22000 ~ level_sodium[11],
    sodium >= 22000 & sodium < 24000 ~ level_sodium[12],
    sodium >= 24000 & sodium < 26000 ~ level_sodium[13])) %>% 
  group_by(sodium_tag, diabetes) %>% 
  summarise(percentage = n()) %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

# plot
sodium <- d_sodium %>% 
  ggplot(aes(x = sodium_tag, y = percentage, group = diabetes, fill = diabetes)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = level_sodium) +
  # another trick!
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, 0.1), 
                     labels = abs(seq(-1 , 1, 0.1))) +
  labs(x = "Total Sodium (mg)", y = "Percentage", title = "Daily Sodium Intake - Diabetes Distribution Comparison") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Has Diabetes", "No Diabetes"),
                    labels=c("Has Diabetes", "No Diabetes")) 

print(sodium)


# Laboratory
# Runting
# lab1 <- read.xport("CRCO_J.XPT")
# lab1 <- lab1 %>% select(SEQN, LBXBCR, LBXBCO) %>% rename(Chromium = LBXBCR, Cobalt = LBXBCO)
# lab2 <- read.xport("INS_J.XPT")
# lab2 <- lab2 %>% select(SEQN, LBXIN) %>% rename(Insulin = LBXIN)
# lab3 <- read.xport("HDL_J.XPT")
# lab3 <- lab3 %>% select(SEQN, LBDHDD) %>% rename(HDL_Cholesterol = LBDHDD)
# lab <- full_join(lab1, lab2, by = "SEQN")
# lab <- lab %>% full_join(lab3, by = "SEQN")
# head(lab)
#print(class(lab3$SEQN) == class(lab$SEQN))
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

