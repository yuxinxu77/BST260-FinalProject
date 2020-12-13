#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("shinythemes")
library(shinythemes)
library(shiny)
library(dplyr)
library(tidyverse)
library(ggcorrplot)

data <- read_csv('../data_preprocess.csv')[-1]
data_onehot <- read_csv('../data_onehot.csv')[-1]
feature <- colnames(data)[-1]
feature_pie <- setNames(c("highest_edu", "marital_status", "gender", "race", "total_family_income"), 
                        c("highest_edu", "marital_status", "gender", "race", "total_family_income"))
feature_back <- setNames(c("age", "energy", "carbonhydrate", "total_sugar", "total_fat", "sodium"), 
                         c("age", "energy", "carbonhydrate", "total_sugar", "total_fat", "sodium"))
## Later
feature_name <- 0

# data visualization
level_edu <- c("Less than 9th grade","9-11th grade (Includes 12th grade with no diploma)",
               "High school graduate/GED or equivalent",	
               "Some college or AA degree",
               "College graduate or above")
level_marital <- c("Married", "Widowed", "Divorced", "Separated", "Never married",
                   "Living with partner")	
level_gender <- c("Male", "Female")

level_race <- c("Mexican American",		
                "Other Hispanic",	
                "Non-Hispanic White",	
                "Non-Hispanic Black",	
                "Non-Hispanic Asian",	
                "Other Race - Including Multi-Racial")
level_income <- c("$0 to $4,999", 
                  "$5,000 to $9,999", 
                  "$10,000 to $14,999",
                  "$15,000 to $19,999",
                  "$20,000 to $24,999", 
                  "$25,000 to $34,999", 
                  "$35,000 to $44,999", 
                  "$45,000 to $54,999", 
                  "$55,000 to $64,999", 
                  "$65,000 to $74,999", 
                  "$75,000 to $99,999", 
                  "$100,000 and Over")

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
  summarise(percentage = n(), .groups = "drop") %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

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
  summarise(percentage = n(), .groups = "drop") %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

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
  summarise(percentage = n(), .groups = "drop") %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

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
  summarise(percentage = n(), .groups = "drop") %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

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
  summarise(percentage = n(), .group = "drop") %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

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
  summarise(percentage = n(), .group = "drop") %>% 
  mutate(percentage = ifelse(diabetes == 1, -percentage/sum(data$diabetes == 1), percentage/sum(data$diabetes == 0))) %>% 
  mutate(diabetes = ifelse(diabetes == 0, "No Diabetes", "Has Diabetes"))

################# Hist & Density ##############
feature_hist <- setNames(c("income_vs_poverty", "BMI", "HDL_Cholesterol"), 
                         c("income_vs_poverty", "BMI", "HDL_Cholesterol"))

mean_ip <- data %>% select(income_vs_poverty, diabetes) %>%
  mutate(diabetes = ifelse(diabetes == 1, "Diabetes", "No Diabetes")) %>%
  group_by(diabetes) %>%
  summarise(mean = mean(income_vs_poverty), .groups = "drop")

mean_bmi <- data %>% select(BMI, diabetes) %>%
  mutate(diabetes = ifelse(diabetes == 1, "Diabetes", "No Diabetes")) %>%
  group_by(diabetes) %>%
  summarise(mean = mean(BMI), .groups = "drop")

mean_cho <- data %>% select(HDL_Cholesterol, diabetes) %>%
  mutate(diabetes = ifelse(diabetes == 1, "Diabetes", "No Diabetes")) %>%
  group_by(diabetes) %>%
  summarise(mean = mean(HDL_Cholesterol), .groups = "drop")




# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"), 
    navbarPage("Predictive Risk of Diebetes",
               tabPanel("Home",
                        fluidRow(
                          img(src='front.jpeg', height = 350, style="display: block; margin-left: auto; margin-right: auto;"),
                          h1("Overview"),
                          htmlOutput("text_background"),
                          h1("Motivation"),
                          htmlOutput("text_motivation")
                        )
                ),
               tabPanel("Data Preprocessing", 
                        fluidRow(
                          h1("Table View of Feature Statistics"),
                          p("You can select which feature to view, we provide count data for categorical view and quantile data for numeric data.")
                        ),
                        fluidRow(
                          htmlOutput("text_choice")
                        ),
                        fluidRow(
                          column(4, selectInput("feature_select", "Features of the Data", setNames(feature, feature))),
                          column(8, tableOutput("table_preprocess"))
                        ),
                        fluidRow(
                          htmlOutput("text_preprocess"),
                          h3("Diet Data Preprocessing"),
                          htmlOutput("text_preprocess_diet"),
                          h3("Demographic Data Preprocessing"),
                          htmlOutput("text_preprocess_demo"),
                          h3("Target Feature and Extreme Values"),
                          htmlOutput("text_preprocess_other")
                        )
                ),
               tabPanel("Data Visualization", 
                        tabsetPanel(
                          tabPanel("Pie Chart", 
                                   fluidRow(
                                     h1("Pie Chart for Categorical Features"),
                                     p("You can select which feature to view. 
                                       We provide seperated pie charts for people 
                                       with diabetes and people without diabetes.")
                                   ),
                                   fluidRow(
                                     selectInput("feature_pie", "Features to Visualize", feature_pie)
                                   ),
                                   fluidRow(
                                     plotOutput("pie_chart1")
                                   ), 
                                   fluidRow(
                                     plotOutput("pie_chart2")
                                   ), 
                                   fluidRow(
                                     htmlOutput("text_pie")
                                   )
                          ),
                          tabPanel("Back to Back Histogram", 
                                   fluidRow(
                                     h1("Back-to-Back Histogram for Selected Numerical Features"),
                                     p("You can select which feature to view. 
                                       We provide Back-to-Back Histogram for age and diet related features.")
                                   ),
                                   fluidRow(
                                     selectInput("feature_back", "Features to Visualize", feature_back)
                                   ),
                                   fluidRow(
                                     plotOutput("back_chart")
                                   ), 
                                   fluidRow(
                                     htmlOutput("text_back")
                                   )
                          ),
                          tabPanel("Histogram and Density Plot", 
                                   fluidRow(
                                     h1("Dual Histogram with Density Plot for Selected Numerical Features"),
                                     p("You can select which feature to view. 
                                       We provide Dual Histogram with Density Plot for demographic and labs related features.")
                                   ),
                                   fluidRow(
                                     selectInput("feature_hist", "Features to Visualize", feature_hist)
                                   ),
                                   fluidRow(
                                     plotOutput("hist_chart")
                                   ), 
                                   fluidRow(
                                     htmlOutput("text_hist")
                                   )
                          ), 
                          tabPanel("Correlation Plot", 
                                   fluidRow(
                                     h1("Correlation Plot for All Features"),
                                     plotOutput("corr_chart")
                                   ), 
                                   fluidRow(
                                     htmlOutput("text_corr")
                                   )
                          )
                        )
                ),
               tabPanel("Data Pipeline",
                        fluidRow(
                          htmlOutput("text_onehot")
                        ),
                        fluidRow(
                          htmlOutput("text_split")
                        ),
                        fluidRow(
                          h1("PCA"),
                          p("The below plot shows the significance of each priciple components."),
                          img(src='pca1.png', height = 350, style="display: block; margin-left: auto; margin-right: auto;"),
                          p("The below plots show the contribution of the original features to the priciple components."),
                          img(src='pca2.png', height = 275),
                          img(src='pca3.png', height = 250),
                          htmlOutput("text_pca")
                        )
                ),
               tabPanel("Machine Learning Models",
                        tabsetPanel(
                          tabPanel("Logistic Regression",
                                   fluidRow(
                                     h1("Logistic Regression"),
                                     p("Confusion Matrix and ROC Curve"),
                                     img(src='glm_cm.png', height = 400),
                                     img(src='glm_roc.png', height = 350),
                                     htmlOutput("text_lr")
                                   )
                          ),
                          tabPanel("Support Vector Machine",
                                   fluidRow(
                                     h1("Support Vector Machine"),
                                     p("Confusion Matrix and ROC Curve"),
                                     img(src='svm_cm.png', height = 400),
                                     img(src='svm_roc.png', height = 350),
                                     htmlOutput("text_svm")
                                   )
                          ),
                          tabPanel("KNN",
                                   fluidRow(
                                     h1("KNN"),
                                     p("Confusion Matrix and ROC Curve"),
                                     img(src='knn_cm.png', height = 400),
                                     img(src='knn_roc.png', height = 350),
                                     htmlOutput("text_knn")
                                   )
                          ),
                          tabPanel("Random Forest",
                                   fluidRow(
                                     h1("Random Forest"),
                                     p("Confusion Matrix and ROC Curve"),
                                     img(src='rf_cm.png', height = 400),
                                     img(src='rf_roc.png', height = 350),
                                     htmlOutput("text_rf")
                                   )
                          ),
                          tabPanel("Boosting",
                                   fluidRow(
                                     h1("Boosting"),
                                     p("Confusion Matrix and ROC Curve"),
                                     img(src='b_cm.png', height = 400),
                                     img(src='b_roc.png', height = 350),
                                     htmlOutput("text_bt")
                                   )
                          )
                        )
                ),
               tabPanel("Conclusion", 
                        fluidRow(
                          h1("Conclusion"),
                          img(src='conclude.jpg', height = 350, style="display: block; margin-left: auto; margin-right: auto;"),
                          htmlOutput("text_con")
                        )
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ############ Introduction and Background ############
  output$text_background <- renderText({
    paste("<p>",
          "Diabetes is a group of metabolic disorders characterized by a high 
          \blood sugar level over a prolonged period of time. Diabetes prevalence 
          has been rising more rapidly among adults over 18 years of age from 
          4.7% in 1980 to 8.5% in 2014. On the one hand, the onset of type 2 
          diabetes is highly associated with an unhealthy lifestyle. On the other 
          hand, diabetes is a major cause of various diseases and was the seventh 
          leading cause of death in 2016 (data from WHO).",
          "</p>", 
          "<br>", 
          "<p>",
          "What we did in this project: ",
          "</p>",
          "<ul>", 
            "<li>Preprcoess real world data</li>", 
            "<li>Visualize feature selected</li>", 
            "<li>Build data pipeline</li>", 
            "<li>Construct machine learning models</li>",
            "</ul>"
          )
  })
  
  output$text_motivation <- renderText({
    paste("<p>",
          "Given this situation, our motivation for this project is: first, to 
          find and analyze possible risk factors of diabetes, such as diet, 
          smoking, health condition, and demographic and socioeconomic factors. 
          Based on our results, we could encourage people to follow healthier 
          lifestyles and therefore to delay or avoid the onset of type 2 diabetes. 
          Secondly, we would like to study the potential health-related influences 
          of diabetes according to their lab test results and questionnaire 
          answers. Finally, we would like to forecast the risk of an individual 
          developing diabetes given the current living condition. We hope that 
          these findings can lead to a better understanding of diabetes and help 
          patients to prevent diabetes.",
          "</p>")
  })
  
  ############# Data Preprocessing #################
  data_proprocess_cat <- reactive({as.data.frame(table(data %>% select(input$feature_select), dnn = list("Category")), responseName = "Count")})
  data_proprocess_num <- reactive({as.data.frame(summary(data %>% select(input$feature_select))) %>% select(Freq) %>% rename("Quantile" = Freq)})
  
  output$table_preprocess <- renderTable(
    if (input$feature_select %in% c("highest_edu", "marital_status", "gender", "race", "total_family_income", "diabetes")){
      data_proprocess_cat()
    }
    else{
      data_proprocess_num()
    }
  )
  
  output$text_choice <- renderText({
    if (input$feature_select %in% c("highest_edu", "marital_status", "gender", "race", "total_family_income")){
      paste("<p>", 
            "The feature you selected is categorical.", 
            "</p>")
    }
    else{
      paste("<p>", 
            "The feature you selected is numeric.", 
            "</p>")
    }
  })
  
  output$text_preprocess <- renderText({
    paste("<h1>Data Preprocessing</h1>", "<br>", "<p>", 
          "Our dataset contains a target variable in the form of a single-choice questionnaire answer, and features from four categories: diet, demographics, examination and lab.",
          "</p>",
          "<p>There are a few steps that we performed across all categories of data:</p>",
          "<ul>", 
          "<li>Preprcoess real world data</li>", 
          "<li>Visualize feature selected</li>", 
          "<li>Build data pipeline</li>", 
          "<li>Construct machine learning models</li>",
          "</ul>",
          "<p>But there are still some steps that are special to specific categories that we went through.</p>")
  })
  
  output$text_preprocess_diet <- renderText({
    paste("<p>", 
          "Since in diet data, all wanted features may have both data from day 1 and day 2, we replace the values with the mean from the two days if both exists, and keep the value where on data from one of the day exists.", 
          "</p>")
  })
  
  output$text_preprocess_demo <- renderText({
    paste("<p>", 
          "In order to decrease the number of categories inside feature \"marital status\", we reassign answers representing \"Refused\", \"Don't know\" and NA values to the category \"Not Specified\".", 
          "</p>")
  })
  
  output$text_preprocess_other <- renderText({
    paste("<p>", 
          "For target data, we filtered the dataset to make sure that the target variable (\"If doctors told the participant in the patients in the past that they have diabetes\") only contains factors that represent yes and no.", 
          "</p>",
          "<br>", 
          "<p>", 
          "To simplify our classification problem, we decided to delete rows where the value of feature \"highest education\" and feature \"marital status\" only rarely appear in our dataset.", 
          "</p>")
  })
  
  ##################### data visulization ################
  
  ##################### Pie Chart ########################

  # data_pie0 <- reactive({as.data.frame(table(data %>% filter(diabetes == 0) %>% select(input$feature_pie))) %>% rename(input$feature_pie = Var1)})
  data_pie0 <- reactive({as.data.frame(table(data %>% filter(diabetes == 0)%>%select(input$feature_pie)))})
  data_pie1 <- reactive({as.data.frame(table(data %>% filter(diabetes == 1) %>% select(input$feature_pie)))})
  
  ##### Add Title on the plot #######
  output$pie_chart1 <- renderPlot({
    if(input$feature_pie == "highest_edu"){
      data_pie0() %>% mutate(level = level_edu) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People Without Diabetes")
    }
    else if(input$feature_pie == "marital_status"){
      data_pie0() %>% mutate(level = level_marital) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People Without Diabetes")
    }
    else if(input$feature_pie == "gender"){
      data_pie0() %>% mutate(level = level_gender) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People Without Diabetes")
    }
    else if(input$feature_pie == "race"){
      data_pie0() %>% mutate(level = level_race) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People Without Diabetes")
    }
    else{
      data_pie0() %>% mutate(level = level_income) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People Without Diabetes")
    }
  })
  
  output$pie_chart2 <- renderPlot({
    if(input$feature_pie == "highest_edu"){
      data_pie1() %>% mutate(level = level_edu) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People With Diabetes")
    }
    else if(input$feature_pie == "marital_status"){
      data_pie1() %>% mutate(level = level_marital) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People With Diabetes")
    }
    else if(input$feature_pie == "gender"){
      data_pie1() %>% mutate(level = level_gender) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People With Diabetes")
    }
    else if(input$feature_pie == "race"){
      data_pie1() %>% mutate(level = level_race) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People With Diabetes")
    }
    else{
      data_pie1() %>% mutate(level = level_income) %>% 
        ggplot(aes(x="", y=Freq, fill = level)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        ggtitle("Pie Chart for People With Diabetes")
    }
  })
  
  output$text_pie <-renderText({
    if(input$feature_pie == "highest_edu"){
      paste("<h1>Pie Chart Analysis</h1>", "<br>", "<p>", 
            "First, for the highest education level, people with diabetes tend to have lower educational background than the other group. A greater fraction of people with diabetes have education levels: less than 9th grade, 9-11th grade, and high school graduate, than that of the non-diabetes group, while the non-diabetes group has more college graduate or above education background.", 
            "</p>")
    }
    else if(input$feature_pie == "marital_status"){
      paste("<h1>Pie Chart Analysis</h1>", "<br>", "<p>", 
            "For marital status, more people are married or divorced in the diabetes group compared to the non-diabetes group. However, since marital status highly depends on age, and age is known to be a key risk factor for diabetes, this factor could be a confounder.", 
            "</p>")
    }
    else if(input$feature_pie == "gender"){
      paste("<h1>Pie Chart Analysis</h1>", "<br>", "<p>", 
            "The pie charts of gender show an interesting point. While females are slightly more than males in the non-diabetes group, more males appear in the diabetes group. This may tell us that males have a higher risk of diabetes than females.", 
            "</p>")
    }
    else if(input$feature_pie == "race"){
      paste("<h1>Pie Chart Analysis</h1>", "<br>", "<p>", 
            "The distributions of races in these two groups are very similar, except that the diabetes group has more Mexican Americans and less Non-Hispanic Whites. Overall, race seems not to be a key feature for predicting diabetes.", 
            "</p>")
    }
    else{
      paste("<h1>Pie Chart Analysis</h1>", "<br>", "<p>", 
            "Lastly, looking at total family income, we might find that the fraction of people with the highest income ($100,000 and over, $75,000 to $99,999, and $65,000 to $74,999) in the non-diabetes group is greater than the diabetes group.", 
            "</p>")
    }
  })
  
  ##################### Back to Back Chart ########################
  output$back_chart <- renderPlot({
    if(input$feature_back == "age"){
      d_age %>% 
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
    }
    else if(input$feature_back == "energy"){
      d_energy %>% 
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
    }
    else if(input$feature_back == "carbonhydrate"){
      d_carb %>% 
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
    }
    else if(input$feature_back == "total_sugar"){
      d_sugar %>% 
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
    }
    else if(input$feature_back == "total_fat"){
      d_fat %>% 
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
      
    }
    else{
      d_sodium %>% 
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
      
    }
  })
  
  output$text_back <- renderText({
    if(input$feature_back == "age"){
      paste("<h1>Back-to-back Histogram Analysis</h1>", "<br>", "<p>",
            "According to CDC, more than 90% American diabetes patient has type two diabetes, which most often develops in people more than age 45. Our data accurately captured this skew in age distribution among diabetes patients: patients above age 40 takes up 95% of the whole population of positively diagnosed patients. In the contrary, the distribution of the healthy population spreads more evenly between 20 and 70, and only drastically decreased in the age group 70 or above. This distribution makes sense, since the elderly population might be less responsive to surveys compared to younger age groups.",
            "</p>")
    }
    else{
      paste("<h1>Back-to-back Histogram Analysis</h1>", "<br>", "<p>",
            "In terms of diet distribution, patients diagnosed with diabetes don't differ much from the healthy population. But there is one consistent pattern across the five graphs: diagnosed patients in general tend to consume less compared to the healthy. This differ from our expectation, since we expect to see the diabetes patients having a less-as-healthy diet. However, given that all diabetes patients already know there diagnosis and will thus pay more attention to their daily consumption, this difference is reasonable.",
            "</p>")
    }
  })

  
  ################## Hist & Density ###############
  data_hist <- reactive({data %>% select(input$feature_hist, diabetes) %>%
      mutate(diabetes = ifelse(diabetes == 1, "Diabetes", "No Diabetes")) %>%
      rename(Diabetes = diabetes)})
  
  output$hist_chart <- renderPlot({
    if(input$feature_hist == "income_vs_poverty"){
      data_hist() %>% 
        ggplot(aes(x=income_vs_poverty, color=Diabetes, fill=Diabetes)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 15) +
        geom_density(alpha=0.4) +
        geom_vline(data = mean_ip, aes(xintercept=mean, color=diabetes),
                   linetype="dashed")+
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        labs(title="Ratio of Family Income to Poverty",x="Family Income Ratio", y = "Density")+
        theme_classic()
    }
    else if(input$feature_hist == "BMI"){
      data_hist() %>% 
        ggplot(aes(x=BMI, color=Diabetes, fill=Diabetes)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
        geom_density(alpha=0.4) +
        geom_vline(data = mean_bmi, aes(xintercept=mean, color=diabetes),
                   linetype="dashed")+
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        labs(title="Body Mass Index",x="Body Mass Index (kg/m**2)", y = "Density")+
        theme_classic()
    }
    else{
      data_hist() %>% 
        ggplot(aes(x=HDL_Cholesterol, color=Diabetes, fill=Diabetes)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
        geom_density(alpha=0.4) +
        geom_vline(data = mean_cho, aes(xintercept=mean, color=diabetes),
                   linetype="dashed")+
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        labs(title="Direct HDL-Cholesterol",x="Direct HDL-Cholesterol (mg/dL)", y = "Density")+
        theme_classic()
    }
  })
  
  output$text_hist <- renderText({
    if(input$feature_hist == "income_vs_poverty"){
      paste("<h1>Dual Histogram with Density Plot Analysis</h1>", "<br>", "<p>",
            "The difference between distribution of diabetes patients and that of the healthy is not big, but in general, diabetes patients tend to have less family income compared to the healthy. Considering the fact that illness might potentially decrease patients' ability of making money, the difference is consistent to the reality.",
            "</p>")
    }
    else if(input$feature_hist == "BMI"){
      paste("<h1>Dual Histogram with Density Plot Analysis</h1>", "<br>", "<p>",
            "The difference between Body Mass Index distributions of diabetes patients and the healthy is still small, but BMI distribution is more right skewed compared to that of the healthy, implying a higher BMI. This is consistent with the fact that BMI is positively correlated with risk of diabetes.",
            "</p>")
    }
    else{
      paste("<h1>Dual Histogram with Density Plot Analysis</h1>", "<br>", "<p>",
            "According to the American Heart Association, diabetes patients tend to have a higher cholesterol levels. This fact is reflected in oure data: the Cholesterol distribution of diabetes patients in general is more right-skewed than that of the healthy.",
            "</p>")
    }
  })
  
  ########### Corr plot ###########
  output$corr_chart <- renderPlot({
    r <- cor(data, use="complete.obs")
    ggcorrplot(r)
    })
  
  output$text_corr <- renderText({
    paste("<h1>Correlation Plot Analysis</h1>", "<br>", "<p>",
          "We then constructed a heatmap to inspect the correlation between features. Not to our surprise, features from the diet category are highly correlated (which makes sense, since if people eat more or less, the amount of their carbonhydrate, sugar, sodium and fat intake will likely increase or decrease accordingly). On the other hand, the high correlation between total family income and income-poverty ratio is also understandable, given that they are likely just a multiple of each other. There are also some other explainable correlations displayed in the heatmap: the correlation between age and marital status, between highest education degree and income-poverty ratio, between daily energy intake and gender, etcetera.",
          "</p>")
  })
  
  ######## End of Visualization Section ########
  
  ######## Pipeline Section ########
  output$text_onehot <- renderText({
    paste("<h1>One-hot Encoding</h1>", "<br>", "<p>",
          "We then use one-hot encoding to convert categorical features into multiple columns that eliminates the unnecessary ordinality between categeories.",
          "</p>")
  })
  
  output$text_split <- renderText({
    paste("<h1>Train-test-split</h1>", "<br>", "<p>",
          "We then randomly split the data into training set (80%) and testing set (20%), and created a balanced training set by random over-sampling minority examples.",
          "</p>")
  })
  
  output$text_pca <- renderText({
    paste("<p>",
          "We apply PCA analysis on data after one-hot coding because we noticed many feature columns contain a lot of zero and PCA analysis can extract useful features and result in a smaller dataset. We also want to see how computer construct features from the cleaned dataset. We first apply full PCA analysis on the dataset to visualized the contribution of each features to the principle components. We noticed that the most significant principle component is constructed main by diet related features, which include energy, total fat intake etc. The second significant principle component is constructed main by demographics related features, which include energy, total fat intake etc. We then picked the first 10 principle components and apply the resulting transformation to the training and testing data for constructing machine learning models.",
          "</p>")
  })
  
  output$text_lr <- renderText({
    paste("<p>",
          "Logistic Regression is a linear classifier that can be used for prediction. We applied logistics regression on four training datasets with/without PCA and balanced/non-balanced.",
          "</p>",
          "<br>",
          "<p>",
          "Logistic regression achieved its best performance when we were using balanced data without PCA transformation. Imbalanced data caused the model to obtain very low sensitivity rate and the model fitted with imbalanced data can not correctly identify most of the diabetes patients. PCA transformation did not make significance differences in the model performances but model with PCA transformation tends to lower the specificity comparing with model without PCA transformation.",
          "</p>")
  })

  output$text_svm <- renderText({
    paste("<p>",
          "SVM stands for support vector machine. This binary classification algorithm aims to create a hyperplane in the hyperspace of data with features being the dimensions, that split the two classes to different sides as much as possible. In our training, we used linear kernal, did 10-fold cross validation, and trained using dataset with principal component analysis and oversampling, dataset without principal component analysis and with oversampling, dataset with principal component analysis and without oversampling, and dataset without principal component analysis or oversampling. We also normalized each feature before training so that they all have zero mean and unit variance.",
          "</p>",
          "<br>",
          "<p>",
          "Turns out that the best model trained using SVM algorithm is the one trained with dataset oversampled and without principal component analysis.",
          "</p>",
          "<br>",
          "<p>",
          "It's not hard to see why SVM performs better in a balanced dataset using oversampling. Since SVM uses penalty to update the hyperplane at each round, if the data is significantly imbalanced, SVM is going to penalize more by the dominant class and affected less by the class with fewer data. In our case, we care more about the class with less instances (the positive class). As a result, it's easy to see that a balanced dataset will produce a better performance.",
          "</p>",
          "<br>",
          "<p>",
          "We suspect the reason that SVM performs better with principal component analysis is that PCA filtered out some spatial information in the 28 features we picked that could be important to the training of SVM. ",
          "</p>")
  })

  output$text_knn <- renderText({
    paste("<p>",
          "KNN stands for k-nearest neighbor. It assumes that similar objects belongs to the the same class. For each data point in the test set, it finds the nearest k neighbors with classes, look at their classes and assign the class with most vote to the data point. For KNN, we set k to 18, and trained using dataset with principal component analysis and oversampling, dataset without principal component analysis and with oversampling, dataset with principal component analysis and without oversampling, and dataset without principal component analysis or oversampling.",
          "</p>",
          "<br>",
          "<p>",
          "For KNN, the best model is the one trained with dataset oversampled and with principal component analysis.",
          "</p>",
          "<br>",
          "<p>",
          "It's also not a surprise that KNN performs better in a balanced dataset. KNN is, in its essence, a communty-level voting system that considers a majority of vote as a win. So no wonder the dominant class will use its dominance in number to affect the voting result, even when in some cases, data points have some neighbors from the non-dominant class and actually belong to that class themselves. A balanced dataset can avoid much of this negative effect.",
          "</p>",
          "<br>",
          "<p>",
          "KNN requires a dense distribution of data points in the hyperspace of data, and is thus very vulnerable to the curse of dimensionality. With principal component analysis decreasing th dimensions, for KNN, it is imaginable that this help outweighs the loss of information caused by principal component analysis.",
          "</p>")
  })

  output$text_rf <- renderText({
    paste("<p>",
          "We trained Random Forest models on four training sets, with/without PCA and balanced/not balanced, respectively. After carefully tuning parameters and plotting ROC curves, we found that random forest models are very sensitive to imbalanced data. In our original training set, there are 1549 negative(0) observations, but only 500 positive(1) observations. Therefore, there is a significant probability that a bootstrap sample contains few or even none of the minority class, resulting in a tree with poor performance for predicting the minority class.(https://statistics.berkeley.edu/tech-reports/666) Indeed, two of our models, trained by original imbalanced data, can hardly discriminate between 0 and 1. Instead, they classify almost everyone into the negative class.",
          "</p>",
          "<br>",
          "<p>",
          "Furthermore, in the two models trained by balanced data, we found that the model without PCA gives a better result, possibly because some information was lost in the process of performing PCA.",
          "</p>",
          "<br>",
          "<p>",
          "Finally, from all Random Forest models, we chose the model trained by balanced data without PCA. The area under the ROC curve (AUC) is 0.83, suggesting that it is a fairly good classification model. As high sensitivity is our first goal, we set a threshold such that sensitivity is high enough while specificity is acceptable. ",
          "</p>")
  })

  output$text_bt <- renderText({
    paste("<p>",
          "Gradient boosting models tell a different story. Similarly, as training Random Forest model, we tried on four training datasets with/without PCA and balanced/non-balanced.",
          "</p>",
          "<br>",
          "<p>",
          "However, in this case, PCA had a strong negative impact on model performance. On the other hand, the boosting model is very robust to handle imbalanced data: both models training by balanced/imbalanced data give similar result, with AUC around 0.83. To keep the model training process simple, we chose the one trained by original imbalanced data.",
          "</p>")
  })
  
  output$text_con <- renderText({
    paste("<p>",
          "Based on the descriptive analysis and the predictive analysis we did previously, we were able to view and make prediction on whether a people with giving data is currently under high risks of developing diabetes. We conclude that diabetes patients above age 20 tend to be male, tends to be not as wealthy, tend to follow a healthier diet because of the disease, and tend to have higher BMI. The results of our project can be utilized to improve the health quality of the population. If the diabetes condition of a person id predicted to be positive, it does not indicate that the person is definitely developing diabetes. Since our models we optimized by the criteria of sensitivity, we are able to capture most of the of the people who is having diabetes, but our false positive rate is also relatively high.",
          "</p>", 
          "<br>", 
          "<p>", 
          "Thus, people whoever get positive results from our model is suggested to conduct a diabetes test from medical center. Moreover, positive results also indicate that under current diet patterns and other conditions, one might be under a great risk of developing diabetes even this disease is not detected now.", 
          "</p>")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
