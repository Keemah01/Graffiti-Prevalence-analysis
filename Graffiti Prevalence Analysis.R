#############################
#######Hikmat's R SCRIPT#######


######################################
#####Call the necessary libaries######
library(haven)
library(ggplot2)
library(dplyr)
library(skimr)
library(stargazer)
library(caret)
library(erer)
library(rattle)
library(randomForest)
library(broom)
library(RColorBrewer)



#####Set the Working Directory#####
setwd("C:/Users/Windows/Documents/B.I")



#####Query the Crime Data#####
Crime_Data <- read_dta("crime_survey (1).dta")



####Explore the data ######
skim(Crime_Data)
summary(Crime_Data)




#######Select relevant Variables for the purpose of this analysis########
Crime_Data1 <- Crime_Data %>% select(vandcomm,rubbcomm,tenure1,ethgrp2a,rural2,
                                     work2,poorhou,sex,agegrp7,bcsvictim,edeprivex)




#######Data Cleaning######
######Filter na's and outliers ######
Crime_Data1 <- Crime_Data1 %>% filter(!is.na(edeprivex),!is.na(ethgrp2a),
                                      tenure1 != 8,vandcomm != 5,rubbcomm != 5,
                                      poorhou != 5, tenure1!= 9, work2 != 8)



##########################################
####### Data Transformation Section ######


#######Transform the outcome variable Vandcomm#####
####### Grouping it into 1 & 0, 1 if graffiti is common and 0 otherwise ######
###### Create Categorical for Random Forest ########

Crime_Data1 <- Crime_Data1 %>% mutate(Graffiti = ifelse(vandcomm <= 3,1,0),
                                      Graffiti2 = ifelse(Graffiti == 1,"Common", "Notcommon"))
Crime_Data1$Graffiti <- as.numeric(Crime_Data1$Graffiti)
table(Crime_Data1$Graffiti2)





#####Transform the key explanatory variable rural###
##### 1 for urban, 0 for rural ####
Crime_Data1 <- Crime_Data1 %>% mutate(Urban = ifelse(rural2 == 1,1,0))
Crime_Data1$Urban <- as.numeric(Crime_Data1$Urban)
table(Crime_Data1$Urban)




###### Transform Rubbcomm to indicate if the area is littered or not####
Crime_Data1 <- Crime_Data1 %>% mutate(Littered = ifelse(rubbcomm<= 3,1,0))
table(Crime_Data1$Littered)
Crime_Data1$Littered <- as.numeric(Crime_Data1$Littered)




#####Create a new variable to indicate areas with poor house 1 and 0 otherwise#####
Crime_Data1 <- Crime_Data1 %>% mutate(Poor_house = ifelse(poorhou<= 3,1,0))
Crime_Data1$Poor_house <- as.numeric(Crime_Data1$Poor_house)
table(Crime_Data1$Poor_house)




##### Create a dummy variable, Gender variable, 1 to indicate male and 0 for female #####
Crime_Data1 <- Crime_Data1 %>% mutate(Male = ifelse(sex == 1,1,0))
Crime_Data1$Male <- as.numeric(Crime_Data1$Male)
table(Crime_Data1$Male)




####Create a dummy variable those that are  crime victim 1, 0 for those that aren't ###
Crime_Data1 <- Crime_Data1 %>% mutate(Crime_Victim = ifelse(bcsvictim== 1,1,0))
Crime_Data1$Crime_Victim <- as.numeric(Crime_Data1$Crime_Victim)
table(Crime_Data1$Crime_Victim)




#### Create dummy variable for those that are working 1, 0 for those that aren't ###
Crime_Data1 <- Crime_Data1 %>% mutate(working = ifelse(work2== 1,1,0))
Crime_Data1$working <- as.numeric(Crime_Data1$working)
table(Crime_Data1$working)




#### Create a dummy variable for mostdeprived indicating 1 and least deprived 0###
Crime_Data1 <- Crime_Data1 %>% mutate(Most_deprived = ifelse(edeprivex>= 3,1,0))
Crime_Data1$Most_deprived <- as.numeric(Crime_Data1$Most_deprived)
table(Crime_Data1$Most_deprived)




#####Transform Tenure into dummy variables Owned and Rented, k-1#####
Crime_Data1 <- Crime_Data1 %>%
  mutate(
    Owned = ifelse(tenure1 <= 2, 1, 0),
    Rented = ifelse(tenure1 > 2 & tenure1 <= 4, 1, 0))
Crime_Data1$Owned<- as.numeric(Crime_Data1$Owned)
Crime_Data1$Rented <- as.numeric(Crime_Data1$Rented)
table(Crime_Data1$Owned)
table(Crime_Data1$Rented)




####Transform the age group variable into dummy variables, k-1######
Crime_Data1 <- Crime_Data1 %>%
  mutate(
    Age_16_24 = ifelse(agegrp7 == 1, 1, 0),
    Age_25_34 = ifelse(agegrp7 == 2, 1, 0),
    Age_35_44 = ifelse(agegrp7 == 3, 1, 0),
    Age_45_54 = ifelse(agegrp7 == 4, 1, 0))

Crime_Data1$Age_16_24 <- as.numeric(Crime_Data1$Age_16_24)
Crime_Data1$Age_25_34 <- as.numeric(Crime_Data1$Age_25_34)
Crime_Data1$Age_35_44 <- as.numeric(Crime_Data1$Age_35_44)
Crime_Data1$Age_45_54 <- as.numeric(Crime_Data1$Age_45_54)
table(Crime_Data1$Age_16_24)
table(Crime_Data1$Age_25_34)
table(Crime_Data1$Age_35_44)
table(Crime_Data1$Age_45_54)




# Transform the ethnic group variable into dummy variables
Crime_Data1 <- Crime_Data1 %>%
  mutate(
    Ethnic_Mixed = ifelse(ethgrp2a == 2, 1, 0),
    Ethnic_Asian_Bri = ifelse(ethgrp2a == 3, 1, 0),
    Ethnic_Black_Bri = ifelse(ethgrp2a == 4, 1, 0),
    Ethnic_Chinese = ifelse(ethgrp2a == 5, 1, 0),cid = row_number())

Crime_Data1$Ethnic_Mixed <- as.numeric(Crime_Data1$Ethnic_Mixed)
Crime_Data1$Ethnic_Asian_Bri <- as.numeric(Crime_Data1$Ethnic_Asian_Bri)
Crime_Data1$Ethnic_Black_Bri <- as.numeric(Crime_Data1$Ethnic_Black_Bri)
Crime_Data1$Ethnic_Chinese<- as.numeric(Crime_Data1$Ethnic_Chinese)





######Select the variables for the purpose of this analysis####
Crime_Data1 <- Crime_Data1 %>% select(Graffiti, Graffiti2, Urban,Littered,Poor_house, 
                                      Owned,Rented,Age_16_24, Age_25_34, Age_35_44, 
                                      Age_45_54,Most_deprived,Ethnic_Mixed, 
                                      Ethnic_Chinese, Ethnic_Asian_Bri,Ethnic_Black_Bri,Male,
                                      Crime_Victim,working, cid)




#####Check The final state before analysing ####
skim(Crime_Data1)


#######Descriptive Statistics#####
summary(Crime_Data1)



######### Save the estimation, to call easily######
myformula <- formula(Graffiti ~ Urban + Littered + Poor_house + Owned + 
                       Rented + Age_16_24 + Age_25_34+ Age_35_44 + Age_45_54 + Most_deprived +
                       Ethnic_Mixed + Ethnic_Black_Bri + Ethnic_Asian_Bri + 
                       Ethnic_Chinese + Male + Crime_Victim + working )




#####For random forest, since we can't use numeric####
myformula2 <- formula(Graffiti2 ~ Urban + Littered + Poor_house + Owned + 
                        Rented + Age_16_24 + Age_25_34+ Age_35_44 + Age_45_54 + Most_deprived +
                        Ethnic_Mixed + Ethnic_Black_Bri + Ethnic_Asian_Bri + 
                        Ethnic_Chinese + Male + Crime_Victim + working )






###################################
#### Analysis  Session  ###########



######################################
###### Exploratory Data Analysis #####

#### Figure1: Graffiti Prevalence ####
F1 <- Crime_Data1 %>%
  count(Graffiti) %>%
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x = factor(Graffiti), y = pct, fill = factor(Graffiti), label = scales::percent(pct))) +
  geom_col() +
  geom_text(position = position_dodge(width = .8), vjust = -0.8, size = 3) + 
  expand_limits(y = c(0, .85)) +
  theme(
    panel.border = element_blank(), 
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    text = element_text(size = 14, family = "mono", face = "bold"),
    legend.position = "none") +
  scale_x_discrete(breaks = c(1, 0), labels = c("Common", "Not Common")) +
  scale_fill_manual(values = c("#2171B5","#08306B")) +
  labs(x = "Graffiti", y = "Proportion (%)", title = "Graffiti Prevalence")





###### Figure2: Graffiti By Area ######

F2 <- ggplot(data = Crime_Data1, aes(x = Graffiti, fill = factor(Urban))) + 
  geom_bar(position = "fill") +
  theme(
    panel.border = element_blank(), 
    axis.line = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    text = element_text(size = 14, family = "mono", face = "bold")) +
  scale_x_continuous(breaks = c(1, 0), labels = c("Common", "Not common")) +
  scale_fill_manual(labels = c("Rural", "Urban"), values = c("#2171B5", "#08306B")) +
  labs(x = "Graffiti", y = "Proportion", title = "Graffiti By Area")




#######Linear Probability Model for the Data #####
####Bivariate ###
prob <- lm(data = Crime_Data1, Graffiti ~ Urban)
summary(prob)



#####Multiple#####
prob1 <- lm(data = Crime_Data1,formula = myformula)
summary(prob1)

######To show in console####
stargazer(prob,prob1, type = "text")


######To download tabular form #####
stargazer(prob,prob1, type = "html",out = "Prob")






#######################################
###### PREDICTION ANALYSIS SECTION ####

set.seed(3)

##### Split the data ####
### 80%  for the train data, 20% for test ###
train <- Crime_Data1 %>% sample_frac(0.8)
test  <- anti_join(Crime_Data1, train, by = "cid")




##############################
##### Logistic Model #########
logit <- glm(data = train, myformula, family = binomial(link = "logit"))
summary(logit)


#####Calculate marginal effect #####
logit3 <- glm(data = train,formula = myformula, family = binomial(link = "logit"),x=TRUE)
summary(logit3)
maBina(logit3,x.mean = FALSE, rev.dum = FALSE,digits=4)

#####To get the tabular form#####
stargazer (maBina(logit3,x.mean = FALSE, rev.dum = FALSE,digits=4), type = "html", out = "Mabina1")


###specify the prediction###
logit_pred <- augment(logit, type.predict = "response", newdata = test)
summary(logit_pred$.fitted)
logit_pred <- logit_pred %>% mutate(pred_common_logit = ifelse(.fitted > 0.5,"Common","Notcommon"))

#### Logit confusion matrix
table(logit_pred$pred_common_logit, logit_pred$Graffiti, useNA = "ifany")

# accuracy: (995 + 500)/1617 = 92.4%
# sensitivity: 500/(105+500) = 82.6%
# specificity: 995/(995+17) = 98.3%





##########################
####linear probability model

lm <- lm(data = train, myformula)
summary(lm)

lm_pred <- augment(lm, newdata = test)
summary(lm_pred$.fitted)

# accuracy: (969+ 501)/1617= 90.9%
# sensitivity: 501/(131+501) = 79.2%
# specificity: 969/(969+16) = 98.3%





#######################################
##### Advanced Analysis ###############

#####Manually selecting variables #####
glm(data = Crime_Data1, formula = Graffiti ~ Urban, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Poor_house, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Littered,family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Male, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Owned, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Rented, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Age_16_24, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Age_25_34, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Age_45_54, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Most_deprived, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Ethnic_Chinese, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Ethnic_Black_Bri, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Ethnic_Asian_Bri, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti ~ Ethnic_Mixed, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti~ Crime_Victim, family = binomial(link = "logit"))
glm(data = Crime_Data1, formula = Graffiti~ working, family = binomial(link = "logit"))


####### Automated ##########
####### random forest#######
rf.caret <- train(myformula2, 
                  data = Crime_Data1,
                  metric = "Accuracy",
                  method = "rf",
                  trControl = trainControl(method = "cv", number = 10, classProbs = TRUE))
rf.caret
pred_rf.caret <- predict(rf.caret, Crime_Data1)
table(pred_rf.caret, Crime_Data1$Graffiti2)

varImp(rf.caret)


# accuracy: (5092+ 2359)/8086= 92.1%
# sensitivity: 2359/(533+2359) = 81.5%
# specificity: 5092/(5092+102) = 98%






###########################################
#### PREDICTION VISUALIZATION SECTION #####

### Figure 3: Predicted output for graffiti prevalence####
plot_frame <- data.frame(Prediction = c("Notcommon", "Common"),
                         Count = c(1000, 600))
plot_frame$Percentage <- plot_frame$Count / sum(plot_frame$Count) * 100
F3 <- ggplot(plot_frame, aes(x = Prediction, y = Percentage, fill = Prediction)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  labs(x = "Graffiti", y = "Propotion", title = "Predicted Output for Graffiti prevalence") +
  theme_minimal() +
  theme(
    panel.border = element_blank(), 
    axis.line = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position = "",
    text = element_text(size = 14, family = "mono", face = "bold")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(labels = c("Notcommon" = "Not Common", "Common" = "Common")) +
  scale_fill_manual(values = c("Notcommon" = "#2171B5", "Common" = "#08306B"))







####### Figure 4: Accuracy vs number of predictors

# Extract the results from the random forest model
rf_results <- rf.caret$results


F4 <- ggplot(rf_results, aes(x = mtry, y = Accuracy)) +
  geom_line(color = "#08306B") +  
  geom_point(color = "#2171B5") + 
  labs(title = "Accuracy vs. Number of Predictors",
       x = "Number of Predictors (mtry)",
       y = "Accuracy") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "mono", face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"))






######Figure 5:Variable Importance Plot###
#### Assign variable importance
var_importance <- varImp(rf.caret)

# Convert to data frame for ggplot2
var_importance_df <- as.data.frame(var_importance$importance)
var_importance_df$Variables <- rownames(var_importance_df)

F5 <- ggplot(var_importance_df, aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#2171B5") +  # Set the bar color to blue
  coord_flip() +  # Flip the coordinates for better readability
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "mono", face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"))

