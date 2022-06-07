#Reading the CSV file
tobacco_data = read.csv(file = '/Users/jaswanthsai/Documents/HIT 750/Project/Project_coding/BRFSS_Prevalence_and_Trends_Data__Tobacco_Use_-_Four_Level_Smoking_Data_for_1995-2010.csv')
# Trying to display the first few columns of data.
head(tobacco_data)
#Summary of the data set
summary(tobacco_data)
#Column names of the dataset
colnames(tobacco_data)
#Datatype of columns in a dataframe
str(tobacco_data)
#Unique States in the dataset
unique_states=unique(tobacco_data$State)
paste('The total number of unique states being considered',length(unique_states))
#Unique Years in the dataset
unique_years = unique(tobacco_data$Year)
paste('The total number of unique years unders considerattion',length(unique_years))
#let’s check whether the tobacco data has NULL
paste('The tobacco dataset has null values:',is.null(tobacco_data))


#Histogram plot for people who smoke everyday
hist(tobacco_data$Smoke.everyday)

#Histogram plot for people who smoke few days
hist(tobacco_data$Smoke.some.days)

#Histogram plot for people who previously smoked but quit now
hist(tobacco_data$Former.smoker)

#Histogram plot for people who never smoked
hist(tobacco_data$Never.smoked)


# Removing location column because we do not need it 
df = subset(tobacco_data, select = -c(Location.1))
head(df)

# Highest Smoking percentage recorded in year and the state
highest_state_rec = df[which.max(df$Smoke.everyday),]
highest_state = df[df$State==highest_state_rec$State,]
highest_state = highest_state[order(highest_state$Year),]  
print(highest_state)
plot(highest_state$Year, highest_state$Smoke.everyday, type = "l", col = 1 ,main='Highest Smoking percentage recorded in a year') 

#Smallest Smoking percentage recorded in year and the state
smallest_state_rec = df[which.max(df$Smoke.everyday),]
smallest_state = df[df$State==smallest_state_rec$State,]
smallest_state = smallest_state[order(smallest_state$Year),]  
print(smallest_state)
plot(smallest_state$Year, smallest_state$Smoke.everyday, type = "l", col = 2 ,main='Smallest Smoking percentage recorded in a year') 

#################
#looking at how the trends have been changing over the years
install.packages("reshape2")
library(reshape2)
install.packages('ggplot2')
library('ggplot2')
install.packages("gridExtra")
library(gridExtra)
library(grid)
data_long = melt(df, id=c('Year','State'))
print(data_long)

graph_data = readline(prompt = "Enter the nummber of States you want to observe the trend for: ");
for(i in 1:graph_data){
  state_name = readline(prompt = "Enter the State you want to observe the trend for: ");
  state_name1 = noquote(state_name)
  data_frame_mod <- data_long[data_long$State==state_name1,]
  print(data_frame_mod)
  
  data_frame_mod = data_frame_mod[order(factor(data_frame_mod$variable, levels = unique(data_frame_mod$variable)), data_frame_mod$Year),]
  cat("\nAfter melting data frame and reordering:\n")
  print(data_frame_mod)
  data_frame_mod_1 <- data_frame_mod[data_frame_mod$variable=="Smoke.everyday",]
  plot(data_frame_mod_1$Year, data_frame_mod_1$value, type = "l", col = 2,xlab='Year',ylab='Percentage Smoking Everyday',main='Smoking Everyday')
  myplot <- ggplot(data_frame_mod,
                   aes(x=Year,
                       y=value,
                       col=variable))+
    geom_line(aes(color = variable))+ggtitle(paste0('Year wise summary for different types of smokers for state of ',state_name1))
  print(myplot)}


#Regression Analysis

#Converting 'State' column into numeric for regression
tobacco_data$State<-factor(tobacco_data$State)
tobacco_data$State<-as.numeric(tobacco_data$State)
tobacco_data

#Installing reguired Packages
#install.packages('GGally')
library(GGally)


#Splitting the dataset testing and training in the ratio 80:20
split<- sample(c(rep(0, 0.8 * nrow(tobacco_data)), rep(1, 0.2 * nrow(tobacco_data))))
train <- tobacco_data[split == 0, ]
test <- tobacco_data[split== 1, ]   

#Predicting people who smoke everyday
lm_tobacco_1=lm(test$Smoke.everyday~test$State+test$Smoke.some.days+test$Former.smoker+test$Never.smoked)
length(predict(lm_tobacco_1))
length(tobacco_data$Smoke.everyday)
plt_1<-ggplot(test,aes(x=predict(lm_tobacco_1),y=Smoke.everyday))+
  geom_point()+
  geom_smooth(formula=y~x,method=lm,se=FALSE,color='red',fullrange=TRUE)+
  theme_classic()+ggtitle("Actual vs Predicted values for people who smoke everyday")+labs(y= "Actual", x = "Predicted")
plt_1
#Determing performance measures RMSE,R2 for this prediction
RSS_1<- c(crossprod(lm_tobacco_1$residuals))
MSE_1<- RSS_1/ length(lm_tobacco_1$residuals)
RMSE_1<- sqrt(MSE_1)
print((RMSE_1))
summary(lm_tobacco_1)$r.squared

#Predicting people who smoke some days
lm_tobacco_2=lm(tobacco_data$Smoke.some.days~tobacco_data$State+tobacco_data$Smoke.everyday+tobacco_data$Former.smoker+tobacco_data$Never.smoked)
plt_2<-ggplot(tobacco_data,aes(x=predict(lm_tobacco_2),y=Smoke.some.days))+
  geom_point()+
  geom_smooth(formula = y ~ x,method=lm,se=FALSE,color='red',fullrange=TRUE)+
  theme_classic()+ggtitle("Actual vs Predicted values for people who smoke some days")+labs(y= "Actual", x = "Predicted")
plt_2
#Determing performance measures RMSE,R2 for this prediction
RSS_2<- c(crossprod(lm_tobacco_2$residuals))
MSE_2<- RSS_2/ length(lm_tobacco_2$residuals)
RMSE_2<- sqrt(MSE_2)
print((RMSE_2))
summary(lm_tobacco_2)$r.squared

#Predicting people who are former smokers
lm_tobacco_3=lm(tobacco_data$Former.smoker~tobacco_data$State+tobacco_data$Smoke.everyday+tobacco_data$Smoke.some.days+tobacco_data$Never.smoked)
plt_3<-ggplot(tobacco_data,aes(x=predict(lm_tobacco_3),y=Former.smoker))+
  geom_point()+
  geom_smooth(formula = y ~ x,method=lm,se=FALSE,color='red',fullrange=TRUE)+
  theme_classic()+ggtitle("Actual vs Predicted values for people who are former smokers")+labs(y= "Actual", x = "Predicted")
plt_3
#Determing performance measures RMSE,R2 for this prediction
RSS_3<- c(crossprod(lm_tobacco_3$residuals))
MSE_3<- RSS_3/ length(lm_tobacco_3$residuals)
RMSE_3<- sqrt(MSE_3)
print((RMSE_3))
summary(lm_tobacco_3)$r.squared


#Predicting people who never smoked
lm_tobacco_4=lm(tobacco_data$Never.smoked~tobacco_data$State+tobacco_data$Smoke.everyday+tobacco_data$Smoke.some.days+tobacco_data$Former.smoker)
plt_4<-ggplot(tobacco_data,aes(x=predict(lm_tobacco_4),y=Never.smoked))+
  geom_point()+
  geom_smooth(formula = y ~ x,method=lm,se=FALSE,color='red',fullrange=TRUE)+
  theme_classic()+ggtitle("Actual vs Predicted values for people who never smoked")+labs(y= "Actual", x = "Predicted")
plt_4
#Determing performance measures RMSE,R2 for this prediction
RSS_4<- c(crossprod(lm_tobacco_4$residuals))
MSE_4<- RSS_4/ length(lm_tobacco_4$residuals)
RMSE_4<- sqrt(MSE_4)
print((RMSE_4))
summary(lm_tobacco_4)$r.squared