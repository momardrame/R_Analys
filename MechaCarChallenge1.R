

MechaCar_MPG_Table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)


# Use tidyverse's dplyr library to transforms R data.
library(tidyverse)

# MPG Regression
# Using multiple linear regression, we designed a linear model that predicts the mpg of MechaCar prototypes using a number of variables within the MechaCar mpg dataset.
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_MPG_Table) #generate multiple linear regression model

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_MPG_Table)) #generate summary statistics

 # Data points to use for our line plot 
MPG_model1 <- lm(mpg ~ vehicle_length,MechaCar_MPG_Table) #create linear model
MPG_yvals1 <- MPG_model1$coefficients['vehicle_length']*MechaCar_MPG_Table$vehicle_length + MPG_model1$coefficients['(Intercept)'] 
# Plot the linear model over our scatter plot:
plt1 <- ggplot(MechaCar_MPG_Table,aes(x=vehicle_length,y=mpg)) 
plt1 + geom_point() + geom_line(aes(y=yvals), color = "red") 

MPG_model2 <- lm(mpg ~ vehicle_weight,MechaCar_MPG_Table) 
MPG_yvals2 <- MPG_model2$coefficients['vehicle_weight']*MechaCar_MPG_Table$vehicle_weight + MPG_model2$coefficients['(Intercept)'] 
plt2 <- ggplot(MechaCar_MPG_Table,aes(x=vehicle_weight,y=mpg)) 
plt2 + geom_point() + geom_line(aes(y=MPG_yvals2), color = "red") 

MPG_model3 <- lm(mpg ~ spoiler_angle,MechaCar_MPG_Table) 
MPG_yvals3 <- MPG_model3$coefficients['spoiler_angle']*MechaCar_MPG_Table$spoiler_angle + MPG_model3$coefficients['(Intercept)'] 
plt3 <- ggplot(MechaCar_MPG_Table,aes(x=spoiler_angle,y=mpg))
plt3 + geom_point() + geom_line(aes(y=MPG_yvals3), color = "blue") 

MPG_model4 <- lm(mpg ~ ground_clearance,MechaCar_MPG_Table)
MPG_yvals4 <- MPG_model4$coefficients['ground_clearance']*MechaCar_MPG_Table$ground_clearance + MPG_model4$coefficients['(Intercept)'] #determine y-axis values from linear model
plt4 <- ggplot(MechaCar_MPG_Table,aes(x=ground_clearance,y=mpg)) 
plt4 + geom_point() + geom_line(aes(y=MPG_yvals4), color = "red") #plot scatter and linear model

MPG_model5 <- lm(mpg ~ AWD,MechaCar_MPG_Table)
MPG_yvals5 <- MPG_model5$coefficients['AWD']*MechaCar_MPG_Table$AWD + MPG_model5$coefficients['(Intercept)'] 
plt5 <- ggplot(MechaCar_MPG_Table,aes(x=AWD,y=mpg)) 
plt5 + geom_point() + geom_line(aes(y=MPG_yvals5), color = "blue")


# Suspension Coil Summary

Summary_Statistics_Table <- MechaCar_Suspension_Table %>% group_by(PSI) %>% summarize(Mean= mean(MechaCar_Suspension_Table$PSI), Median= median(MechaCar_Suspension_Table$PSI), Variance= var(MechaCar_Suspension_Table$PSI), Standard_Deviation= sd(MechaCar_Suspension_Table$PSI))
View(Summary_Statistics_Table)

# Suspension Coil T-Test

population_table <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F) 
plt <- ggplot(population_table,aes(x=PSI)) 
plt + geom_density() 

# Sample dataset using dplyr's sample_n() function.
sample_table <- population_table %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table,aes(x=log10(PSI))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

#  first Sample t-test
t.test((sample_table$PSI),mu=mean(population_table$PSI)) #compare sample and population means

# Second Sample t-test
sample_table <- population_table %>% sample_n(50) 
sample_table2 <- population_table %>% sample_n(50) 
t.test((sample_table$PSI),(sample_table2$PSI))


