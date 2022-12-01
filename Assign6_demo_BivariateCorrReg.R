# DEMOSTRATION OF A PROCESS FOR PART 3 OF ASSIGNMENT 6
# Sticking with my interest in the association
# between the level of POVERTY in the neighborhood
# and the prevalence of ASTHMA in the area.

# We can look at this association with
# BIVARIATE CORRELATION and REGRESSION.
# We can use a variety of variable types using these tools
# but our DV must be an interval-level variable.

# The variables I'm interested in from the data:
# CASTHMA # people with asthma per 1,000 population
# pov13_17 Proportion people w/ income past 12 months 


# I downloaded the data, now I have to tell R where to access the data.
getwd()   #this command allows you to see CURRENT working directory
setwd("C:\\Users\\Kyle\\Downloads") #setting a new working directory

# loading the full tract-level data from Deleware
load("DATA_Neighborhood_Health_Data_Delaware_WIDE.rda")

# following line creates a data object with just the variables
# I need from frame called DEL_NHs that I will manipulate locally.
# Keeping just a few variables (columns) that I will use.
# Here I am keeping my IV and DV plus another variable
# that I will use for illustrative purposes.
DEL_NHs <- PLACES_wide[ , c("ppov13_17", "CASTHMA", "popden08_12")]

# looking at the distribution of my two variables
summary(DEL_NHs$ppov13_17)
summary(DEL_NHs$CASTHMA)

# Looking at the correlation between my focal variables
# Specifying a Pearson Correlation coefficient (rather
# than a Spearman or other rank-order type).
# Last part of the command tells R to just focus on those
# cases with complete data (no missing infor) on the
# variables in the analysis.
cor(DEL_NHs$CASTHMA, DEL_NHs$ppov13_17, method="pearson", use = "complete.obs")

# Following line does an hypothesis test for my focal
# association.  Here the null hypothesis is that the
# correlation in the population (rho) = 0
cor.test(DEL_NHs$CASTHMA, DEL_NHs$ppov13_17, method="pearson")


# VISUALIZING THE ASSOCIATION (optional)
# Because I'm fancy, I want to visualize the association
# between my variables using a scatterplot.
# Here I'll install and use a simple function called
# "ggscatter" within the package called "ggpubr"
install.packages("ggpubr")
library("ggpubr")
ggscatter(DEL_NHs, x = "ppov13_17", y = "CASTHMA", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Poverty Rate", ylab = "Asthma/1000")


#fit simple linear regression model
reg_results <- lm(DEL_NHs$CASTHMA ~ DEL_NHs$ppov13_17)
summary(reg_results)  #view the results


# ========================================================

# BONUS EXAMPLE: regression with a set of dummy variables
# Say that my theoretical argument implies an association
# between population density and asthma.  Specifically, the
# argument holds that neighborhoods with more than 10,000
# people per square mile will have particularly high levels
# of asthma while those with fewer than 400 people per
# square mile will have very low levels of asthma.

# To test these ideas I will create two dummy variables:
# 1) high_density will take a value of 1 for those
#    neighborhoods with at least 10,000/sqmile and
#    0 for those that do not fit this criterion.
# 2) low_density will take a value of 1 for those
#    neighborhoods with no more than 400/sqmile and
#    0 for those that do not fit this criterion.

# using "ifelse" to create a dummy variable
# code says: create a new variable called "high-density" and
# give it a value of 1 for those cases where the value
# of the variable "popden08_12" is greater than or equal to
# 10,000, and a value of 0 otherwise.
DEL_NHs$high_density <- ifelse(DEL_NHs$popden08_12 >= 10000,1,0)
table(DEL_NHs$high_density)  #this line just asks for a table of the results

# using "ifelse" to create another dummy variable
DEL_NHs$low_density <- ifelse(DEL_NHs$popden08_12 <= 400,1,0)
table(DEL_NHs$low_density)  #this line just asks for a table of the results

# NOTE that including both high_density and low_density in
# a regression model, the reference category will be those
# neighborhoods taking a value of 0 on BOTH of these
# dummy variables.  These are the neighborhoods with
# density > 400 but < 10,000.
# >>>>ANY TIME YOU USE DUMMY VARIABLES IN A REGRESSION
#     ANALYSIS, YOU SHOULD INCLUDE k-1 DUMMY VARIABLES
#     WHERE k = THE NUMBER OF GROUPS YOU WANT TO CONTRAST. 
reg_results2 <- lm(DEL_NHs$CASTHMA ~ DEL_NHs$high_density + DEL_NHs$low_density)
summary(reg_results2)

