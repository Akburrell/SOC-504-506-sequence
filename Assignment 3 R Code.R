#question 1
state_covid <- read.csv("state_covid.csv", stringsAsFactors = FALSE, colClasses = c(fips = "character"))
state_covid$restrictions


#question 2
state_covid$restrictions <- factor(state_covid$restrictions, levels = c("low", "medium", "high"))
clean_covid <- state_covid[!is.na(state_covid$pop) &  !is.na(state_covid$restrictions), ]
clean_covid$restrictions
sort(clean_covid$restrictions)
as.integer(clean_covid$restrictions)
table(clean_covid$restrictions)

#question 3
table(state_covid$restrictions, useNA ="always")


#question 4
state_covid$region_name[is.na(state_covid$region_name)] <- "Unrepresented"
midwest <- state_covid[state_covid$region_name=="Midwest",]
table(midwest$restrictions)
south<- state_covid[state_covid$region_name=="South",]
table(south$restrictions)
northeast <- state_covid[state_covid$region_name=="Northeast",]
table(northeast$restrictions)
unrepresented <- state_covid[state_covid$region_name=="Unrepresented",]
table(unrepresented$restrictions)

UR <- as.data.frame(table(unrepresented$restrictions))
UR$region_name <- "Unrepresented"

NE <- as.data.frame(table(northeast$restrictions))
NE$region_name <- "Northeast"

S <- as.data.frame(table(south$restrictions))
S$region_name <- "South"

MW <- as.data.frame(table(midwest$restrictions))
MW$region_name <- "Midwest"

table(c(UR, NE, S, MW))
all <- rbind(UR, NE, S, MW)

#question 5

state_covid$state
state_covid$"Puerto_Rico" & state_covid$pop
state_covid[43,11] <- "3,193,694"
state_covid$pop[43]  <- "3,193,694"
state_covid$pop[state_covid$state == "Puerto Rico"] <- "3,193,694"

#question 6

state_covid.nona<- data.frame(na.omit(state_covid, pop))
na.omit(state_covid)

#question 7 
as.integer(clean_covid$pop)
clean_covid$pop
class(clean_covid$pop)
clean_covid$pop <- as.integer(clean_covid$pop)
clean_covid$death_per_100k <- clean_covid$total_deaths / clean_covid$pop * 100000

#question 8 
mean(clean_covid$death_per_100k)
#this tells us that the average death per 100k people was 209.2908
mean_death <- median(clean_covid$death_per_100k)
#this tells us that out of all the death per 100k people the half way amount of 216.97 people. Which is pretty close to the average.
sd_death <- sd(clean_covid$death_per_100k)
#this tells us that out of all the death per 100k people that the data is dispersed 68.76 both ways from the mean. This is where most of the data lies.

#question 9 

clean_covid$death_z <- (clean_covid$death_per_100k - mean_death)/sd_death
#Hawaii (under) and Vermont(under) are the two states that are under 2 deviations. 