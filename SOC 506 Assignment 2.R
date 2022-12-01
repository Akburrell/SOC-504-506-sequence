data(highway)
summary(highway)
coef.vec <- c( 1.31, .93, 1.46, .07, .96, .2, .22, -.21, -.32, -.27,.23, 
               0, -.03, .13, .15, .31, -.10, .41)
se.vec <- c( .33, .32, .32, .37, .37, .13, .12, .12, .12, .07, .07, .01, .21,
             .14, .29, .25, .27, .93)
var.names <- c("Argentina", "Chile", "Colombia", "Mexico", "Venezuela", #for longer names, we split into 2 lines using "\n" function
               "Retrospective egocentric\neconomic perceptions", "Prospective egocentric\neconomic perceptions",
               "Retrospective sociotropic\neconomic perceptions", "Prospective sociotropic\neconomic perceptions",
               "Ideological Distance\nfrom president", "Ideology", "Age", "Female", "Education",
               "Academic sector", "Business sector", "Government sector", "Constant")

y.axis <- c(length(coef.vec):1)#create indicator for y.axis, descending so that R orders vars from top to bottom on y-axis

par(mar=c(2, 9, 2, 1))#set margins for plot, leaving lots of room on left-margin (2nd number in margin command) for variable names
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .6,#plot coefficients as points, turning off axes and labels. 
     xlim = c(-2.5,2.5), xaxs = "i", main = "") #set limits of x-axis so that they include mins and maxs of 
#coefficients + .95% confidence intervals and plot is symmetric; use "internal axes", and leave plot title empty
#the 3 lines below create horiztonal lines for 95% confidence intervals, and vertical ticks for 90% intervals
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd =  1.5)#coef +/-1.96*se = 95% interval, lwd adjusts line thickness
segments(coef.vec-qnorm(.95)*se.vec, y.axis -.1, coef.vec-qnorm(.95)*se.vec, y.axis +.1, lwd = 1.1)#coef +/-1.64*se = 90% interval
segments(coef.vec+qnorm(.95)*se.vec, y.axis -.1, coef.vec+qnorm(.95)*se.vec, y.axis +.1, lwd = 1.1)
axis(1, at = seq(-2.5,2.5,by=.5), labels =  seq(-2.5,2.5,by=.5), tick = T,#draw x-axis and labels with tick marks
     cex.axis = .8, mgp = c(2,.5,0))#reduce label size, moves labels closer to tick marks
axis(3, at = seq(-2.5,2.5,by=.5), labels =  seq(-2.5,2.5,by=.5), tick = T, las  = 1,#same as x-axis, but on top axis so 
     line =0, cex.axis = .8, mgp = c(2,.7,0))                                            #it's easier to lookup coefs at top of graph
axis(2, at = y.axis, label = var.names, las = 1, tick = T, 
     cex.axis = .8) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(v=0, lty = 2) # draw dotted line through 0
box(bty = "o") #place box around plot
text(1.2, 5, expression(R^{2} == .15), adj = 0, cex = .7) #add text for R-squared
text(1.2, 4, expression(paste("Adjusted ", R^{2} == .12, "")), adj = 0, cex = .7)#add text for Adjusted-R-squared
text(1.2, 3, "n = 500", adj = 0, cex = .7)#add text for sample size
segments(1.1, 2.7, 1.1,5.3)#use 3 segment commands to draw box around text; connect to  right-side of box 
segments(1.1, 2.7, 5,2.7)
segments(1.1, 5.3, 5.2,5.3)
summary(highway)
