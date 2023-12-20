## r code for multiplot of the four models for paper1 
#library(ggplot2)
setwd("C:\\Users\\Demiss\\Documents\\Project\\Chapter1\\Graph")
Graphdata <- read.table(file.choose(), header=TRUE, sep="", quote="", stringsAsFactors = FALSE)
#Graphdata<-read.table(file.choose(), header=T, sep="", quote="", stringsAsFactors = FALSE, fill = TRUE, na.strings = "")
#Graphdata<-as.data.frame(multiplot)
#library(Cairo)
head(Graphdata)
#head(multiplot)
par(oma=c(2,2,0,0), mar=c(3,3,3,2), mfrow=c(1,2), mgp = c(2.5, 0.5, 0))
#par(mfrow=c(2,2))

SiteA <- subset(Graphdata, Sites == "ALF")
plot(SiteA$f,xlim=c(1,17),ylim=c(72,177.5),xlab="Number of years of training set to fit the model", ylab="Mean Prediction Error", lwd=2, asp = NA, type = "n")
#plot(SiteA$f,xlim=c(1,18),ylim=c(72,177.5),xlab="", ylab="", lwd=2, asp = NA, type = "n")

lines(SiteA$f, SiteA$Gompertz, type="l", col="blue",lwd=2)
#arrows(x0=SiteA$f, y0=SiteA$Gompertz -SiteA$GomSE,x1=SiteA$f, y1=SiteA$Gompertz + SiteA$GomSE,code=3, angle=90, length=0.1)
#arrows(x0=17, y0=72.10812134,x1=17, y1=77.83380099, code = 3, angle=90, length=0.1, lwd=2)
lines(SiteA$f, SiteA$Logistic, type="l", col="red",lwd=2)
#arrows(x0=SiteA$f, y0=SiteA$Logistics -SiteA$LogisticsSE,x1=SiteA$f, y1=SiteA$Logistics + SiteA$LogisticsSE,code=3, angle=90, length=0.1)
#arrows(x0=17, y0=75.74711008, x1=17, y1=82.09743915, code = 3,angle=90, length=0.1, lwd=2)
lines(SiteA$f, SiteA$Mean, type="l", col="black",lwd=2)
#arrows(x0=SiteA$f, y0=SiteA$Mean -SiteA$MeanSE,x1=SiteA$f, y1=SiteA$Mean + SiteA$MeanSE,code=3, angle=90, length=0.1)
#arrows(x0=5, y0=78.16465902, x1=5, y1=83.77207652, code = 3,angle=90, length=0.1, lwd=2)
#lines(SiteA$f, SiteA$SSM, type="l", col="magenta",lwd=2)
lines(SiteA$f, SiteA$Trend, type="l", col="green",lwd=2)
#arrows(x0=SiteA$f, y0=SiteA$Trend -SiteA$TrendSE,x1=SiteA$f, y1=SiteA$Trend + SiteA$TrendSE,code=3, angle=90, length=0.1)
#arrows(x0=17, y0= 95.83707919,x1=17, y1=103.6132471,code = 3,angle=90, length=0.1, lwd=2)
#text(17, 75.12124352, "PE = c(17, 75.12124352)", PE = c(17, 75.12124352), col = "red", font = 2 )
text(17, 74.97096117, "x", col = "blue")
mtext("Aleutian Islands Ground Fish Datasets")
# text(x = 15, y = 140, paste("TS = 968"), 
#    cex = 1.5, col = "black", family="Arial", font=0.3, adj=0.5)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteB <- subset(Graphdata, Sites == "Bakker")

plot(SiteB$f, xlim = c(1,18), ylim=c(4,10),xlab="",ylab="",lwd=2,type = "n", asp = NA)
lines(SiteB$f, SiteB$Gompertz,type="l", col="blue",lwd=2)
arrows(x0=17, y0=4.381822492,x1=17, y1=5.081182434, code = 3, angle=90, length=0.1, lwd=2)

# lines(SiteB$f,SiteB$ARIMA,type="l", col="blue",lwd=2)
# lines(SiteB$f,SiteB$EDM, type="l", col="purple",lwd=2)
lines(SiteB$f,SiteB$Logistics,type="l",col="red",lwd=2)
arrows(x0=17, y0=4.315977799,x1=17, y1=4.996649872, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteB$f,SiteB$Mean,type="l", col="black",lwd=2)
arrows(x0=1, y0=4.266310713,x1=1, y1=4.990099544, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteB$f,SiteB$SSM, type="l", col="magenta",lwd=2)
lines(SiteB$f,SiteB$Trend,type="l",col="green",lwd=2)
arrows(x0=17, y0 = 4.858153672, x1=17, y1=5.696095971, code = 3, angle=90, length=0.1,lwd=2 )

#text(1, 4.541025641, "(1, 4.541025641)", "(1, 4.541025641)", col = "red", font = 1 )
text(1, 4.628205128, "x", col = "black")
mtext("Bakker Plant Datasets")
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line = 0)
mtext(text = "Number of years of training set to fit the model", side = 1, outer = TRUE, line = 0)
# text(x = 14, y = 10, paste("TS = 40"), 
#      cex = 1.5, col = "black", family="Arial", font=0.5, adj=0.5)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)
# legend(locator(1),
#     legend=c("Logistic","Gompertz","Mean","Trend"),
#     col=c("red","orange","black","green"),lty=0,lwd=2)

SiteC <- subset(Graphdata, Sites=="BBSALL")
plot(SiteC$f, xlim = c(1,16),ylim=c(8,12),xlab="",ylab="",lwd=2,type = "n", asp = NA)
lines(SiteC$f,SiteC$Gompertz, type="l",col="blue",lwd=2)
arrows(x0=13, y0=8.320163709,x1=13, y1=8.801885002, code = 3, angle=90, length=0.1, lwd=2)
# lines(SiteC$f,SiteC$ARIMA, type ="l",col="blue",lwd=2)
# lines(SiteC$f,SiteC$EDM, type="l", col="purple",lwd=2)
lines(SiteC$f,SiteC$Logistics, type="l",col="red",lwd=2)
arrows(x0=15, y0=8.556669483,x1=15, y1=9.045425234, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteC$f,SiteC$Mean, type="l", col="black",lwd=2)
arrows(x0=3, y0=8.239118646,x1=3, y1=8.737189894, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteC$f,SiteC$SSM, type="l", col="magenta",lwd=2)
lines(SiteC$f,SiteC$Trend,type="l", col="green",lwd=2)
arrows(x0=15, y0=9.737613091, x1=15, y1=10.37437706, code = 3, angle=90, length=0.1, lwd=2)

text(3, 8.48815427, "x", col = "black")
# text(x = 14, y = 10, paste("TS = 78"), 
#      cex = 1.5, col = "black", family="Arial", font=0.5, adj=0.5)

mtext("North America Bird Survey (50 stops) Datasets")
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteD <-subset(Graphdata, Sites=="BBSTOP")

plot(SiteD$f,xlim = c(1,16), ylim=c(3.5,5.5),xlab="",ylab="",lwd=2, type="n")
lines(SiteD$f,SiteD$Gompertz, type="l", col="blue",lwd=2)
arrows(x0=11, y0=3.836169235,x1=11, y1=4.20186903, code = 3, angle=90, length=0.1, lwd=2)

# lines(SiteD$f,SiteD$ARIMA, type="l", col="blue",lwd=2)
# lines(SiteD$f,SiteD$EDM, type="l", col="purple",lwd=2)
lines(SiteD$f,SiteD$Logistics, type="l", col="red",lwd=2)
arrows(x0=15, y0=3.935581569,x1=15, y1=4.25965396, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteD$f,SiteD$Mean, type="l", col="black",lwd=2)
arrows(x0=5, y0=3.68925126,x1=5, y1=4.006042858, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteD$f,SiteD$SSM, type="l", col="magenta",lwd=2)
lines(SiteD$f,SiteD$Trend,type="l",col="green",lwd=2)
arrows(x0=15, y0=4.838345349,x1=15, y1=5.319213866, code = 3, angle=90, length=0.1, lwd=2)

text(5, 3.847647059, "x", col = "black")
mtext("North America Bird Survey (10 stops) Datasets")
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line = 0)
mtext(text = "Number of years of training set to fit the model", side = 1, outer = TRUE, line = 0)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)
#This code gives the graph on the next page
#par(oma=c(2,2,0,4),mar=c(3,3,2,0),mfrow=c(2,2))

SiteG <-subset(Graphdata,Sites=="BSS")

plot(SiteG$f,xlim = c(1,12), ylim=c(1,2.9),xlab="",ylab="",lwd=2, type = "n")
lines(SiteG$f,SiteG$Gompertz, type="l",col="blue",lwd=2)
arrows(x0=11, y0 = 1.339959697,x1=11, y1=1.700539311, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteG$f,SiteG$ARIMA, type="l",col="blue",lwd=2)
lines(SiteG$f,SiteG$Logistics, type="l",col="red",lwd=2)
arrows(x0=9, y0=1.436074212, x1=9, y1=1.817285148, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteG$f,SiteG$Mean, type="l", col="black",lwd=2)
arrows(x0=1, y0=1.31162913,x1=1, y1=1.656519697, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteG$f,SiteG$Trend, type="l",col="green",lwd=2)
arrows(x0=7, y0=1.572619327,x1=7, y1=2.049387392, code = 3, angle=90, length=0.1, lwd=2)

text(1, 1.484744807, "x", col = "black")
mtext("Buell-Small Succession Plant Datasets")
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteE <- subset(Graphdata,Sites=="DBBS")

plot(SiteE$f,xlim = c(1,16),ylim=c(3.4,5.5),xlab="",ylab="",lwd=2, type="n")
lines(SiteE$f,SiteE$Gompertz,type="l",col="blue",lwd=2)
arrows(x0=15, y0=3.634708601,x1=15, y1=3.818959658, code = 3, angle=90, length=0.1, lwd=2)

# lines(SiteE$f,SiteE$ARIMA,type="l",col="blue",lwd=2)
# lines(SiteE$f,SiteE$EDM, type="l", col="purple",lwd=2)
lines(SiteE$f,SiteE$Logistics,type="l",col="red",lwd=2)
arrows(x0=15, y0=3.725908703,x1=15, y1=3.914033867, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteE$f,SiteE$Mean,type="l",col="black",lwd=2)
arrows(x0=1, y0=3.438088058,x1=1, y1=3.613151611, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteE$f,SiteE$SSM, type="l", col="magenta",lwd=2)
lines(SiteE$f,SiteE$Trend,type="l",col="green",lwd=2)
arrows(x0=13, y0=3.659957121, x1=13, y1=3.84417132, code = 3, angle=90, length=0.1, lwd=2)

text(1, 3.525619835, "x", col = "black")
# text(x = 5, y = 5, paste("TS = 726"), 
#      cex = 1.5, col = "black", family="Arial", font=0.5, adj=0.5)

mtext("Dutch Birds Survey Datasets")
legend("topleft", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line = 0)
mtext(text = "Number of years of training set to fit the model", side = 1, outer = TRUE, line = 0)

SiteF <- subset(Graphdata,Sites=="Hay")

plot(SiteF$f,xlim = c(1,22),ylim=c(19.5,31),xlab="",ylab="",lwd=2, type = "n")
lines(SiteF$f,SiteF$Gompertz, type="l",col="blue",lwd=2)
arrows(x0=9, y0=20.32766499,x1=9, y1=23.30048386, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteF$f,SiteF$ARIMA, type="l",col="blue",lwd=2)
lines(SiteF$f,SiteF$Logistics, type="l",col="red",lwd=2)
arrows(x0=17, y0=19.59240015,x1=17, y1=22.44281018, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteF$f,SiteF$Mean, type="l",col="black",lwd=2)
arrows(x0=19, y0=19.71430009,x1=19, y1=22.29651072, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteF$f,SiteF$Trend, type="l",col="green",lwd=2)
arrows(x0=21, y0=24.56455282, x1=21, y1=28.18627992, code = 3, angle=90, length=0.1, lwd=2)

text(19, 21.00540541, "x", col = "black")

# text(x = 5, y = 33, paste("TS = 37"), 
#      cex = 1.5, col = "black", family="Arial", font=0.5, adj=0.5)

mtext("Hay Plant Datasets")
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line = 0)
mtext(text = "Number of years of training set to fit the model", side = 1, outer = TRUE, line = 0)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)


SiteH <- subset(Graphdata,Sites=="Portal")

plot(SiteH$f,xlim = c(1,22),ylim=c(7.5,16),xlab="",ylab="",lwd=2,type = "n")
lines(SiteH$f,SiteH$Gompertz, type="l",col="blue",lwd=2)
arrows(x0=17, y0=7.565334858,x1=17, y1=9.75848186, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteH$f,SiteH$ARIMA, type="l",col="blue",lwd=2)
lines(SiteH$f,SiteH$Logistics, type="l",col="red",lwd=2)
arrows(x0=17, y0=7.686209676,x1=17, y1=9.899425758, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteH$f,SiteH$Mean,type="l", col="black",lwd=2)
arrows(x0=9, y0=8.827390532,x1=9, y1=11.76654886, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteH$f,SiteH$Trend,type="l",col="green",lwd=2)
arrows(x0=17, y0=10.00152725,x1=17, y1=12.9990936, code = 3, angle=90, length=0.1, lwd=2)

text(17, 8.661908359, "x", col = "blue")
mtext("Portal Rodent Datasets")
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line =0)
mtext("Number of years of training set to fit the model", side = 1, outer = TRUE, line = 0)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteI<-subset(Graphdata,Sites=="PUSmall")

plot(SiteI$f,xlim = c(1,14),ylim=c(1.3,3.3),xlab="",ylab="",lwd=2,type = "n")
lines(SiteI$f, SiteI$Gompertz, type = "l", col = "blue",lwd=2)
#arrows(x0=13, y0=1.532747244, x1=13, y1= 1.915698088, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteI$f, SiteI$ARIMA,type="l", col = "blue",lwd=2)
lines(SiteI$f, SiteI$Logistics, type = "l", col = "red",lwd=2)
arrows(x0=13, y0=1.829742477,x1=13, y1=2.44926108, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteI$f, SiteI$Mean, type ="l", col = "black",lwd=2)
arrows(x0=1, y0=1.353658754, x1=1, y1=1.672397743, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteI$f, SiteI$Trend,type ="l", col ="green",lwd=2)
arrows(x0=13, y0=1.853713521,x1=13, y1=2.351522847, code = 3, angle=90, length=0.1, lwd=2)

text(1, 1.513028249, "x", col = "black")

mtext("Pumice Plot Plant Datasets")
legend("topleft", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteJ <- subset(Graphdata, Sites == "PUSquare")

plot(SiteJ$f,xlim = c(1,14),ylim=c(0.8,10.6),xlab="", ylab="",lwd=2,type = "n")
lines(SiteJ$f, SiteJ$Gompertz, type="l", col = "blue",lwd=2)
arrows(x0=9, y0=0.936948546, x1= 9, y1=1.208901633, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteJ$f, SiteJ$ARIMA, type="l", col = "blue",lwd=2)
lines(SiteJ$f, SiteJ$Logistics, type="l", col = "red",lwd=2)
arrows(x0=13, y0=0.994934921, x1=13, y1=1.278431593, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteJ$f, SiteJ$Mean, type = "l", col = "black", lwd=2)
arrows(x0=1, y0=0.855175824,x1=1, y1=1.113902674, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteJ$f, SiteJ$Trend, type = "l", col = "green", lwd=2)
arrows(x0=13, y0=1.136285136,x1=13, y1=1.534591495, code = 3, angle=90, length=0.1, lwd=2)

text(1, 0.9845392497, "x", col = "black")
mtext("Pumice Square Plant Datasets")
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line =0)
mtext("Number of years of training set to fit the model", side = 1, outer = TRUE, line = 0)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteK <- subset(Graphdata,Sites == "PUYard")

plot(SiteK$f,xlim = c(1,14),ylim=c(0.5,2.5),xlab="",ylab="",lwd=2,type = "n")
lines(SiteK$f,SiteK$Gompertz, type="l", col = "blue",lwd=2)
arrows(x0=7, y0=1.003224515,x1=7, y1=1.421869773, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteK$f,SiteK$ARIMA, type="l", col = "blue",lwd=2)
lines(SiteK$f,SiteK$Logistics, type="l", col = "red",lwd=2)
arrows(x0=7, y0=0.960752346,x1=7, y1=1.354065154, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteK$f,SiteK$Mean, type="l", col = "black",lwd=2)
arrows(x0=9, y0=0.923382159,x1=9, y1=1.328105215, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteK$f,SiteK$Trend, type="l", col = "green",lwd=2)
arrows(x0=11, y0=1.162521266,x1=11, y1=1.843666538, code = 3, angle=90, length=0.1, lwd=2)

text(9, 1.125743687, "x", col = "black")
mtext("Pumice Yard Plant Datasets")
legend("topleft", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteL <- subset(Graphdata,Sites=="Rothamsted")

plot(SiteL$f,xlim = c(1,22),ylim = c(30,65), xlab = "",ylab="",lwd=2,type = "n")
lines(SiteL$f,SiteL$Gompertz, type = "l", col = "blue",lwd=2)
arrows(x0=17, y0=30.52865807,x1=17, y1=34.0485485, code = 3, angle=90, length=0.1,lwd=2 )

#lines(SiteL$f,SiteL$ARIMA, type = "l", col = "blue",lwd=2)
lines(SiteL$f,SiteL$Logistics, type = "l", col = "red",lwd=2)
arrows(x0=21, y0=31.54910752,x1=21, y1=35.27697576, code = 3, angle=90, length=0.1,lwd=2)

lines(SiteL$f,SiteL$Mean, type = "l", col = "black",lwd=2)
arrows(x0=1, y0=33.12131472,x1=1, y1=37.19229475, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteL$f,SiteL$Trend, type = "l", col = "green",lwd=2)
arrows(x0=21, y0=34.58561249,x1=21, y1=38.85704168, code = 3, angle=90, length=0.1, lwd=2)

text(17, 32.28860328, "x", col = "blue")
mtext("Rothamsted Moth Datasets")
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line = 0)
mtext(text = "Number of years of training set to fit the model", side = 1, outer = TRUE, line =0)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteN <-subset(Graphdata,Sites=="SCZ")

plot(SiteN$f,xlim = c(1,22),ylim=c(715,1185),xlab="",ylab="",lwd=2,type="n")
lines(SiteN$f,SiteN$Gompertz, type = "l", col = "blue",lwd=2)
arrows(x0=17, y0=715.2100317,x1=17, y1=892.7046955, code = 3, angle=90, length=0.1, lwd=2,col = "blue")

#lines(SiteN$f,SiteN$ARIMA, type="l", col ="blue",lwd=2)
lines(SiteN$f,SiteN$Logistics, type = "l", col = "red",lwd=2)
arrows(x0=15, y0=755.4842113,x1=15, y1=936.3463701, code = 3, angle=90, length=0.1, lwd=2, col = "red")

lines(SiteN$f,SiteN$Mean, type = "l", col = "black",lwd=2)
arrows(x0=7, y0=756.1231698,x1 = 7, y1=941.3816816, code = 3, angle=90, length=0.1, lwd=2, col = "black")

lines(SiteN$f,SiteN$Trend, type = "l", col = "green",lwd=2)
arrows(x0=21, y0=936.7479485, x1=21, y1=1184.793113, code = 3, angle=90, length=0.1, lwd=2, col = "green")

text(17, 803.9573917, "x", col = "blue")
mtext("South Central Ontario Lakes Zooplankton Datasets")
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line = 0)
mtext(text = "Number of years of training set to fit the model", side = 1, outer = TRUE, line =0)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

# mtext(text = "Mean Prediction Error", side = 2)
# mtext(text = "Different training datapoints to fit the model", side = 1)


SiteM <- subset(Graphdata,Sites=="SNF")

plot(SiteM$f,xlim = c(1,20), ylim = c(111.5,284), xlab ="",ylab="",lwd=2,type = "n")
lines(SiteM$f,SiteM$Gompertz, type ="l", col ="blue",lwd=2)
arrows(x0= 7, y0=139.3670673,x1= 7, y1=238.541363, code = 3, angle=90, length=0.1, lwd=2,col = "blue")

#lines(SiteM$f, SiteM$ARIMA, type = "l", col="blue",lwd=2)
lines(SiteM$f,SiteM$Logistics, type = "l", col = "red",lwd=2)
arrows(x0=17, y0=142.5667669,x1=17, y1=247.23135, code = 3, angle=90, length=0.1, lwd=2, col = "red")

lines(SiteM$f,SiteM$Mean, type="l", col="black",lwd=2)
arrows(x0=5, y0=111.972378,x1=5, y1=184.9928394, code = 3, angle=90, length=0.1, lwd=2, col = "black")

lines(SiteM$f,SiteM$Trend, type="l", col="green",lwd=2)
arrows(x0=13, y0=163.5524523,x1=13, y1=283.4070222, code = 3, angle=90, length=0.1, lwd=2, col = "green")

text(5, 148.4826087, "x", col = "black")
mtext("San Nicolas Island Fish (Kelp sites)")
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

SiteN <-subset(Graphdata,Sites=="SCZ")

plot(SiteN$f,xlim = c(1,22),ylim=c(803,1145),xlab="",ylab="",lwd=2,type="n")
lines(SiteN$f,SiteN$Gompertz, type = "l", col = "blue",lwd=2)
arrows(x0=17, y0=72.10812134,x1=17, y1=77.83380099, code = 3, angle=90, length=0.1, lwd=2)

#lines(SiteN$f,SiteN$ARIMA, type="l", col ="blue",lwd=2)
lines(SiteN$f,SiteN$Logistic, type = "l", col = "red",lwd=2)
arrows(x0=17, y0=72.10812134,x1=17, y1=77.83380099, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteN$f,SiteN$Mean, type = "l", col = "black",lwd=2)
arrows(x0=17, y0=72.10812134,x1=17, y1=77.83380099, code = 3, angle=90, length=0.1, lwd=2)

lines(SiteN$f,SiteN$Trend, type = "l", col = "green",lwd=2)
arrows(x0=17, y0=72.10812134,x1=17, y1=77.83380099, code = 3, angle=90, length=0.1, lwd=2)

text(17, 803.9573917, "x", col = "blue")
mtext("Sudbury Zooplankton Datasets")
mtext(text = "Mean Prediction Error", side = 2, outer = TRUE, line = 0)
mtext(text = "Number of years of training set to fit the model", side = 1, outer = TRUE, line =0)
legend("topright", col=c( "blue","red", "black", "green"), lwd = 2, 
       legend = c("Gompertz", "Logistic","Mean", "Trend"), bty = 'n', cex = 0.6)

# mtext(text = "Mean Prediction Error", side = 2)
# mtext(text = "Different training datapoints to fit the model", side = 1)

legend(locator(1),
       legend=c("Logistic","Gompertz","Mean","Trend"),
       col=c("blue","green","red","orange","pink"),
       lty=0,lwd=2)
legend("bottomright",
       legend=c("ARIMA","Logistics","Gompertz","Mean","Trend"),
       col=c("blue","green","red","orange","pink"),
       lty=0,lwd=2)
legend(-0.2, 0.3, legend = c("ARIMA","Logistics","Gompertz","Mean","Trend"),
       col=c("blue","green","red","orange","pink"),
       bty = "n", cex = 1, pch = c(10, 15, 1))
#Legend outside the graph
legend("right", inset = c(-0.17,0), legend = 1:12, xpd = TRUE, 
       horiz = FALSE, col = rainbow(12), lty = 1, bty = "n")

#using ggplot
data.df <- read.table(text="
   Year  Type Area
     1  2011 corn  30
    2   2012 corn  15
    3   2013 corn  50
    4   2011 Soy  45
    5  2012 Soy  30
    6 2013 Soy  60",
                      header = TRUE)

ggplot(data=data.df, aes(x=as.factor(Year), y=Area, group=Type, color=Type)) + geom_line() +
  xlab("Year") + ylab("Area (ha)") + theme_bw() + scale_color_manual(values=c("red", "blue"))
#using ggplot based on the sample given above
ggplot(data=Graphdata, aes(x=as.factor(f),y=))

## To put the legend outside the plot, there are two options:
legend(-0.2, 0.3, legend = c("apple", "orange", "tree"),
       bty = "n", xpd=TRUE, mar=c(7,7,7,7), cex = 1, pch = c(10, 15, 1))

legend("bottomleft", legend = c("apple", "orange", "tree"), inset=c(-0.15,0),
       bty = "n", xpd=TRUE, mar(c(7,7,7,7)), cex = 1, pch = c(10, 15, 1))
#Removing the grids in ggplot
library(ggplot2)
a <- seq(1,20)
b <- a^0.25
df <- as.data.frame(cbind(a,b))

#base ggplot object
p <- ggplot(df, aes(x = a, y = b))

p +
  #plots the points
  geom_point() +
  
  #theme with white background
  theme_bw() +
  
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
#########################################################################################
myplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#########################################################################################