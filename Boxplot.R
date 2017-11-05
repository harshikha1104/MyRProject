filename= file.choose()
art1=read.csv(filename, header=T, stringsAsFactors = F)
View(art1)

par(mfrow = c(1,2))

#Question 1:
boxplot(art1$total.sale~art1$paper, main="Distributions of total sales for water color and drawing papers", 
        xlab="Paper", ylab="Total Sales", col=c("coral2", "chocolate1"),border="chartreuse4", outcol="cadetblue", lwd=2)

#Question 2:
options(scipen = 999)
art1$date1=as.Date(art1$date, "%m/%d/%Y")
str(art1)
art1=art1[,-1]
View(art1)
Sales=tapply(art1$total.sale, list(art1$store, art1$year), sum)
x=colnames(Sales)
plot(x, Sales[1,], type = "l", col="brown", lwd=3, ylab = "Total Sales", xlab = "years", ylim = c(5000,20000),
     bty = "n", lty= 4, main = "Total sales for each store over the years", 
     col.lab="navyblue", col.main="navyblue", cex.lab=1.4)
lines(x, Sales[2,], type="l", col = "darkcyan", lwd= 3, lty=4)
lines(x,Sales[3,], type="l", col = "chartreuse4", lwd= 3, lty=4)
lines(x,Sales[4,], type="l", col = "orange1", lwd= 3, lty=4)
legend(2012.01, 21000, legend = rownames(Sales), lwd = 2, lty=1, 
       col = c('brown','darkcyan','chartreuse4','orange1'),bty = 'n', cex = .75 )

#Question 3:
UnitsSold<- tapply(art1$units.sold, list(art1$paper, art1$store), FUN= sum)
View(UnitsSold)
thirdplot=barplot(UnitsSold,col = c("lightpink", "indianred4"), main = "Total units of water color and drawing paper sold based on store",
                  xlab="Store", ylab="Sales", beside=T, 
                  legend.text = rownames(UnitsSold), args.legend = list(x= "topleft", bty = "n"), 
                  ylim = c(0,6000))
text(x=thirdplot,y= UnitsSold, label= UnitsSold, pos= 3, col="green", cex= 0.8)

#Question 4:
water= subset(art1, paper=="watercolor")
WSales <- tapply(water$total.sale, list(water$paper.type, water$store), FUN= sum)
fourthplot=barplot(WSales, col = c("antiquewhite1", "aquamarine2","red", "darkgoldenrod2" ), 
                   main = "Sales of different paper types of Watercolour",
                   xlab="Store", ylab="Sales", beside=T,
                   legend.text = rownames(WSales), args.legend = list(x= "topright", bty = "n", cex=0.75), 
                   ylim = c(0,16000))
text(fourthplot, 0, round(WSales, 0),cex=0.7,pos=3, srt=45) #srt=45 rotates the text by 45 degree

#Question 5:
Dport= subset(art1, store=="Davenport")
AmountSold= tapply(Dport$units.sold, list(Dport$rep, Dport$paper), FUN=sum)
Fifthplot= barplot(AmountSold, beside = T, col = c("burlywood1","mediumvioletred"), main = "Units of paper sold in Davenport by the representatives",
                   xlab="Representative", ylab="Total sales",legend.text = rownames(AmountSold), args.legend = list(x= "topright", bty = "n", cex=0.65), ylim = c(0,1200))
text(Fifthplot, 0, round(AmountSold, 1),cex=1.2,pos=3, col="yellowgreen") 

#Question 6:
Ratio <- tapply(art1$units.sold,list(art1$paper, art1$year),FUN= sum)
SixthPlot=barplot(Ratio, beside = F, col = c("gray85", "chartreuse4"),
                  main = "Ratio of units sold for water color and drawing paper ", 
                  xlab="Year", ylab="Units sold",
                  legend.text = rownames(Ratio), args.legend = list(x= "topleft", bty = "n"), 
                  ylim = c(0,7000))
text(SixthPlot,Ratio[1,]-650,labels=Ratio[1,],cex=1, col="goldenrod1")
text(SixthPlot,colSums(Ratio)-650,labels=Ratio[2,],cex=1, col="goldenrod1")


