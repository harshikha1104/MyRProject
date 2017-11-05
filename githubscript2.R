filename= file.choose()
art1=read.csv(filename, header=T, stringsAsFactors = F)
par(mfrow = c(1,2))

#First Plot: Each paper (watercolor and drawing) has different subtypes. 
#For watercolor only, how are the total sales of the different paper types (column is paper.type) 
#similar or different for each store?
water= subset(art1, paper=="watercolor")
WSales <- tapply(water$total.sale, list(water$paper.type, water$store), FUN= sum)
fourthplot=barplot(WSales, col = c("antiquewhite1", "aquamarine2","red", "darkgoldenrod2" ), 
                   main = "Sales of different paper types of Watercolour",
                   xlab="Store", ylab="Sales", beside=T,
                   legend.text = rownames(WSales), args.legend = list(x= "topright", bty = "n", cex=0.75), 
                   ylim = c(0,16000))
text(fourthplot, 0, round(WSales, 0),cex=0.7,pos=3, srt=45) #srt=45 rotates the text by 45 degree

#Second Plot: Over the years, does the ratio of units sold for water color and drawing paper stay the same? 
#Is one growing while the other stays constant?
Ratio <- tapply(art1$units.sold,list(art1$paper, art1$year),FUN= sum)
SixthPlot=barplot(Ratio, beside = F, col = c("gray85", "chartreuse4"),
                  main = "Ratio of units sold for water color and drawing paper ", 
                  xlab="Year", ylab="Units sold",
                  legend.text = rownames(Ratio), args.legend = list(x= "topleft", bty = "n"), 
                  ylim = c(0,7000))
text(SixthPlot,Ratio[1,]-650,labels=Ratio[1,],cex=1, col="goldenrod1")
text(SixthPlot,colSums(Ratio)-650,labels=Ratio[2,],cex=1, col="goldenrod1")


