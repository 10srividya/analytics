library(readxl)
library(ggplot2)

corruption = read_excel("C:\\Users/srividya/Downloads/dat1.xlsx")




ggplot(data=corruption, aes(x=CPI, y=HDI)) +geom_smooth(method="lm",se=FALSE, formula=y~log(x),color="red")+geom_point(aes(color=factor(Region)),stroke=1.5, shape=21, size=3)+theme_gdocs()





ggplot(data=corruption, aes(x=CPI, y=HDI)) +geom_point(shape=1,size=3,stroke=1.5) + geom_smooth(method="lm",se=FALSE, formula=y~log(x))

p3 <- ggplot(data=corruption, aes(x=CPI,y=HDI))+
  geom_point(aes(colour=Region), shape=21,size=3, stroke=1.5)+
  geom_smooth(method="lm",se=FALSE, formula= y ~ log(x))

p4<-ggplot(data=corruption, aes(x=CPI,y=HDI))+
  geom_point(aes(colour=Region), shape=21,size=3, stroke=1.5)+
  geom_smooth(method="lm",se=FALSE, formula= y ~ log(x))

 p4 <- p3 +scale_color_manual( values = c("darkblue",
                                     "blue",
                                     "dodgerblue",
                                     "darkcyan",
                                     "brown2",
                                     "darkred"))
 p4 + geom_text(aes(label=Country))

 pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                    "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                    "India", "Italy", "China", "South Africa", "Spane",
                    "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                    "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                    "New Zealand", "Singapore")

 ecodata.sub <- subset(corruption, Country %in% pointsToLabel)
 
 p4 + geom_text(data=ecodata.sub,aes(label=Country))
 require(ggrepel)
 p4 + geom_text_repel(data=ecodata.sub,aes(label=Country))
 
 