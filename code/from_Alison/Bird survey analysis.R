
Birds<-read.csv("NCOS_Monthly_Bird_Surveys_COMPILED_Rexport.csv")
str(Birds)
Birds$Observation.Date<-as.Date(Birds$Observation.Date, format="%m/%d/%Y")
Birds$Month <- format(as.Date(Birds$Observation.Date), "%B")
unique(birdsyear4$Month)
a<-aggregate(Count~Observation.Date+Slough.Water.Elevation..ft.+Survey.Year, Birds, FUN=sum)
a
a$Observation.Date<-as.Date(a$Observation.Date, format="%m/%d/%Y")
a$Month <- format(as.Date(a$Observation.Date), "%B")
a$Year <- format(as.Date(a$Observation.Date), "%Y")
a$Month <- factor(a$Month,levels = c("September", "October", "November", "December","January","February","March","April","May","June","July","August"))

## Figure of water leveel for each survey year over time
ggplot(a, aes(Month, Slough.Water.Elevation..ft.,color=Survey.Year, group=Survey.Year))+geom_point(size=4)+theme_bw()+geom_line(size=2)+ylab("Slough Water Elevation (feet)")+
  ggtitle("Monthly Water elevation at Upper Devereux Slough for the Bird Monitoring Period")+
  theme(axis.text.x=element_text(hjust=1,angle=45),
        plot.title= element_text(size=22, face="bold"),
        axis.title= element_text(size=18, face="bold"),
        axis.text= element_text(size=18),
        legend.title= element_text(size=18,face = "bold"),
        legend.text= element_text(size=17),
        legend.position = c(.9,.9))


#+ggtitle("Tree Size")+
  scale_size(range=c(2.7,8))+xlim(0.2,7)+ylim(50,300)+ylab("Height (Inches)")+xlab("Diameter at Breast Height")+ geom_line()
  theme(axis.text.x=element_text(hjust = 2),
        plot.title= element_text(size=22, face="bold"),
        axis.title= element_text(size=18, face="bold"),
        axis.text= element_text(size=18),
        legend.title= element_text(size=18,face = "bold"),
        legend.text= element_text(size=17))

birdsyear4<-subset(Birds,Birds$Survey.Year=="Survey Year 4")
count<-aggregate(Count~Observation.Date,birdsyear4, FUN=sum)
count
quant<-aggregate(Count~Month+Survey.Year, Birds,FUN=sum)
quant2<-aggregate(Count~Survey.Year, quant, FUN=mean)
quant2
