library(borsdata)
library(stringr)
library(dplyr)

# hämtar info genom borsdata
year<-fetch_year(221,key = key)
r12<-fetch_r12(221,key = key)
kvartal<-fetch_quarter(221,key = key)



# städar upp datan
for(i in 1:33){
  year[,i]<-rev(year[,i])
  r12[,i]<-rev(r12[,i])
  kvartal[,i]<-rev(kvartal[,i])
  
}

# lägger in 2020 Q3 som ett eget år
year[11,]<-r12[10,]


library(ggplot2)

skr<-data.frame(year=year$year,revenue=year$revenues,
                ebit=year$profit_Before_Tax,
                ebit_marg=year$profit_Before_Tax/year$revenues)

cagr<-(year$revenues[11]/year$revenues[1])^(1/length(year$year))-1
plot_labels <- data.frame(label = round(cagr,digits = 3)*100,
                          x = seq(2010.5,2020.5,1),
                          y = year$revenues[1:11]*1.3/1000)

ggplot(data =  skr, aes(x=year))+ 
  geom_bar(stat = "identity", 
           fill = "dark green",   
           colour = "black", aes(y=skr$revenue/1000)) + theme_bw()+ labs(
             x = "År",
             y = "Omsättning\n MDR",
             title = "Systemair omsättning och rörelsemarginal 2010-2020e",
             caption = "Källa: Börsdata"
           )+ 
  theme(
    plot.title = element_text(hjust = 0.5)
  )+ theme(
    panel.grid.major.x =
      element_blank(),
    panel.grid.minor.x =
      element_blank(),
    panel.grid.major.y =
      element_line(color = "grey")
  )+
  scale_x_continuous(breaks=seq(2010,2020,1), labels = c(seq(2010,2019,1),"2020e"))+ 
  geom_text(data = plot_labels,
            aes(x = 2016, y = 9.1,
                label = paste("cagr:",label)), size=5, parse = TRUE)+
  annotate("segment", x = 2010, xend = plot_labels$x[10],
           y = year$revenues[2]*1.1/1000,
           yend = year$revenues[10]*1.2/1000,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50")+
  geom_line(aes(y=round(ebit_marg, digits=3)*100)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1),
                     sec.axis = sec_axis(~.,
                                         name = "Rörelsemarginal [%]",
                                         labels = seq(0,10,1),
                                         breaks = seq(0,10,1)))+
  geom_text(aes(x = year, y = revenue/1000,
                label=round(skr$ebit/skr$revenue, digits=3)*100),
            vjust=1.5, color="white", size=3.5)

