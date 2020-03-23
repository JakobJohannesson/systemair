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
                ebit=year$operating_Income,
                ebit_marg=year$operating_Income/year$revenues)

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
             title = "Systemair omsättning och rörelsemarginal 2010-2020(R12)",
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
      element_line(color = "grey"),
    axis.title.y = element_text(face = "bold",size=14),
    axis.title.x = element_text(face = "bold",size=14)
  )+
  scale_x_continuous(breaks=seq(2010,2020,1), labels = c(seq(2010,2019,1),"2020(R12)"))+ 
  geom_text(data = plot_labels,
            aes(x = 2016, y = 8.1,
                label = paste("cagr:",label)), size=5, parse = TRUE, angle=17)+
  annotate("segment", x = 2010, xend = plot_labels$x[10],
           y = year$revenues[2]*1.1/1000,
           yend = year$revenues[10]*1.2/1000,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50")+
  geom_line(aes(y=round(skr$ebit_marg, digits=3)*100),colour="red", size=1.5) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,2),
                     sec.axis = sec_axis(~.,
                                         name = "Rörelsemarginal [%]",
                                         labels=seq(0,12,2),
                                         breaks = seq(0,12,2)))+
  geom_hline(aes(yintercept=10), colour="red", linetype="dashed", size=1)+
  geom_text(x = 2013, y = 10.5,
                label = "Lönsamhetsmål", color="red", size=5, parse = TRUE)

#### Kvartal #### 

skr<-read.csv("data_long.csv") # Använder en egen fil från Systemairs IR.

# Ändrar så att det står vilken period det gäller
skr$period<-str_replace_all(string = skr$period,pattern = "1","Q1 (maj-jul)")
skr$period<-str_replace_all(string = skr$period,pattern = "2","Q2 (aug-okt)")
skr$period<-str_replace_all(string = skr$period,pattern = "3","Q3 (nov-jan)")
skr$period<-str_replace_all(string = skr$period,pattern = "4","Q4 (feb-apr)")


library(tidyr)
ggplot(data =  skr, aes(year, group=period))+ 
  geom_bar(stat = "identity", 
           fill = "dark green", 
           position = "dodge",   
           colour = "black", aes(y=skr$revenue))+
  geom_point(y=round(skr$ebit_marg, digits=3)*200,colour="red", size=3)+
  theme_bw()+ 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(face = "bold",size=14),
    axis.title.x = element_text(face = "bold",size=14)
  )+
  scale_y_continuous(limits = c(0,2500), breaks = seq(0,2500,500),
                     sec.axis = sec_axis(~./200,
                                         name = "Rörelsemarginal [%]",
                                         labels=seq(0,12.5,2.5),
                                         breaks = seq(0,12.5,2.5)))+
  labs( x = "År",
        y = "Omsättning\n MKR",
        title = "Systemair omsättning & rörelsemarginal per kvartal",
        caption = "Källa: Systemair IR")+
  facet_grid(~period)


#### Lindab ####

# hämtar info genom borsdata
year<-fetch_year(127,key = key)
r12<-fetch_r12(127,key = key)
kvartal<-fetch_quarter(127,key = key)



# städar upp datan
for(i in 1:33){
  year[,i]<-rev(year[,i])
  r12[,i]<-rev(r12[,i])
  kvartal[,i]<-rev(kvartal[,i])
  
}

# lägger in 2020 Q3 som ett eget år
year[11,]<-r12[10,]




skr<-data.frame(year=year$year,revenue=year$revenues,
                ebit=year$operating_Income,
                ebit_marg=year$operating_Income/year$revenues)

cagr<-(year$revenues[11]/year$revenues[1])^(1/length(year$year))-1

plot_labels <- data.frame(label = round(cagr,digits = 3)*100,
                          x = seq(2009.5,2019.5,1),
                          y = year$revenues[1:11]*1.3/1000)

ggplot(data =  skr, aes(x=year))+ 
  geom_bar(stat = "identity", 
           fill = "dark green",   
           colour = "black", aes(y=skr$revenue/1000)) + theme_bw()+ labs(
             x = "År",
             y = "Omsättning\n MDR",
             title = "Lindab omsättning och rörelsemarginal 2009-2019",
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
      element_line(color = "grey"),
    axis.title.y = element_text(face = "bold",size=14),
    axis.title.x = element_text(face = "bold",size=14)
  )+
  scale_x_continuous(breaks=seq(2009,2019,1), labels = seq(2009,2019,1))+ 
  geom_text(data = plot_labels,
            aes(x = 2013, y = 8.7,
                label = paste("cagr:",label)), size=5, parse = TRUE, angle=12)+
  annotate("segment", x = 2010, xend = plot_labels$x[10],
           y = year$revenues[2]*1.1/1000,
           yend = year$revenues[10]*1.13/1000,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50")+
  geom_line(aes(y=round(skr$ebit_marg, digits=3)*100),colour="red", size=1.5) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,2),
                     sec.axis = sec_axis(~.,
                                         name = "Rörelsemarginal [%]",
                                         labels=seq(0,12,2),
                                         breaks = seq(0,12,2)))+
  geom_hline(aes(yintercept=10), colour="red", linetype="dashed")+
  geom_text(x = 2013, y = 10.5,
            label = "Lönsamhetsmål", color="red", size=5, parse = TRUE)

### Kvartalsdata Lindab




skr<-data.frame(year=kvartal$year,period=kvartal$period,
                 revenue=kvartal$revenues,
                 ebit=kvartal$operating_Income,
                 ebit_marg=round(kvartal$operating_Income/kvartal$revenues*100,digits = 1))


skr$period<-str_replace_all(string = skr$period,pattern = "1","Q1 (jan-mar)")
skr$period<-str_replace_all(string = skr$period,pattern = "2","Q2 (apr-jun)")
skr$period<-str_replace_all(string = skr$period,pattern = "3","Q3 (jul-sep)")
skr$period<-str_replace_all(string = skr$period,pattern = "4","Q4 (okt-dec)")

library(tidyr)
ggplot(data =  skr, aes(year, group=period))+ 
  geom_bar(stat = "identity", 
           fill = "dark green", 
           position = "dodge",   
           colour = "black", aes(y=skr$revenue))+
  geom_point(y=round(skr$ebit_marg, digits=3)*250,colour="red", size=3)+
  theme_bw()+ 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(face = "bold",size=14),
    axis.title.x = element_text(face = "bold",size=14)
  )+
  scale_y_continuous(limits = c(0,3000), breaks = seq(0,3000,500),
                     sec.axis = sec_axis(~./250,
                                         name = "Rörelsemarginal [%]",
                                         labels=seq(0,12,1.5),
                                         breaks = seq(0,12,1.5)))+
  labs( x = "År",
        y = "Omsättning\n MKR",
        title = "Lindab omsättning & rörelsemarginal per kvartal",
        caption = "Källa: Börsdata")+
  facet_grid(~period)


