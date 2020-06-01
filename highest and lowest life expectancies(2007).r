library(dplyr)
library(gapminder)
library(ggplot2)
global_mean = gapminder%>%
  filter(year==2007)%>%
  summarise(mean=mean(lifeExp))%>%
  pull()
?pull
global_mean=round(global_mean,0)
global_mean
top2007 = gapminder %>%
  filter(year==2007)%>%
  arrange(desc(lifeExp))%>%
  select(country,lifeExp)%>%
  top_n(10)


bottom2007 = gapminder %>%
  filter(year==2007)%>%
  arrange(desc(lifeExp))%>%
  select(country,lifeExp)%>%
  top_n(-10)

gap2007<-bottom2007%>%
  bind_rows(top2007)%>%
  arrange(lifeExp)

gap2007$country <- factor(gap2007$country,levels=gap2007$country)

ggplot(gap2007,aes(lifeExp,country,color=lifeExp))+
  geom_point(size=5)+
  geom_segment(aes(xend=30,yend=country),size=3.25)+
  geom_text(aes(label=round(lifeExp,0)),color="white",size=2.8)+
  geom_vline(xintercept = global_mean,color="grey40",linetype=3,size=.5)+
  annotate("text",x=global_mean+4,y=3.7,label="the\nglobal\naverage",
           color="grey40")+
  annotate(
    "curve",
    x=global_mean+4,y=6.5,
    xend=global_mean,yend = 8.5,
    arrow=arrow(length = unit(0.2,"cm") ,type="closed"),
    )+
  #annotate(
    #"curve",
    #x = x_start, y = y_start,
    #xend = global_mean, yend = 7.5,
    #arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
   # color = "grey40"
  #)
  labs(title="Highest and lowest life expectancies, 2007",
       caption="Source: gapminder")+
  scale_x_continuous("",expand = c(0,0),limits = c(30,85),position = "top")+
  scale_color_gradient(low="red",high="blue")+
  theme(panel.background = element_rect(fill="white"),
        axis.line.x=element_line(color="black"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks = element_blank())
