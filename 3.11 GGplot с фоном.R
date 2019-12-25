# http://ru-datascience.ru/2016/07/20/grafik-kak-v-dribble/

library(ggplot2)
library(jpeg)
library(grid)

df1 <- data.frame(SHORTDAY=c('Ïí','Âò','Ñð','×ò','Ïò'),
                  DAY=c('2016-07-11','2016-07-12','2016-07-13','2016-07-14','2016-07-15'),
                  d=c(0.32,0.4, 0.48, 0.52, 0.12))

img <- readJPEG("Mont_Blanc.jpg")

ggplot(df1, aes(x=DAY, y=d)) +
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")),
                    -Inf, Inf, -Inf, Inf) +
  geom_bar(stat='identity', aes(fill=SHORTDAY),alpha=0.6, width=0.97)+
  geom_text(aes(y=max(df1$d)1.05, label=SHORTDAY ),color='lightgray', size=8)+
  geom_text(aes(y=0.03, label=paste0(round(df1$d100, digits=0),'%')),
            size=9, color='white',fontface = "bold")+
  scale_y_continuous(limits=c(0, max(df1$d)*1.1))+
  guides(fill=FALSE)+
  theme_bw()+
  theme_void()