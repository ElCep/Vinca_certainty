##script analyse de coop d'aprs google ngram
## Info pour installer le package https://github.com/seancarmody/ngramr

library('ngramr')
library('ggplot2')
library('plyr')

rm(list = ls())
setwd("~/Dropbox/chaire_EAU/stat_irrigation/")

####################
### GRAPHIQUES ####
####################
#### DEFINITION DU THEME GGPLOT####
ggtheme<-theme(strip.text.x = element_text(size=14),
               strip.text.y = element_text(size=14),
               legend.title = element_text(size=14),
               legend.text = element_text(size=14),
               axis.title.x = element_text(face="bold", size=12),
               axis.title.y = element_text(face="bold", size=12),
               axis.text.x = element_text(size=12),
               axis.text.y = element_text(size=12, angle=45)
)


ng <- ngram(c("collective","cooperation","self-help","mutual aid","mutualisation",
              "self-management","self-organisation","self-organization"),
            year_start = 1880, smoothing = 5, corpus = "eng_2012")
ng$Phrase <- revalue(ng$Phrase, c("self - organization"="self - organisation", "self - help"="mutual aid"))

ng <- ddply(ng, c("Year", "Phrase"), summarise,
                 Frequency = sum(Frequency, na.rm = TRUE)
            )

ng$Phrase <- factor(ng$Phrase,levels(ng$Phrase)[c(1,2,3,6,5,4)])

ggcoop<-ggplot(ng, aes(x=Year, y=Frequency, color=Phrase))+
      geom_line()+
      labs(x = "année", y = "fréquence")+
      scale_colour_manual(name  ="",
                          values=c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f'))+
      ggtheme
ggcoop



ggsave("pres_audition/img/ngram_self.png",ggcoop, width = 10, height = 7)



#################################################
##            FRANCAIS 
#################################################

ngfr <- ngram(c("coopération","collectif","entraide","autogestion","auto-organisation","mutualisation"),
            year_start = 1880, smoothing = 5, corpus = "fre_2012")

ggcoop<-ggplot(ngfr, aes(x=Year, y=Frequency, color=Phrase))+
  geom_line()+
  labs(x = "année", y = "fréquence")+
  scale_colour_manual(name  ="",
                      values=c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f'))+
  ggtheme
ggcoop

ggsave("pres_audition/img/ngram_auto.png",ggcoop, width = 10, height = 7)
