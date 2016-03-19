library(dplyr)
library(readr)
library(ggplot2)
library(reshape)

#preprocessing
df<-read_csv("data/repdata_data_StormData.csv.bz2",col_types =cols_only(EVTYPE=col_character(),FATALITIES=col_number(),
                                                                        INJURIES=col_number(),PROPDMG=col_number(),
                                                                        PROPDMGEXP=col_character(),CROPDMG=col_number(),
                                                                        CROPDMGEXP=col_character()))
df$EVTYPE<-toupper(df$EVTYPE)

df$EVTYPE <- sub("^(.*TORM.*)|^(.*STM.*)", "STORM", df$EVTYPE)
df$EVTYPE <- sub("^(.*SNOW.*)", "SNOW", df$EVTYPE)
df$EVTYPE <- sub("^(.*FLOOD.*)|^(.*FLD.*)|^(.*FLOOOD.*)", "FLOOD", df$EVTYPE)
df$EVTYPE <- sub("^(.*WIND.*)|^(.*WND.*)", "WIND", df$EVTYPE)
df$EVTYPE <- sub("^(.*HAIL.*)", "HAIL", df$EVTYPE)
df$EVTYPE <- sub("^(.*FREEZ.*)|^(.*FROST.*)", "FREEZING", df$EVTYPE)
df$EVTYPE <- sub("^(.*TORN.*)", "TORNADO", df$EVTYPE)
df$EVTYPE <- sub("^(.*ICE.*)|^(.*ICY.*)", "ICE", df$EVTYPE)
df$EVTYPE <- sub("^(.*DRY.*)|^(.*DRI.*)|^(.*DROUG.*)", "DRY WEATHER", df$EVTYPE)
df$EVTYPE <- sub("^(.*COLD.*)|^(.*COOL.*)|^(.*LOW.*TEMP.*)", "COLD WEATHER", df$EVTYPE)
df$EVTYPE <- sub("^(.*HOT.*)|^(.*HEAT.*)|^(.*HIGH.*TEMP.*)", "HOT WEATHER", df$EVTYPE)
df$EVTYPE <- sub("^(.*RAIN.*)|^(.*PRECIP.*)|^(.*SHOWE.*)", "RAIN", df$EVTYPE)
df$EVTYPE <- sub("^(.*SMOK.*)", "SMOKE", df$EVTYPE)
df$EVTYPE <- sub("^(.*SLIDE.*)", "SLIDE", df$EVTYPE)
df$EVTYPE <- sub("^(.*FIRE.*)", "FIRE", df$EVTYPE)
df$EVTYPE <- sub("^(.*WARM.*)", "WARM WEATHER", df$EVTYPE)
df$EVTYPE <- sub("^(.*TID.*)", "TIDE", df$EVTYPE)
df$EVTYPE <- sub("^(.*SURF.*)", "HIGH SURF", df$EVTYPE)
df$EVTYPE <- sub("^(.*LIGHTNI.*)|^(.*LIGNTN.*)", "LIGHTNING", df$EVTYPE)
df$EVTYPE <- sub("^(.*DUST.*)", "DUST", df$EVTYPE)
df$EVTYPE <- sub("^(.*WET.*)", "WET WEATHER", df$EVTYPE)
df$EVTYPE <- sub("^(.*ERSPOUT.*)", "WATERSPOUT", df$EVTYPE)
df$EVTYPE <- sub("^(.*SWELL.*)|^(.*HIGH.*WATER.*)|^(.*SEA.*)|^(.*MARINE.*)|^(.*HIGH.*WAVE.*)", "HIGH OR ROUGH SEA", df$EVTYPE)
df$EVTYPE <- sub("^(.*DAM.*)", "DAM FAILURE", df$EVTYPE)
df$EVTYPE <- sub("^(.*VOLCAN.*)|^(.*VOG.*)", "VOLCANIC EVENT", df$EVTYPE)
df$EVTYPE <- sub("^(.*HURRIC.*)|^(.*FLOYD.*)", "HURRICANE", df$EVTYPE)
df$EVTYPE <- sub("^(.*FOG.*)", "FOG", df$EVTYPE)


df.health<-data.frame(df[,1:3])
df.health<-df.health[complete.cases(df.health),]

df.economy<-data.frame(c(df[,1],df[,4:7]))
df.economy<-df.economy[complete.cases(df.economy),]

df.economy <- df.economy[!(df.economy$CROPDMGEXP == "?"),]
df.economy$CROPDMGEXP<-toupper(df.economy$CROPDMGEXP)
df.economy$PROPDMGEXP<-toupper(df.economy$PROPDMGEXP)
unique(df.economy$CROPDMGEXP)
unique(df.economy$PROPDMGEXP)
df.economy[df.economy$PROPDMGEXP=="K", ]$PROPDMGEXP <-3
df.economy[df.economy$PROPDMGEXP == "M", ]$PROPDMGEXP<-6
df.economy[df.economy$PROPDMGEXP == "B", ]$PROPDMGEXP<-9
df.economy[df.economy$CROPDMGEXP == "K", ]$CROPDMGEXP <-3
df.economy[df.economy$CROPDMGEXP == "M", ]$CROPDMGEXP<-6
df.economy[df.economy$CROPDMGEXP == "B", ]$CROPDMGEXP <-9

df.economy$PROPDMG<-df.economy$PROPDMG*(10^as.numeric(df.economy$PROPDMGEXP))
df.economy$CROPDMG<-df.economy$CROPDMG*(10^as.numeric(df.economy$CROPDMGEXP))

#check economy
scale_amount<-1000000
df.economy<-group_by(df.economy,EVTYPE)
df.economy.summary<-df.economy %>%
  summarise(ECONOMY=sum(PROPDMG)+sum(CROPDMG),PROPDMG=sum(as.numeric(PROPDMG)),CROPDMG=sum(as.numeric(CROPDMG)))
df.economy.summary<-mutate(df.economy.summary,ECONOMY=ECONOMY/scale_amount,PROPDMG=PROPDMG/scale_amount,CROPDMG=CROPDMG/scale_amount)
df.economy.summary<-arrange(df.economy.summary,desc(ECONOMY))
mdf<-melt(as.data.frame(df.economy.summary[1:10,]),c("EVTYPE","ECONOMY"))
ggplot(mdf,aes(x=reorder(as.factor(EVTYPE),value),y=value,fill=variable))+theme_bw()+geom_bar(stat="identity")+coord_flip()+ylab("Monetary damage (Millions of dollars)")+xlab("Event type")+ggtitle("Impact of event type on economy")

#check healthrisks
df.health<-group_by(df.health,EVTYPE)
df.health.summary<-df.health %>%
  filter(FATALITIES>0,INJURIES>0) %>%
  summarise(HEALTHRISKS=sum(FATALITIES)+sum(INJURIES),FATALITIES=sum(as.numeric(FATALITIES)),INJURIES=sum(as.numeric(INJURIES)))
df.health.summary<-arrange(df.health.summary,desc(HEALTHRISKS))

mdf<-melt(as.data.frame(df.health.summary[1:10,]),c("EVTYPE","HEALTHRISKS"))
ggplot(mdf,aes(x=reorder(as.factor(EVTYPE),value),y=value,fill=variable))+theme_bw()+geom_bar(stat="identity")+coord_flip()
