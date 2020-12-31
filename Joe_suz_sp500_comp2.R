library(quantmod)
library(tidyverse)
library(TRD)
library(lubridate)
library(scales)
library(ggrepel)
library(reshape2)
setwd("/home/joe/Documents/Finance")

#sent to git 31Dec2020

bud_stuff <- read_csv("Joe_Suz_rtmt.csv") %>% 
  mutate(date=as.Date(Date,  format = "%d-%B-%y"), day=wday(date)) %>% 
  filter(!is.na(Date) & date > '2018-01-01')  

         
bud_stuff2 <- bud_stuff %>% 
  mutate(new_date1=date-1, new_date2=date-2,
         clean_date2=if_else(wday(date)==1, new_date2, new_date1), 
         label=paste0('$',prettyNum(round(Total/1000,0), big.mark = ',')),
         pctchg=100*((Total-lag(Total))/lag(Total))) %>% 
  select(clean_date2, Total, label) #, pctchg)

tail(bud_stuff2)

ggplot(data=bud_stuff2 , aes(x=clean_date2, y=Total/1000, group=1)) +
  geom_line(color="red")+
  geom_point() +
  ylab('Balance ($000s)') +
  scale_y_continuous(label=dollar) +
  geom_text_repel(aes(label =label)) +
  ggtitle("Bud/Barber IRA Balance")
  
SPX <- getSymbols("^GSPC",auto.assign = FALSE,src='yahoo', from = "2019-01-01")

chartSeries(SPX, theme = 'white',TA="addVo();addBBands();addCCI()")
addSMA(n = c(50, 200))

spx_df <- as.data.frame(SPX) %>% 
  rownames_to_column() 

#names(spx_df)
spx_df2 <- spx_df%>% 
  mutate(clean_date2=as.Date(rowname), sp500=GSPC.Adjusted) %>%
  select(clean_date2, sp500)


inboth <- inner_join(bud_stuff2, spx_df2, by=c("clean_date2")) %>% 
  mutate(Total=Total) 

print(paste0('S&P delta =', round((inboth[nrow(inboth),"sp500"]-inboth[28,"sp500"])/inboth[28,"sp500"]*100,2),"%"))

print(paste0('Joe & Suz delta =', round((inboth[nrow(inboth),"Total"]-inboth[28,"Total"])/inboth[28,"Total"]*100,2),"%"))

SP <- (inboth[nrow(inboth),"sp500"]-inboth[28,"sp500"])/inboth[28,"sp500"]
JoeSuz <- (inboth[nrow(inboth),"Total"]-inboth[28,"Total"])/inboth[28,"Total"]

idx <- JoeSuz[1,1]/SP[1,1]

print(paste('index S&P/Joe& Suz = ',round(idx,2)))

inboth_long <- inboth %>% 
  select(-label)

inboth_long <- melt(inboth_long, id.vars = c("clean_date2"))

inboth_long2 <- inboth_long %>% 
  group_by(variable) %>% 
  mutate(pct_chg=(value-lag(value))/lag(value), labl=paste0(round(pct_chg*100,1),"%"))%>% 
  filter(clean_date2 > '2019-07-26')

ggplot(data=inboth_long2 , aes(x=clean_date2, y=pct_chg, col=variable)) +
  geom_line()+
  geom_point() +
  ylab('Percent Change') +
  scale_y_continuous(label=dollar) +
  geom_text_repel(aes(label =labl)) +
  ggtitle("Joe/Suz vs. S&P500")


ggplot(inboth_long2, aes(x=pct_chg)) + 
  geom_density(aes(group=variable, colour=variable, fill=variable), alpha=0.3)
