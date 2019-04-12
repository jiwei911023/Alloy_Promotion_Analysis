install.packages("dplyr")
install.packages("magrittr")
install.packages("purrr")
install.packages("tidyr")

#Importing the Dataset
setwd("C:/Users/54329/Desktop/R/Datasets")

#Packages Used
library(ggplot2)
library(plotly)
library(readxl)
library(minpack.lm)
library(dplyr)
library(magrittr)
library(readxl)
Web_Analytics_Case_Student_Spreadsheet <- read_excel("C:/Users/54329/Desktop/R/Datasets/Web Analytics Case Student Spreadsheet.xls")
View(Web_Analytics_Case_Student_Spreadsheet)


#Importing the Weekly Tab
weekly_visits <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                            sheet = "Weekly Visits", skip = 4)
colnames(weekly_visits)<-c("week","visits","unique_visits","pageviews",
                           "page_visits","avg_time_on_site",
                           "bounce_rate","%_new_visits")


#Importing Demographics
library(readxl)
demographics <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                           sheet = "Demographics", col_names = FALSE)
View(demographics)


colnames(demographics)<-c("field", "visits")


all_traffic_sources<- demographics[2:5,]
colnames(all_traffic_sources)[1]<-c("all_traffic_sources")

top_10_ref<-demographics[7:16,]
colnames(top_10_ref)[1]<-c("top_10_referring_sites")

Top_10_se<-demographics[18:27,]
colnames(Top_10_se)<-c("Top_Ten_Search_Engine_Sources")

top_10_geo<-demographics[29:38,]
colnames(top_10_geo)<-c("top_10_geo")

top_10_browser<-demographics[40:49,]
colnames(top_10_browser)<-c("Top_Ten_Browsers_Used")

top_10_OS<-demographics[51:60,]
colnames(top_10_OS)<-c("Top_Ten_Operating_Systems_Used")


#Importing Financial
library(readxl)
fin <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                  sheet = "Financials", skip = 4)

colnames(fin)[4]<-c("pounds_sold")
colnames(fin)[1]<-c("week")
fin$costs<- c(fin$Revenue-fin$Profit)
fin$rev_pound<-c(fin$Revenue/fin$pounds_sold)

#Joining Data
library(sqldf)

weekly <- sqldf("SELECT * 
                FROM weekly_visits a
                LEFT JOIN fin b
                ON a.week = b.week
                ")



weekly$margin <- weekly$Revenue
margin <- c()
for(i in 1:nrow(weekly)){margin[i] <- weekly$Profit[i]/(weekly$Revenue[i]-weekly$Profit[i])}
weekly$margin <- margin

#Importing Pounds Sold
library(readxl)
pounds <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                     sheet = "Lbs. Sold", skip = 4)
colnames(pounds)<-c("week","pounds_sold")

#Importing  Daily Visits
library(readxl)
daily_visits <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                           sheet = "Daily Visits", skip = 4)







#Summary Stats
summary(weekly_visits)
summary(fin)
summary(pounds)
summary(daily_visits)

#Standard Deviation 
sd(weekly_visits$visits)
sd(weekly_visits$unique_visits)
sd(fin$Revenue)
sd(fin$Profit)
sd(pounds$pounds_sold)



##Promotion Schedule
initial_promotion<- weekly_visits[1:14,]
pre_promotion<- weekly_visits[15:35,]
promotion<- weekly_visits[36:52,]
post_promotion<- weekly_visits[53:66,]

#Pre Website subsets
preweb_2005<-pounds[1:52,]
preweb_2006<-pounds[53:105,]
preweb_2007<-pounds[106:157,]
preweb_2008<-pounds[158:177,]

#Website pounds
initial_pounds<-fin[1:14,]
pre_pounds<- fin[15:35,]
pro_pounds<- fin[36:52,]
post_pounds<- fin[53:66,]

INI_M<- (sum(initial_pounds$Profit)/sum(initial_pounds$costs))*100
print(INI_M)
PRE_M <- (sum(pre_pounds$Profit)/sum(pre_pounds$costs))*100
print(PRE_M)
PRO_M<- (sum(pro_pounds$Profit)/sum(pro_pounds$costs))*100
print(PRO_M)
POST_M<- (sum(post_pounds$Profit)/sum(post_pounds$costs))*100
print(POST_M)


#means for weekly visits
ini_v_mean<-mean(initial_promotion$visits)
pre_v_mean<- mean(pre_promotion$visits)
promo_v_mean<- mean(promotion$visits)
post_v_mean<- mean(post_promotion$visits)

#means for Unique Visits
ini_uq_mean<-mean(initial_promotion$unique_visits)
pre_uq_mean<- mean(pre_promotion$unique_visits)
promo_uq_mean<- mean(promotion$unique_visits)
post_uq_mean<- mean(post_promotion$unique_visits)

#means for revenue
ini_r_mean<-mean(initial_pounds$Revenue)
pre_r_mean<- mean(pre_pounds$Revenue)
promo_r_mean<- mean(pro_pounds$Revenue)
post_r_mean<- mean(post_pounds$Revenue)

#means for Profit
ini_p_mean<-mean(initial_pounds$Profit)
pre_p_mean<- mean(pre_pounds$Profit)
promo_p_mean<- mean(pro_pounds$Profit)
post_p_mean<- mean(post_pounds$Profit)

#means for LBS Sold
ini_lbs_mean<-mean(initial_pounds$pounds_sold)
pre_lbs_mean<- mean(pre_pounds$pounds_sold)
promo_lbs_mean<- mean(pro_pounds$pounds_sold)
post_lbs_mean<- mean(post_pounds$pounds_sold)


pre_2005_mean<- mean(preweb_2005$pounds_sold)
pre_2006_mean<- mean(preweb_2006$pounds_sold)
pre_2007_mean<- mean(preweb_2007$pounds_sold)
pre_2008_mean<- mean(preweb_2008$pounds_sold)

#PLOTS/GRAPHS/CHARTS
#Ploty LBS vs Visits

p <- plot_ly(weekly) %>%
  add_trace(x = ~week, y = ~visits, type = 'bar', name = 'Visits',
            marker = list(color = '#C9EFF9'),
            hoverinfo = "text",
            text = ~paste(visits, ' clicks')) %>%
  add_lines(x = ~week, y = ~pounds_sold, type = 'bar', name = 'Lbs Sold', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text",
            text = ~paste(pounds_sold, 'lbs')) %>%
  layout(title = 'Visits and Pounds per Week',
         xaxis = list(title = ""),
         yaxis = list(side = 'left', title = 'Visits per Week', showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Lbs per Week', showgrid = FALSE, zeroline = FALSE))
print(p)


#Time Series

#Pounds Sold per Week since Begining
require(xts)
sold <- ts(pounds$pounds_sold, frequency = 12, start = c(2005, 5), end = c(2010,8), deltat = 1/6)
plot(as.xts(sold), major.format = "%Y-%m", col = "red", main = "Pounds Sold All Time")



#Pounds Sold per Week
sold_lbs<-ggplot(fin, aes(week, pounds_sold))+geom_col()
print(sold_lbs)

#Pounds Sold + weekly visits
ggplot(weekly) +
  geom_bar(aes(x = week, weight = visits)) +
  geom_line(aes(x = week, y = pounds_sold))






#Pounds Sold per Week
require(xts)
Pounds <- ts(pounds$pounds_sold, frequency = 12, start = c(2008, 5), end = c(2009,8), deltat = 1/6)
plot(as.xts(Pounds), major.format = "%Y-%m", col = "purple", main = "Pounds Sold")

#Bounce Rate per Week

require(xts)
Bounce <- ts(weekly_visits$bounce_rate, frequency = 66, start = c(2008, 5), end = c(2009,8), deltat = 1/6)
plot(Bounce, col = "orange", main = "Bounce Rate")
require(xts)
Pounds_sd<-ts(pounds$pounds_sold, frequency = 66, start=c(2008,5), end=c(2009,8), deltat =1/6)
plot(Pounds_sd,col= "orange", main= "Pounds Sold")



weekly_df <- as.data.frame(weekly)
par(mfcol=c(2,1))
ggplot(data = weekly_df, aes(x = week, y = unique_visits, group = 1, main = "Unique Visits per Week")) +
  geom_line(color = "orange", size = 2) +
  geom_point(color = "orange") +
  geom_vline(aes(xintercept = which(weekly_df$week == "Aug 31 - Sep 6")), size = 1) +
  geom_vline(aes(xintercept = which(weekly_df$week == "Jan 25 - Jan 31")), size = 1) +
  geom_vline(aes(xintercept = which(weekly_df$week == "May 24 - May 30")), size = 1) +
  theme_minimal() +
  ggtitle("Unique Visits per Week") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14,face = "bold"), 
        plot.title = element_text(size = 20, face = "bold"))
ggplot(data = weekly_df, aes(x = week, y = Inquiries, group = 1, main = "Inquiries per Week")) +
  geom_line(color = "orange", size = 2) +
  geom_point(color = "orange") +
  geom_vline(aes(xintercept = which(weekly_df$week == "Aug 31 - Sep 6")), size = 1) +
  geom_vline(aes(xintercept = which(weekly_df$week == "Jan 25 - Jan 31")), size = 1) +
  geom_vline(aes(xintercept = which(weekly_df$week == "May 24 - May 30")), size = 1) +
  theme_minimal() +
  ggtitle("Inquiries per Week") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14,face = "bold"), 
        plot.title = element_text(size = 20, face = "bold"))




bounce_graph<-ggplot(data=weekly_visits, aes(x=week, y=bounce_rate))+geom_point()+geom_abline()
print(bounce_graph)


bounce_graph2<- plot(x=weekly_visits$week, y=weekly_visits$bounce_rate, type = "p")
print(bounce_graph2)


#Visits per Week but time series did not show it correctly
require(xts)
Visits <- ts(weekly_visits$visits, frequency = 12, start = c(2008, 5), end = c(2009,8), deltat = 1/6)
plot(as.xts(Visits), major.format = "%Y-%m", col = "purple", main = "Visits per Week")


#Visits Per Week on a bar chart 
Visits<- ggplot(data=weekly_visits, aes(week, visits))+ geom_col()
print(Visits)





#Revenue/Pound Sold per Weeks
require(xts)
rev_lb <- ts(fin$rev_pound, frequency = 12, start = c(2008, 5), end = c(2009,8), deltat = 1/6)
plot(as.xts(rev_lb), major.format = "%Y-%m", col = "blue", main = "Revenue per Pound")

#Margin per Week
require(xts)
margin <- ts(weekly$margin, frequency = 12, start = c(2008, 5), end = c(2009,8), deltat = 1/6)
plot(as.xts(margin), major.format = "%Y-%m", col = "red", main = "Margin per Week")

# Revenue per Week
require(xts)
Revenue_ts <- ts(weekly$Revenue, frequency = 12, start = c(2008, 5), end = c(2009,8), deltat = 1/6)
plot(as.xts(Revenue_ts), major.format = "%Y-%m", col = "red", main = "Revenue per Week")


# Profit per Week 
require(xts)
profit_ts <- ts(weekly$Profit, frequency = 12, start = c(2008, 5), end = c(2009,8), deltat = 1/6)
plot(as.xts(profit_ts), major.format = "%Y-%m", col = "red", main = "Profit per Week")

# Costs per Week 
require(xts)
costs_ts <- ts(fin$costs, frequency = 12, start = c(2008, 5), end = c(2009,8), deltat = 1/6)
plot(as.xts(costs_ts), major.format = "%Y-%m", col = "red", main = "Costs per Week")


## Scatter plots to find correlation
#Pounds Sold per Revenue
R_LBS<-ggplot(fin, aes(pounds_sold, Revenue)) + geom_point()
print(R_LBS)

# Revenue vs visits scatter plot
revenue_visits <- ggplot(weekly, aes(visits, Revenue)) + geom_point()
print(revenue_visits)


# pounds sold vs visits scatter plot
pounds_visits <- ggplot(weekly, aes(visits, pounds_sold)) + geom_point()
print(pounds_visits)


pounds_visits <- ggplot(weekly, aes(bounce_rate, pounds_sold)) + geom_point()
print(pounds_visits)


# Revenue vs avg time on site scatter plot
revenue_time <- ggplot(weekly, aes(avg_time_on_site, Revenue)) + geom_point()
print(revenue_time)


# Revenue vs page/visit scatter plot
revenue_pv <- ggplot(weekly, aes(page_visits, Revenue)) + geom_point()
print(revenue_pv)


#Visits vs Profit 
v_profit<- ggplot(weekly, aes(Profit, visits)) + geom_point()
print(v_profit)

#Pounds Sold vs Visits
visit_pounds<-ggplot(weekly,aes(pounds_sold, visits))+geom_point(color="orange")+(xlabs="Pounds Sold"+ ylab="Visits")+ggtitle("Pounds Sold per Visit")+theme(plot.title = element_text(lineheight=.8, face="bold"))
print(visit_pounds)
dp <- ggplot(data=weekly, aes(x=week, y=pounds_sold)) + geom_point()
print(dp)


#Inquiry
ggplot(df,aes(x=v1,y=v2,fill=v1))+geom_bar(stat='identity')+coord_flip()+ggtitle("Web Inquiry in 4 Period")+theme(plot.title = element_text(lineheight=.8, face="bold"))

#pageviews
ggplot(df,aes(x=v1,y=v3,fill=v1))+geom_bar(stat='identity')+coord_flip()+ggtitle("Pageviews in 4 Period")+theme(plot.title = element_text(lineheight=.8, face="bold"))

#Regions
region<-c("Eastern Europe", "Southern Europe", "South-Eastern Asia", "Southern Asia", "Northern Europe", "Eastern Asia", "Western Europe", "Central America", "North America", "South America")
visits_r<-c(1427,1538,1968,2589,2721,3228,5214,6776,17509,22616)
ggplot(data=top_10_geo,aes(x=reorder(region,visits_r), y=visits_r))+
  geom_bar(stat='identity', color="orange")+coord_flip()+
  xlab("Region")+ylab("Number of Visits")+ggtitle("Regions per Visits")+
  theme(plot.title = element_text(lineheight = .8, face="bold"))


ggplot(data=top_10_geo,aes(x=reorder(region, visits_r),y=visits_r, fill=visits_r))+geom_bar(stat = 'identity')+coord_flip()+ggtitle("Regions")+theme(plot.title = element_text(lineheight = .8, face="bold"))

ggplot(data=top_10_geo,aes(x=reorder(region, visits_r),y=visits_r,fill=visits_r))+geom_bar(stat = 'identity',color="orange")+coord_flip()+ggtitle("Regions")+theme(plot.title = element_text(lineheight = .8, face="bold"))


#Top ten Reffering Sites
visits_vec<-c(310,337,344,379,389,582,693,3138,8044,15626)
top_10_ref_vec<-c("psicofxp.com","mail.google.com","mu.com","thomasnet.com","freepatentsonline.com","searchportal.information.com","globalspec.com","sedoparking.com","pagead2.googlesyndication.com","pagead2.googlesyndication.com")

ggplot(data = top_10_ref, aes(x=reorder(top_10_ref_vec,visits_vec), y=visits_vec))+
  geom_bar(stat='identity', color ="orange")+coord_flip()+
  xlab("Referring Websites")+ylab("Number of Visits")+
  ggtitle("Top 10 Referring Sites")+
  theme(plot.title = element_text(lineheight = .8, face="bold"))


ggplot(data=inquiry, aes(Period, Inquires,fill=Period))+ geom_col()+coord_flip()
print(In)
v1<-factor(c('Initial','Pre','Pro','Post'),levels=c('Post','Pro','Pre','Initial'))

v2<-c(102,136,108,76)
v3 <- c(32867,31680,52731,26155)

df<-data.frame(v1,v2)
#Inquiry
ggplot(df,aes(x=v1,y=v2,fill=v1))+geom_bar(stat='identity')+coord_flip()+ggtitle("Web Inquiry in 4 Period")+theme(plot.title = element_text(lineheight=.8, face="bold"))

#pageviews
ggplot(df,aes(x=v1,y=v3,fill=v1))+geom_bar(stat='identity')+coord_flip()+ggtitle("Pageviews in 4 Period")+theme(plot.title = element_text(lineheight=.8, face="bold"))








###Question 8
summary(pounds)
describe(pounds)
summary(pounds)
median(pounds$pounds_sold)
sd(pounds$pounds_sold)
hist(pounds$pounds_sold,breaks = 50)
z <- c()
for(i in 1:nrow(pounds)){
  z[i] <- (pounds$pounds_sold[i]-mean(pounds$pounds_sold))/sd(pounds$pounds_sold)
}
print(z)
pounds$zscore <- z
new_z <- pounds[order(pounds$zscore),]
new_z <- as.data.frame(new_z)
new_z
new_z$pounds_sold[245]##1 std
new_z$pounds_sold[279]###2 std
new_z$pounds_sold[289]#3 std
summary(new_z)
skew(new_z$pounds_sold,na.rm = TRUE,type = 3)
kurtosis(new_z$pounds_sold,na.rm = TRUE,type = 3)
hist(new_z$zscore,breaks = 50)