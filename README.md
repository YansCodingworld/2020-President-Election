# 2020-President-Election
---
title: "Data Analytic Report 1"
author: "Lorraine"
date: "10/19/2021"
output:
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, warning = F, message = F}
library("lubridate")
library("tidyverse")
library(ggplot2)
```

# Problem 1
a. Who is ahead in each of these three states? What is the percentage difference for each state?
```{r}

# read
current_address = getwd()
current_address
d16 <- read.csv(paste0("/Users/lorrainejiang/Desktop/Pstat120C/president_general_polls_sorted_end_date_2016.csv"))


#dimensions of the data
dim(d16)
# create data
d16$startdate2 <- mdy(d16$startdate)
d16$enddate2 <- mdy(d16$enddate)

#subset data based on start date
d16_s <- subset(d16,d16$startdate2 >"2016-08-01")
#subset data based on end date
d16_s <- subset(d16_s, d16_s$enddate2 < "2016-11-02")
d16_state <- subset(d16_s, d16_s$state %in% c("Michigan","North Carolina", "Georgia"))

sum(d16_s$total.clinton)
##total number of people who reported to vote for Trump
sum(d16_s$total.trump)

##difference in total
sum(d16_s$total.clinton)-sum(d16_s$total.trump)
##percentage
(sum(d16_s$total.clinton)-sum(d16_s$total.trump))/(sum(d16_s$total.clinton)+sum(d16_s$total.trump))
## Michigan
d16_s$state
index_penn=which(d16_s$state=="Michigan")

n1=sum(d16_s$total.clinton[index_penn])
n2=sum(d16_s$total.trump[index_penn])
n1
n2
##the following number shows the percentage difference within the group of voters 
##who reported to vote for Trump or Clinton in Michigan
n1/(n1+n2)
n2/(n1+n2)
round(100*(n1/(n1+n2)-n2/(n1+n2)),4)
```
In Michigan, Clinton is ahead as Clinton has 62018.73votes, but Trump has 57809.46 votes.
The percentage difference between Clinton and Trump is 3.5128%.

```{r}
# Georgia
index_penn=which(polls_data_2016$state=="Georgia")

n1=sum(d16_s$total.clinton[index_penn])
n2=sum(d16_s$total.trump[index_penn])
n1
n2
##the following number shows the percentage difference within the group of voters 
##who reported to vote for Trump or Clinton in Georgia
round(100*(n1/(n1+n2)-n2/(n1+n2)),3)
```
In Georgia, Trump is ahead as Clinton has 99108.63 votes, but Trump has 110777.4 votes.
The percentage difference between Clinton and Trump is -5.56%.
```{r}
# North Carolina
index_penn=which(polls_data_2016$state=="North Carolina")

n1=sum(d16_s$total.clinton[index_penn])
n2=sum(d16_s$total.trump[index_penn])
n1
n2
##the following number shows the percentage difference within the group of voters 
##who reported to vote for Trump or Clinton in North Carolina
round(100*(n1/(n1+n2)-n2/(n1+n2)),4)
```
In North Carolina, Clinton is ahead as Clinton has 152858.7 votes, but Trump has 152173.8votes.
The percentage difference between Clinton and Trump is 0.2245%.

b. Run a paired t test of the counts in polls for each of the state. Who is in favor of winning based on the test? Is the test significant? Is there potential problem?
```{r}
##the current date is month date year, the follow linesconvert to the typical year month date data
date_penn <- mdy(polls_data_2016$enddate[index_penn])
ylim_value=c(min(polls_data_2016$total.clinton[index_penn],polls_data_2016$total.trump[index_penn]),
             max(polls_data_2016$total.clinton[index_penn],polls_data_2016$total.trump[index_penn]))
plot(date_penn,polls_data_2016$total.clinton[index_penn],
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Pennsylvania',ylim=ylim_value)
lines(date_penn,polls_data_2016$total.trump[index_penn],col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))
t.test(polls_data_2016$total.clinton[index_penn]-polls_data_2016$total.trump[index_penn],alternative='greater')
```
Null hypothesis: $\mu_{Clinton}\leq\mu_{Trump}$
Alternative: $\mu_{Clinton}>\mu_{Trump}$

From the table, we see the p-value is < 2.2e-16. Thus, we reject the null hypothesis and conclude that $\mu_{Clinton}>\mu_{Trump}$. Clinton is in favor of winning based on the test. It is not significant.
*Potential problem is that the election is not solely based on the difference of votes. The result of swing states are more important than the total difference votes.

c. Run a Wilcoxon signed-rank test of the counts in polls for each of the state. Who is in favor of winning based on the test? Is the test significant? Is there potential problem of the test?
```{r}
wilcox.test(polls_data_2016$total.clinton[index_penn], polls_data_2016$total.trump[index_penn],alternative='greater')

percentage_diff_penn=(polls_data_2016$total.clinton[index_penn]-polls_data_2016$total.trump[index_penn])/(polls_data_2016$total.clinton[index_penn]+polls_data_2016$total.trump[index_penn])
plot(date_penn,percentage_diff_penn,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Pennsylvania')
abline(a=0,b=0)
```
Null hypothesis: $\mu_{Clinton}\leq\mu_{Trump}$
Alternative: $\mu_{Clinton}>\mu_{Trump}$

As the p value (0.02563) is larger than 0.001, so we cannot reject the null and conclude that $\mu_{Clinton}\leq\mu_{Trump}$, Trump is in favor of winning based on the test. It is significant. Potential problem is that the election result may have small difference from poll and the rank can be greatly changed even with small change in the votes in the swing states.

d. Fit a linear model of the percentage difference with respect to date of the polls separately for each of these states. Show a plot of the observations of the polls, fitted values and confidence interval of the fitted line for each of these state. From the linear model and observations, which state may have the closest election (in terms of percentage difference)?
```{r}

## Michigan
statename = "Michigan"
index_penn=which(polls_data_2016$state==statename)
date_penn <- mdy(polls_data_2016$enddate[index_penn])
###explore whether this a linear trend of the difference in percentage 
##create another date frame
counts_penn_for_lm <- data.frame(
  data_date = date_penn,
  percentage_diff = (polls_data_2016$total.clinton[index_penn]-polls_data_2016$total.trump[index_penn])/(polls_data_2016$total.clinton[index_penn]+polls_data_2016$total.trump[index_penn])
)

lm_model_penn=lm(percentage_diff~(data_date),data=counts_penn_for_lm)

##
##early poll probably does not matter much, recent poll are more important 
index_date_oct_nov=which(counts_penn_for_lm$data_date>="2016-01-01")

counts_penn_for_lm_oct <- data.frame(
  data_date = date_penn[index_date_oct_nov],
  percentage_diff =counts_penn_for_lm$percentage_diff[index_date_oct_nov]
)

lm_model_penn_after_oct=lm(percentage_diff~(data_date),data=counts_penn_for_lm_oct)


conf_interval_penn_fitted_oct= predict(lm_model_penn_after_oct, newdata=counts_penn_for_lm_oct, interval="confidence",
                         level = 0.95)

##this is a plot with 95% interval
plot(counts_penn_for_lm_oct$data_date,counts_penn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main=statename)
polygon(c(rev(counts_penn_for_lm_oct$data_date), counts_penn_for_lm_oct$data_date), 
        c(rev(conf_interval_penn_fitted_oct[,2]), conf_interval_penn_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_penn_for_lm_oct$data_date,counts_penn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main=statename)
lines(counts_penn_for_lm_oct$data_date,lm_model_penn_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Michigan')

```
```{r}
## Georgia
statename = "Georgia"
index_penn=which(polls_data_2016$state==statename)
date_penn <- mdy(polls_data_2016$enddate[index_penn])
###explore whether this a linear trend of the difference in percentage 
##create another date frame
counts_penn_for_lm <- data.frame(
  data_date = date_penn,
  percentage_diff = (polls_data_2016$total.clinton[index_penn]-polls_data_2016$total.trump[index_penn])/(polls_data_2016$total.clinton[index_penn]+polls_data_2016$total.trump[index_penn])
)

lm_model_penn=lm(percentage_diff~(data_date),data=counts_penn_for_lm)

##
##early poll probably does not matter much, recent poll are more important 
index_date_oct_nov=which(counts_penn_for_lm$data_date>="2016-01-01")

counts_penn_for_lm_oct <- data.frame(
  data_date = date_penn[index_date_oct_nov],
  percentage_diff =counts_penn_for_lm$percentage_diff[index_date_oct_nov]
)

   
lm_model_penn_after_oct=lm(percentage_diff~(data_date),data=counts_penn_for_lm_oct)


conf_interval_penn_fitted_oct= predict(lm_model_penn_after_oct, newdata=counts_penn_for_lm_oct, interval="confidence",
                         level = 0.95)


##this is a plot with 95% interval
plot(counts_penn_for_lm_oct$data_date,counts_penn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main=statename)
polygon(c(rev(counts_penn_for_lm_oct$data_date), counts_penn_for_lm_oct$data_date), 
        c(rev(conf_interval_penn_fitted_oct[,2]), conf_interval_penn_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_penn_for_lm_oct$data_date,counts_penn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main=statename)
lines(counts_penn_for_lm_oct$data_date,lm_model_penn_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Georgia')

```
```{r}
## North Carolina
statename = "North Carolina"
index_penn=which(polls_data_2016$state==statename)
date_penn <- mdy(polls_data_2016$enddate[index_penn])
###explore whether this a linear trend of the difference in percentage 
##create another date frame
counts_penn_for_lm <- data.frame(
  data_date = date_penn,
  percentage_diff = (polls_data_2016$total.clinton[index_penn]-polls_data_2016$total.trump[index_penn])/(polls_data_2016$total.clinton[index_penn]+polls_data_2016$total.trump[index_penn])
)

lm_model_penn=lm(percentage_diff~(data_date),data=counts_penn_for_lm)

##
##early poll probably does not matter much, recent poll are more important 
index_date_oct_nov=which(counts_penn_for_lm$data_date>="2016-01-01")

counts_penn_for_lm_oct <- data.frame(
  data_date = date_penn[index_date_oct_nov],
  percentage_diff =counts_penn_for_lm$percentage_diff[index_date_oct_nov]
)

   
lm_model_penn_after_oct=lm(percentage_diff~(data_date),data=counts_penn_for_lm_oct)


conf_interval_penn_fitted_oct= predict(lm_model_penn_after_oct, newdata=counts_penn_for_lm_oct, interval="confidence",
                         level = 0.95)

##this is a plot with 95% interval
plot(counts_penn_for_lm_oct$data_date,counts_penn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main=statename)
polygon(c(rev(counts_penn_for_lm_oct$data_date), counts_penn_for_lm_oct$data_date), 
        c(rev(conf_interval_penn_fitted_oct[,2]), conf_interval_penn_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_penn_for_lm_oct$data_date,counts_penn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main=statename)
lines(counts_penn_for_lm_oct$data_date,lm_model_penn_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='North Carolina')
```


e. From the real results of 2016 election, which state has the smallest margin (in terms of percentage difference)? Discuss at least two reasons that
are different than what polls indicate. (You may check Wikipedia for
2016 US presidential election to find out the real voting results for each
state.)
```{r}

```

f. Do polls correctly predict the candidate who wins these states? Discuss
the bias of polls in these states. Name a few possible reasons.

Poll shows Clinton wins in Michigan, tie in NC, lose in Georgia, but actually Clinon lose all 3 states.

- Polls may have non response bias. Trump supporters are not willing to response to the poll.

- Polls may have selection bias. Maybe more Clinton supporters are selected in the poll.

- Polls may be bias by the press to carry the polls. 

# Problem 2

##get current working directory
current_address=getwd() 
##load the data for general poll for 2016 presidential election
polls_data_2020=read.csv(paste0(current_address,"/data/president_polls_2020.csv"))
#polls_data_2020=read.csv(paste0(current_address,"/data/president_general_polls_2016.csv"))

##let's look at the data after Apr 10, 2020 because 
library(lubridate) ##this is to change the format of date

date_2020= mdy(polls_data_2020$end_date)
date_2020_latest_day=date_2020[1]
index_selected=which(date_2020>='2020-08-01') 
polls_data_2020=polls_data_2020[index_selected,]  ###only work on the poll after Aug 1
##get  values with sample size unknown
index_na=which(is.na(polls_data_2020$sample_size)==T)
index_na
##only look at Biden or Trump 
polls_data_2020=polls_data_2020[which(polls_data_2020$answer=='Biden'|polls_data_2020$answer=='Trump'),]
# ##you may delete USC Dornsife/Los Angeles Times and Survey Monkey as their polls seem not disjoint
 polls_data_2020=polls_data_2020[which(polls_data_2020$pollster_id!=1610&polls_data_2020$pollster_id!=1193),]

##they do not match. Some poll has mistakes 
length(which(polls_data_2020$answer=='Biden'))
length(which(polls_data_2020$answer=='Trump'))

##now let's delete those poll that only contains one candidate
polls_data_2020_question_id_num=unique(polls_data_2020$question_id)

for(i in 1:length(unique(polls_data_2020$question_id)) ){
  index_set=which(polls_data_2020$question_id==polls_data_2020_question_id_num[i])
  if(length(index_set)!=2){
    polls_data_2020=polls_data_2020[-index_set,]
  }
}
##now they match 
length(which(polls_data_2020$answer=='Biden'))
length(which(polls_data_2020$answer=='Trump'))

###delete sample size NA
index_NA=which(is.na(polls_data_2020$sample_size)==T)
index_NA
polls_data_2020=polls_data_2020[-index_NA,]
###

index_trump=which(polls_data_2020$answer=='Trump')
index_biden=which(polls_data_2020$answer=='Biden')
##total counts to trump

total_count_trump=sum(polls_data_2020$pct[index_trump]*polls_data_2020$sample_size[index_trump])
##total counts to biden 
total_count_biden=sum(polls_data_2020$pct[index_biden]*polls_data_2020$sample_size[index_biden])

##the percentage may be more meaningful as some poll may be double counted 
(total_count_biden-total_count_trump)/(total_count_biden+total_count_trump)
##look at poll for Pennsylvania 
date_2020= mdy(polls_data_2020$end_date)
polls_data_2020$state
index_penn_2020=which(polls_data_2020$state=="Pennsylvania")

##who is leading Pennsylvania
index_biden_penn_2020=which(polls_data_2020$answer=='Biden' & polls_data_2020$state=="Pennsylvania")
index_trump_penn_2020=which(polls_data_2020$answer=='Trump' & polls_data_2020$state=="Pennsylvania")

counts_biden_penn_2020=polls_data_2020$pct[index_biden_penn_2020]*polls_data_2020$sample_size[index_biden_penn_2020]
counts_trump_penn_2020=polls_data_2020$pct[index_trump_penn_2020]*polls_data_2020$sample_size[index_trump_penn_2020]

n1_2020_penn=sum(counts_biden_penn_2020)
n2_2020_penn=sum(counts_trump_penn_2020)

n1_2020_penn
n2_2020_penn

(n1_2020_penn-n2_2020_penn)/(n1_2020_penn+n2_2020_penn)

#plot the poll by Biden and Trump over time in Penn
ylim_value=c(min(counts_biden_penn_2020,counts_trump_penn_2020),
             max(counts_biden_penn_2020,counts_trump_penn_2020))
plot(date_2020[index_biden_penn_2020],counts_biden_penn_2020,
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Pennsylvania',ylim=ylim_value)
lines(date_2020[index_trump_penn_2020],counts_trump_penn_2020,col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Biden','Trump'))

#plot the difference 
plot(date_2020[index_trump_penn_2020],counts_biden_penn_2020-counts_trump_penn_2020,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='Pennsylvania')
abline(a=0,b=0)
##plot percentage
plot(date_2020[index_trump_penn_2020],(counts_biden_penn_2020-counts_trump_penn_2020)/(counts_biden_penn_2020+counts_trump_penn_2020),
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='Pennsylvania')
abline(a=0,b=0)

##early dates may not matter 
counts_penn_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_penn_2020],
  percentage_diff = (counts_biden_penn_2020-counts_trump_penn_2020)/(counts_biden_penn_2020+counts_trump_penn_2020)
)

lm_model_penn_2020=lm(percentage_diff~(data_date),data=counts_penn_for_lm_2020)
summary(lm_model_penn_2020)

conf_interval_penn_fitted_2020= predict(lm_model_penn_2020, newdata=counts_penn_for_lm_2020, interval="confidence",
                                       level = 0.95)

plot(counts_penn_for_lm_2020$data_date,counts_penn_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Pennsylvania')
polygon(c(rev(counts_penn_for_lm_2020$data_date), counts_penn_for_lm_2020$data_date), 
        c(rev(conf_interval_penn_fitted_2020[,2]), conf_interval_penn_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_penn_for_lm_2020$data_date,lm_model_penn_2020$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Pennsylvania')
lines(counts_penn_for_lm_2020$data_date,counts_penn_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Pennsylvania')



# Problem 5

Compare the polls in Florida and Iowa in 2016 and 2020

## a

Are most of the polls in these two states accurate to predict the elected candidates? If not, please give some reasons.

The first poll predicted Iowa Trump wins and Florida with no obvious winner. The result was Trump won both states. 

The 2020 poll predicted Trump loses Florida and Iowa was a tie, but in the result, Trump won both Iowa and Florida.

So most of the polls are not accurate. The reason is the same with the non-response bias and selection bias.

## b

For Iowa, is there a poll that approximately correct for the final outcome
of the election in Iowa? What is the name of this poll? You may search
the internet to know more some information about this pollster.

It seems most of the poll in the Internet are not accurate for the Iowa state result.

## c

Name a few possible reasons that account for the bias in polls for these
two states.

Polls may have nonresponse bias. Maybe Trump supporters are not willing to response to the poll.

The poll may have selection bias. More people are investigated in cities, where Trump supporters are more in the rural area.

## d

Widen the base with Cell Phones, Email, Text, and the Web

Run a Huge Public Survey, Ask indirect Questions

# *Appendix: Rcode*

```{r ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE}
```
