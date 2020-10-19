---
title: "Inference for numerical data"
author: "Jack Wright"
date: "2020-10-18"
output:
  pdf_document: default
  openintro::pdf: default
  openintro::lab_report: default
---


```r
library(tidyverse)
library(openintro)
library(infer)
```

### Exercise 1

The cases are about 13583 school aged children.
Remember that you can answer this question by viewing the data in the data viewer or
by using the following command:


```r
data(yrbss)
```

### Exercise 2

1.  How many observations are we missing weights from?


```r
summary(yrbss$weight)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   29.94   56.25   64.41   67.91   76.20  180.99    1004
```

```r
yrbss%>%
  filter(is.na(weight))%>%
  nrow()
```

```
## [1] 1004
```
1004 cases are missing weights.


```r
yrbss <- yrbss %>% 
  mutate(physical_3plus = ifelse(yrbss$physically_active_7d > 2, "yes", "no"))
```

### Exercise 3


1.  Make a side-by-side boxplot of `physical_3plus` and `weight`. Is there a 
relationship between these two variables? What did you expect and why?

They look very similar. I was expecting people who were physical 3 times a week to weigh less on average but the medians look similar as well as the IQR.


```r
yrbss$physical_3plus<-as.factor(yrbss$physical_3plus)
yrb_plot<-yrbss%>%
  filter(!is.na(weight),!is.na(physical_3plus))
p<-ggplot(yrb_plot, aes(x=physical_3plus,y=weight))+
  geom_boxplot()
p
```

![](lab-7_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 



```r
yrbss %>%
  group_by(physical_3plus) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 3 x 2
##   physical_3plus mean_weight
##   <fct>                <dbl>
## 1 no                    66.7
## 2 yes                   68.4
## 3 <NA>                  69.9
```


### Exercise 4

1.  Are all conditions necessary for inference satisfied? Comment on each. You can 
compute the group sizes with the `summarize` command above by defining a new variable
with the definition `n()`

CONDITIONS FOR INFERENCE:

Independence:

it was a SRS and the two groups are not linked, necessarily one who excersizes 3 times a week plus cannot be in the group that doesn't.

Size:

the groups are less than 10% of the total population of kids who excersize and those who don't

in the study...


```r
#create new variable n

yrbss<- yrbss %>% 
  mutate(n = ifelse(yrbss$physically_active_7d > 2, "yes", "no"))

yrbss$n<-as.factor(yrbss$n)

view(yrbss)
yrbss%>%
 count(n)
```

```
## Storing counts in `nn`, as `n` already present in input
## i Use `name = "new_name"` to pick a new name.
```

```
## # A tibble: 3 x 2
##   n        nn
##   <fct> <int>
## 1 no     4404
## 2 yes    8906
## 3 <NA>    273
```


### Exercise 5

Hypothesis test:

H_0: the difference in the means between physical 3 plus and less than 3 are equal (mean1-mean2)=0
H_A: the weights are diffferent !=0


```r
obs_diff <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))
```

```
## Warning: Removed 1219 rows containing missing values.
```




```r
null_dist <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("yes", "no"))
```

```
## Warning: Removed 1219 rows containing missing values.
```


```r
ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](lab-7_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 



```r
ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](lab-7_files/figure-latex/unnamed-chunk-9-1.pdf)<!-- --> 



### Exercise 6

1.

How many of these null permutations have a difference of at least obs_stat?


```r
obs_diff_val<-obs_diff$stat[1]

null_list<-as.list(null_dist$stat)
null_abs<-lapply(null_list, FUN=function(x){abs(x)})

null_dist%>%
  summarise(mean= mean(stat, na.rm=TRUE))
```

```
## # A tibble: 1 x 1
##       mean
##      <dbl>
## 1 -0.00366
```

```r
null_dist%>%
  filter(stat>obs_diff_val)
```

```
## # A tibble: 0 x 2
## # ... with 2 variables: replicate <int>, stat <dbl>
```

none of the values are greater than the obs_diff_val


```r
null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")
```

```
## Warning: Please be cautious in reporting a p-value of 0. This result is an
## approximation based on the number of `reps` chosen in the `generate()` step. See
## `?get_p_value()` for more information.
```

```
## # A tibble: 1 x 1
##   p_value
##     <dbl>
## 1       0
```


### Exercise 7

1.

Construct and record a confidence interval for the difference between the weights of those who exercise at least three times a week and those who don’t, and interpret this interval in context of the data.


```r
# number of groups
n_1<-8406
n_2<-4408

x_bar_diff<-1.78
T_score<-pt(.025,4407,lower.tail = FALSE)*2

#get sigmas of samples
sigma_1<-yrbss %>%
  group_by(physical_3plus) %>%
  summarise(sd = sd(weight, na.rm = TRUE))%>%
  filter(physical_3plus=="yes")%>%
  select(sd)%>%
  as.double()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
sigma_2<-yrbss %>%
  group_by(physical_3plus) %>%
  summarise(sd = sd(weight, na.rm = TRUE))%>%
  filter(physical_3plus=="no")%>%
  select(sd)%>%
  as.double()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
SE<-sqrt((sigma_1^2 /n_1)+(sigma_2^2/n_2))

bot<-x_bar_diff-T_score*SE
top<-x_bar_diff+T_score*SE

cat("the 95% confidence interval for comparing the differnece between the means of these two independent samples is ",bot,"to",top)
```

```
## the 95% confidence interval for comparing the differnece between the means of these two independent samples is  1.465649 to 2.094351
```


I am not sure I fully understand the role of the null distribution that we calculated. This is a little backwards than I thought it would work but, maybe we can say, since H_0: there is NO difference bewteen the two groups falls OUTSIDE our 95% confidence interval, we can REJECT the null hypothesis. 

### Exercise 8

1.

Calculate a 95% confidence interval for the average height in meters (height) and interpret it in context.

Explore height

```r
summary(yrbss$height)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.270   1.600   1.680   1.691   1.780   2.110    1004
```

```r
height<-yrbss%>%
  filter(!is.na(height))%>%
  select(height)

n<-nrow(height)
df<-n-1
height<-height$height


x_bar<-mean(height)
sigma<-sd(height)
SE<-sigma/sqrt(n)
t_star<-qt(.025,df=df)

#confidence interval

bot<-x_bar-abs(t_star*SE)
top<-x_bar+abs(t_star*SE)

cat("the 95% confidence interval is ",bot," to ",top)
```

```
## the 95% confidence interval is  1.689411  to  1.693071
```



```r
t_star<-abs(qt(.05,df=df))
# the rest is the same
x_bar<-mean(height)
sigma<-sd(height)
SE<-sigma/sqrt(n)
bot_1<-x_bar-abs(t_star*SE)
top_1<-x_bar+abs(t_star*SE)

cat("the 90% confidence interval is ",bot_1," to ",top_1)
```

```
## the 90% confidence interval is  1.689705  to  1.692777
```

The difference is so slight, could this be due to the really tight standard error due to the sample size?



```r
yrbss<-as.data.frame(yrbss)
ggplot(yrbss, aes(x=height))+
  geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 1004 rows containing non-finite values (stat_bin).
```

![](lab-7_files/figure-latex/unnamed-chunk-15-1.pdf)<!-- --> 

Looking at the histogram. there is a huge peak at what appears to be the sample mean. This could cause the dramatic tightening of the confidence interval.


### Exercise 10

Conduct a hypothesis test evaluating whether the average height is DIFFERENT for those who exercise at least three times a week and those who dont.


```r
obs_diff <- yrbss %>%
  specify(height ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))
```

```
## Warning: Removed 1219 rows containing missing values.
```

hypothesis test:

H_0: the difference in the height for the two groups is zero 

(mean_diff=0)

H_A: the difference in heights is NOT zero 

(mean_diff!=0)


```r
#physically active count
n_1<-yrbss%>%
  filter(physical_3plus=="yes")%>%
  nrow()
n_2<-yrbss%>%
  filter(physical_3plus=="no")%>%
  nrow()
#difference in mean height
obs_diff <- yrbss %>%
  specify(height ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))
```

```
## Warning: Removed 1219 rows containing missing values.
```

```r
null<-0
#sd for physically active
sigma_1<-yrbss %>%
  group_by(physical_3plus) %>%
  summarise(sd = sd(height, na.rm = TRUE))%>%
  filter(physical_3plus=="yes")%>%
  select(sd)%>%
  as.double()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
#sd for not physically active
sigma_2<-yrbss %>%
  group_by(physical_3plus) %>%
  summarise(sd = sd(height, na.rm = TRUE))%>%
  filter(physical_3plus=="no")%>%
  select(sd)%>%
  as.double()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
#standard error
SE<-sqrt((sigma_1^2 /n_1)+(sigma_2^2/n_2))
T_score<-abs(qt(.025,4407))

bot<-x_bar_diff-T_score*SE
top<-x_bar_diff+T_score*SE

cat("the 95% confidence interval for the difference in mean height between physically active and non physically active is ",bot,"to",top)
```

```
## the 95% confidence interval for the difference in mean height between physically active and non physically active is  1.77628 to 1.78372
```

The null value falls outside this range so we can REJECT the null hypothesis that there is no difference in the mean height between physically active and non physically active subjects.

### Exercise 11

1.

Now, a non-inference task: Determine the number of different options there are in the dataset for the hours_tv_per_school_day there are.


```r
yrbss%>%
  filter(!is.na(hours_tv_per_school_day))%>%
  select(hours_tv_per_school_day)%>%
  unique()
```

```
##    hours_tv_per_school_day
## 1                       5+
## 4                        2
## 5                        3
## 10            do not watch
## 12                      <1
## 14                       4
## 19                       1
```



### Exercise 12

1.

Come up with a research question evaluating the relationship between height or weight and sleep. Formulate the question in a way that it can be answered using a hypothesis test and/or a confidence interval. Report the statistical results, and also provide an explanation in plain language. Be sure to check all assumptions, state your α level, and conclude in context.


Exploring data:


```r
yrbss$hours_tv_per_school_day<-as.factor(yrbss$hours_tv_per_school_day)
yrb_plot<-yrbss%>%
  filter(!is.na(weight),!is.na(hours_tv_per_school_day))
p<-ggplot(yrb_plot, aes(x=hours_tv_per_school_day,y=weight))+
  geom_boxplot()
p
```

![](lab-7_files/figure-latex/unnamed-chunk-19-1.pdf)<!-- --> 


looking at the box plots, it looks like there is no major difference between weight and how much TV one watches. 

I would like to do an ANOVA test on this data:

CHECKS:

Normality:

the sample sizes are large enough that the skew can be overlooked.

Homogeneity of variance:

the IQR for the categories looks similar across groups. 


```r
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

```r
desc<-describeBy(yrbss$weight,yrbss$hours_tv_per_school_day,mat=TRUE)

print(desc,row.names=FALSE)
```

```
##  item       group1 vars    n     mean       sd median  trimmed      mad   min
##     1           <1    1 2021 66.48461 15.70216  63.50 64.68001 13.44718 34.02
##     2            1    1 1667 67.37800 16.37220  64.41 65.69415 14.79635 34.02
##     3            2    1 2548 67.78624 16.42145  64.41 66.06289 14.79635 36.29
##     4            3    1 1995 69.03978 17.35373  65.77 67.15432 14.79635 37.65
##     5            4    1  976 68.90626 18.02983  65.77 67.02081 16.81268 34.93
##     6           5+    1 1430 70.20328 19.25428  66.23 67.80165 16.13069 29.94
##     7 do not watch    1 1671 66.26614 15.75067  63.50 64.52085 13.44718 31.75
##     max  range     skew kurtosis        se
##  163.30 129.28 1.304176 2.665420 0.3492821
##  158.76 124.74 1.137777 1.955114 0.4009952
##  145.15 108.86 1.094115 1.497159 0.3253208
##  160.12 122.47 1.252392 2.329786 0.3885271
##  158.76 123.83 1.166745 1.828749 0.5771208
##  180.99 151.05 1.318850 2.351653 0.5091658
##  158.76 127.01 1.258166 2.409482 0.3853104
```


perform ANOVA test:


```r
aov.out<-aov(weight ~ hours_tv_per_school_day, data=yrbss)

summary(aov.out)
```

```
##                            Df  Sum Sq Mean Sq F value  Pr(>F)    
## hours_tv_per_school_day     6   20160    3360   11.83 2.8e-13 ***
## Residuals               12301 3492964     284                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 1275 observations deleted due to missingness
```


The p-value is very small so it looks like there is some effect, but we cannot yet say which group.

My guess would be that the high mean weight of the 7+ hours of tv group is the culprit. 

NEXT STEPS:

we want to do a t test of each pair, with a Bonferroni adjustment to the alpha.

I will look at the difference in mean between the 5+ group and the 3 hour group (in the middle of the hours watched.)

```r
#T_score<-(x_bar_middle-x_bar_lower) - null / SE (for comparisons)
n_five<-1430
n_three<-1995
x_bar_five<-70.20328
x_bar_three<-67.78624	
#from the residuals
MSE<-284
null<-0
SE<-sqrt((MSE/n_five)+(MSE/n_three))
T_score<-((x_bar_five-x_bar_three)-null)/SE
df=12301
k<-7
K<-(k*(k-1))/2
a<-.05
(a_star<-a/K)
```

```
## [1] 0.002380952
```


use R to get the area under the curve of the t-distribution


```r
p_value<-2*pt(T_score,df=df,lower.tail = FALSE)
```


recall we are using the MODIFIED significance level a*


```r
print(a_star)
```

```
## [1] 0.002380952
```

```r
print(p_value)
```

```
## [1] 3.50588e-05
```
since the p-value is LOWER than the a_star, we can reject the null hypothesis, and say that the difference between the means of 5+ hours of tv and 3 is statistically significant. 
