---
layout: post
title:  Speed dating experiment [non-finance]
comments: false
---




I came across the paper about gender difference in mate selection. The original paper is here [Gender Differences in mate selection: Evidence from a Speed Dating Experiment](http://faculty.chicagobooth.edu/emir.kamenica/documents/genderDifferences.pdf).
The data can be downloaded from [here](http://www.stat.columbia.edu/~gelman/arm/examples/speed.dating/).

The main conclusion : attractiveness is the most important attribute in finding a romantic partner from both the male and female perspectives given speed dating experiment.



{% highlight r %}
#*****************************************************************
# Read data
#*****************************************************************
data = read.csv("Speed Dating Data.csv")

# of observations, number of variables
print(cbind(num.obs = nrow(data), num.var = ncol(data)))
{% endhighlight %}



| num.obs| num.var|
|-------:|-------:|
|    8378|     195|
    

Meaning of some of variables are below:
  1. gender  : female = 0, male = 1
  2. career_c : encoded career
  3. samerace : participant and the partner were the same race. 1= yes, 0=no
  4. race : race of person
  5. dec : 1=yes, 0=no


{% highlight r %}
# convert to factors some of variables
data$gender    = as.factor(data$gender)
data$career_c  = as.factor(data$career_c)
data$samerace  = as.factor(data$samerace)
data$race      = as.factor(data$race)
data$dec       = as.factor(data$dec)
data$date      = as.factor(data$date)

#*****************************************************************
# Distribution of career
#*****************************************************************
data_career <- data %>% filter(!is.na(career_c)) %>% select(iid, gender, career_c)
data_career <- unique(data_career, by = iid)

print(head(data_career))
{% endhighlight %}



|   | iid|gender |career_c |
|:--|---:|:------|:--------|
|1  |   4|0      |1        |
|11 |   5|0      |1        |
|21 |   6|0      |1        |
|31 |   7|0      |1        |
|41 |   8|0      |6        |
|51 |   9|0      |9        |
    




{% highlight r %}
print(tail(data_career))
{% endhighlight %}



|     | iid|gender |career_c |
|:----|---:|:------|:--------|
|8109 | 547|1      |7        |
|8131 | 548|1      |7        |
|8153 | 549|1      |7        |
|8175 | 550|1      |7        |
|8197 | 551|1      |7        |
|8219 | 552|1      |15       |
    




{% highlight r %}
# from speed dating data key file
career_label <- c("Lawyer",
                  "Academic/Research",
                  "Psychologist",  
                  "Doctor/Medicine",
                  "Engineer",
                  "Creative Arts/Entertainment", 
                  "Banking/Business",
                  "Real Estate",
                  "International Affairs", 
                  "Undecided",
                  "Social Work",
                  "Speech Pathology",
                  "Politics", 
                  "Sports/Athletics",
                  "Other",
                  "Journalism", 
                  "Architecture")

#*****************************************************************
# Plot career
#*****************************************************************
ggplot(data = data_career) +
  geom_bar(aes(career_c, fill=gender)) + 
  scale_x_discrete(label = career_label) + coord_flip() + 
  labs(title = "Distribution of profession", x = "", y = "Count") + 
  scale_fill_discrete("", labels = c("Female", "Male")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  qis.plot.add.copyright(ggplot = T)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2018-08-01-speed-dating/plot-4-1.png)

{% highlight r %}
#*****************************************************************
# Plot age
#*****************************************************************
data_age <- data %>% filter(!is.na(age)) %>% select(iid, gender, age)
data_age <- filter(data_age, age < max(age))
data_age <- unique(data_age, by = iid)


ggplot(data = data_age, aes(x = age,fill = gender)) + coord_flip() + 
  geom_histogram(data = subset(data_age, gender == "0"), binwidth = 2, color = "white") +  
  geom_histogram(data = subset(data_age, gender == "1"), 
                 aes(y = ..count.. * (-1)), binwidth = 2, color = "white") + 
  scale_y_continuous(breaks = seq(-70, 70, 10), labels = abs(seq(-70, 70, 10)))+ 
  scale_x_continuous(breaks = seq(10, 45, 5), labels = seq(10, 45,5)) + 
  labs(title = "Distribution of age", x = "Age", y = "Count") + 
  scale_fill_discrete("", labels = c("Female", "Male")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  qis.plot.add.copyright(ggplot = T)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2018-08-01-speed-dating/plot-4-2.png)

{% highlight r %}
#*****************************************************************
# Plot race
#*****************************************************************
data_race <- data %>% filter(!is.na(race)) %>% select(iid, gender, race)
data_race <- unique(data_race, by = iid)

race_label <- c("Black/African American", "European/Caucasian American", 
                "Latino/Hispanic American", "Asian/Asian American", 
                "Naitive American", "Other")

ggplot(data = data_race) + 
  geom_bar(aes(x = gender,fill = race), position = "fill") + 
  labs(title = "Distribution of Race", x = "", y = "Frequency") +
  scale_fill_discrete("", labels = race_label) + scale_y_continuous(labels = percent) +
  scale_x_discrete(labels=c("0" = "Male", "1" = "Female")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  qis.plot.add.copyright(ggplot = T)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2018-08-01-speed-dating/plot-4-3.png)

The survey asked each participant to distribute scores to the six attributes of 

1. attractiveness
2. sincerity
3. intelligence
4. fun
5. ambition
6. shared interests

Students were asked to do so from both their **own perspective** and the **perspective of someone** of the opposite gender.


{% highlight r %}
# Normalize scores
data <- data %>% 
  # What you look for in the opposite sex
  mutate(sum1_1 = attr1_1 + sinc1_1 + intel1_1 + amb1_1 + shar1_1) %>%
  mutate(attr1_1 = (attr1_1/sum1_1)*100) %>% 
  mutate(sinc1_1 = (sinc1_1/sum1_1)*100) %>% 
  mutate(intel1_1 = (intel1_1/sum1_1)*100) %>% 
  mutate(amb1_1 = (amb1_1/sum1_1)*100) %>% 
  mutate(shar1_1 = (shar1_1/sum1_1)*100) %>%
  # What do you think the opposite sex looks for in a date?
  mutate(sum2_1 = attr2_1 + sinc2_1 + intel2_1 + amb2_1 + shar2_1) %>%
  mutate(attr2_1 = (attr2_1/sum2_1)*100) %>% 
  mutate(sinc2_1 = (sinc2_1/sum2_1)*100) %>% 
  mutate(intel2_1 = (intel2_1/sum2_1)*100) %>% 
  mutate(amb2_1 = (amb2_1/sum2_1)*100) %>% 
  mutate(shar2_1 = (shar2_1/sum2_1)*100) 

# remove NA's
data_features <- data %>% 
                filter(!is.na(sum1_1)) %>% 
                filter(!is.na(sum2_1)) %>% 
                select(iid, gender, attr1_1:shar2_1)

data_features <- unique(data_features, by = idd) 

# separate data to men and women
men   <- filter(data_features, gender =="1")
women <- filter(data_features, gender =="0") 


row_label    <- c("Self", "Majority")
column_label <- c("Attractive", "Sincere", "Intelligent", 
                  "Fun", "Ambitious", "Shared Interests")


#*****************************************************************
# What mens think
#*****************************************************************
radar_men <- as.data.frame(matrix(0, nrow = 2, ncol = 6))
colnames(radar_men) <- column_label
rownames(radar_men) <- row_label

for (i in (1:nrow(radar_men))) {
  for(j in c(1:ncol(radar_men))) {
    if(i == 1) {
      radar_men[i, j] <- mean(men[ , 2 + j])
    }
    if(i == 2){
      radar_men[i,j] <- mean(women[ , 14 + j])
    }  
  }
}

radar_men = rbind(rep(40, 5) , rep(0, 5) , radar_men)
radarchart(radar_men, plwd  = 3 , plty = 1, vlcex = 0.8,  title = "What males find the most important in female", pcol = c("blue","red"))
legend(x = 1, y = 1.2, legend = c("Male perspective", "Female perspective"), col = c("blue","red"),
       bty = "n", pch = 20, text.col = "black", cex = 0.8, pt.cex = 2)
qis.plot.add.copyright()
{% endhighlight %}

![plot of chunk plot-5](/public/images/2018-08-01-speed-dating/plot-5-1.png)

{% highlight r %}
#*****************************************************************
# What women think
#*****************************************************************
radar_women <- as.data.frame(matrix(0, nrow = 2, ncol = 6))
colnames(radar_women) <- column_label
rownames(radar_women) <- row_label

for (i in (1:nrow(radar_women))) {
  for(j in c(1:ncol(radar_women))) {
    if(i == 1) {
      radar_women[i,j] <- mean(women[ , 2 + j])
    }
    if( i == 2) {
      radar_women[i,j] <- mean(men[ , 14 + j])
    }  
  }
}

layout(1)
radar_women = rbind(rep(40, 5) , rep(0, 5) , radar_women)
radarchart(radar_women, plwd  = 3 , plty = 1, vlcex = 0.8,  title = "What women find the most important in men", pcol = c("blue","red"))
legend(x = 1, y = 1.2, legend = c("Female perspective", "Male perspective"), col = c("blue","red"),
       bty = "n", pch = 20, text.col = "black", cex = 0.8, pt.cex = 2)
qis.plot.add.copyright()
{% endhighlight %}

![plot of chunk plot-5](/public/images/2018-08-01-speed-dating/plot-5-2.png)

It is interesting that both men and women overestimated the weight of importance of attractiveness.


{% highlight r %}
# dec_o : decision of partner the night of event
# attr_o : rating by partner the night of the event

regression <- data %>% group_by(iid) %>% 
  summarize(gender = mean(as.numeric(gender)), 
            attr = mean(attr_o, na.rm = TRUE), 
            sinc = mean(sinc_o, na.rm = TRUE), 
            intel = mean(intel_o, na.rm = TRUE), 
            fun = mean(fun_o, na.rm = TRUE), 
            amb = mean(amb_o, na.rm = TRUE), 
            shar = mean(shar_o, na.rm = TRUE), 
            selected = sum(dec_o, na.rm = TRUE), n = n())  %>% 
  mutate(selected_perc = selected/n*100) %>% 
  filter(selected_perc > 0)

# Summary of one of the person (men)
print(as.data.frame(regression[regression$iid ==1,]))
{% endhighlight %}



| iid| gender|               attr|               sinc| intel|                fun| amb|               shar| selected|  n| selected_perc|
|---:|------:|------------------:|------------------:|-----:|------------------:|---:|------------------:|--------:|--:|-------------:|
|   1|      1| 6.7000000000000002| 7.4000000000000004|     8| 7.2000000000000002|   8| 7.0999999999999996|        5| 10|            50|
    




{% highlight r %}
# Summary of one of the person (women)
print(as.data.frame(regression[regression$iid ==400,]))
{% endhighlight %}



| iid| gender|               attr|              sinc|              intel|                fun|                amb|              shar| selected|  n|      selected_perc|
|---:|------:|------------------:|-----------------:|------------------:|------------------:|------------------:|-----------------:|--------:|--:|------------------:|
| 400|      2| 7.1111111111111107| 7.666666666666667| 8.0555555555555554| 7.1764705882352944| 8.3529411764705888| 6.916666666666667|       14| 18| 77.777777777777786|
    




{% highlight r %}
#*****************************************************************
# Modeling men scores with linear regression
#*****************************************************************
regression_men <- regression %>% filter(gender == 1)

pairs.panels(regression_men[ , c(3:8, 11)], 
             method = "pearson", scale = T, ellipses = F, 
             labels = c("Attractive", "Sincere", "Intelligent", "Fun", 
                        "Ambitious", "Interests", "Selected"), 
             cex.labels = 1.3, hist.col = "lightblue")
qis.plot.add.copyright()
{% endhighlight %}

![plot of chunk plot-6](/public/images/2018-08-01-speed-dating/plot-6-1.png)

Attractiveness has the strongest positive correlation with percent of selections. Sincere is the lowest.


{% highlight r %}
print(summary( lm(selected_perc ~ attr + sinc + intel + fun + amb + shar, data = regression_men)))
{% endhighlight %}



<pre>

Call:
lm(formula = selected_perc ~ attr + sinc + intel + fun + amb + 
    shar, data = regression_men)

Residuals:
              Min                1Q            Median                3Q 
-41.3490445528805  -9.5493705228206  -0.8601138785258   9.2888733748064 
              Max 
 48.3470134709650 

Coefficients:
                      Estimate         Std. Error  t value   Pr(>|t|)    
(Intercept) -66.47210499476427  12.66170467182578 -5.24985 3.1409e-07 ***
attr         13.88174648410581   1.09001240649962 12.73540 < 2.22e-16 ***
sinc         -4.88689944560496   2.08209702092482 -2.34710 0.01966197 *  
intel         2.41044850944657   2.44073805900448  0.98759 0.32426118    
fun           5.72958800287567   1.58448355561772  3.61606 0.00035848 ***
amb          -0.11954431516274   1.84217093254883 -0.06489 0.94830835    
shar          1.09776553136922   1.55123217510383  0.70767 0.47977482    
---
Signif. codes:    0 '***' 0.001 '**' 0.01 '*' 0.050000000000000003 '.'
  0.10000000000000001 ' ' 1

Residual standard error: 13.862518614066 on 263 degrees of freedom
Multiple R-squared:  0.65522940003058,	Adjusted R-squared:  0.64736391105789 
F-statistic: 83.304344117184 on 6 and 263 DF,  p-value: < 2.22044604925e-16

</pre>
    

 The most statistically significant variables were attractiveness


{% highlight r %}
#*****************************************************************
# Modeling women scores with linear regression
#*****************************************************************
regression_women <- regression %>% filter(gender != 1)

pairs.panels(regression_women[ , c(3:8, 11)], 
             method = "pearson", scale = TRUE, ellipses = FALSE, 
             labels = c("Attractive", "Sincere", "Intelligent", "Fun", 
                        "Ambitious", "Interests", "Selected"), 
             cex.labels = 1.3, hist.col = "pink")
qis.plot.add.copyright()
{% endhighlight %}

![plot of chunk plot-8](/public/images/2018-08-01-speed-dating/plot-8-1.png)

Attractiveness has the strongest positive correlation with percent of selections. Sincere is the lowest.


{% highlight r %}
print(summary( lm(selected_perc ~ attr + sinc + intel + fun + amb + shar, data = regression_women)))
{% endhighlight %}



<pre>

Call:
lm(formula = selected_perc ~ attr + sinc + intel + fun + amb + 
    shar, data = regression_women)

Residuals:
              Min                1Q            Median                3Q 
-47.8211907508083  -9.1855533951657   0.1508009588322   7.6668049975306 
              Max 
 40.2615785838117 

Coefficients:
                     Estimate        Std. Error  t value   Pr(>|t|)    
(Intercept) -55.4128056513920   9.9891003213280 -5.54733 7.3238e-08 ***
attr          9.6619448214769   1.0682133951564  9.04496 < 2.22e-16 ***
sinc         -1.7322741112715   1.5257874290277 -1.13533  0.2573152    
intel         3.0996039832806   2.1701777016757  1.42827  0.1544518    
fun           3.1446969462560   1.4506219044988  2.16783  0.0311083 *  
amb          -2.8190965039710   1.5701305517608 -1.79545  0.0737795 .  
shar          4.5933776134819   1.4363795763081  3.19789  0.0015615 ** 
---
Signif. codes:    0 '***' 0.001 '**' 0.01 '*' 0.050000000000000003 '.'
  0.10000000000000001 ' ' 1

Residual standard error: 13.634346805719 on 252 degrees of freedom
Multiple R-squared:  0.60089507526215,	Adjusted R-squared:  0.5913925770541 
F-statistic: 63.235484196511 on 6 and 252 DF,  p-value: < 2.22044604925e-16

</pre>
    

The most statistically significant variable was attractiveness


{% highlight r %}
#*****************************************************************
# Decision Trees
#*****************************************************************
men_tree <- data %>% filter(gender == 1)
men_fit <- rpart(dec_o ~ attr_o + sinc_o + intel_o + fun_o + amb_o + shar_o, data = men_tree, method = "class")
rpart.plot(men_fit)
{% endhighlight %}

![plot of chunk plot-10](/public/images/2018-08-01-speed-dating/plot-10-1.png)

Couple things about men's partitioning :

 1. if a male average attractiveness rating > 6.8 and their average shared interest rating > 6.5, he had a **73%** chance of being selected by his partner to have a second date.
 2. if a male average attractiveness rating < 6.8, he had a **20%** chance of being selected by his partner to have a second date.


{% highlight r %}
women_tree <- data %>% filter(gender != 1)
women_fit <- rpart(dec_o ~ attr_o + sinc_o + intel_o + fun_o + amb_o + shar_o, data = women_tree, method = "class")
rpart.plot(women_fit)
{% endhighlight %}

![plot of chunk plot-11](/public/images/2018-08-01-speed-dating/plot-11-1.png)

Couple things about women's partitioning :

 1. if a female average attractiveness rating > 6.2 and their average fun rating > 6.8, she had a **79%** chance of being selected by her partner to have a second date.
 2. if a female average attractiveness rating < 6.2, she had a **25%** chance of being selected by her partner to have a second date.


{% highlight r %}
#*****************************************************************
# Predict
#*****************************************************************
shuffled_men <- sample_n(men_tree, nrow(men_tree))
split_men <- 0.8*nrow(shuffled_men)
train_men <- shuffled_men[1:split_men, ]
test_men <- shuffled_men[(split_men + 1) : nrow(shuffled_men), ]

men_fit_test <- rpart(dec_o ~ attr_o + sinc_o + intel_o + fun_o + amb_o + shar_o, data = train_men, method = "class")
tree_predictions_men <- predict(men_fit_test, test_men, type = "class")

# accuracy of prediction
print(mean(tree_predictions_men == test_men$dec_o))
{% endhighlight %}



0.750596658711217
    




{% highlight r %}
shuffled_women <- sample_n(women_tree, nrow(women_tree))
split_women <- 0.8*nrow(shuffled_women)
train_women <- shuffled_women[1:split_women, ]
test_women <- shuffled_women[(split_women + 1) : nrow(shuffled_women), ]

women_fit_test <-rpart(dec_o ~ attr_o + sinc_o + intel_o + fun_o + amb_o + shar_o, data = train_women, method = "class")
tree_predictions_women <- predict(women_fit_test, test_women, type = "class")

# accuracy of prediction
print(mean(tree_predictions_women == test_women$dec_o))
{% endhighlight %}



0.740430622009569
    


*(this report was produced on: 2018-09-17)*
