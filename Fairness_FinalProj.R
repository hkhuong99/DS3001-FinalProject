## Fairness {.tabset}

Since we broke the data up into female and male subsets to see what factors affected each, the only other demographic variable we have is age. Want to see if this model is fair for all age groups represented. Broke the data into 3 subsets by the US Census Age Brackets:
  
  1 represents patients 44 years old and younger

2 represents patients between 45 and 64 years old

3 represents patients 65 and older

```{r}

#Since broke data up into female and male subsets to see what factors affected each, the only other demographic variable we have is age. Want to see if this model is fair for all age groups represented.

#first we are going to equality of odds or "proportional parity" and equal opportunity "equal odds" as defined by this package, but we need create a new data frame that includes our set of predicted values and the percentage values associated with each outcome. We will add this to our test set.  


for (i in 1:82) 
{
  if (male_test[i,1] >= 65) {
    male_test[i,1] = 3 #'65 and over'
  } else if ( male_test[i,1] >= 45) {
    male_test[i,1] = 2 #'45 and over'
  }  else {
    male_test[i,1]=1 #'44 and under'
  }
}

for (i in 1:57) 
{
  if (female_test[i,1] >= 65) {
    female_test[i,1] = 3 #'65 and over'
  } else if ( female_test[i,1] >= 45) {
    female_test[i,1] = 2 #'45 and over'
  }  else {
    female_test[i,1]=1 #'44 and under'
  }
}
#view(table(male_test$age))

#Test,Predicted Class and the Probabilities all in one dataframe

```

### Proportional Parity

Proportional parity is achieved if the proportion of positive predictions in the subgroups are close to each other.

Male Patients
```{r}
#first we are going to equality of odds or "proportional parity" and equal opportunity "equal odds" as defined by this package, but we need create a new data frame that includes our set of predicted values and the percentage values associated with each outcome. We will add this to our test set.  
library(fairness)

#table(male_test$age)

male_prob <- predict(male_mdl,male_test, type = 'prob')
#male_prob
#male_predict
#Test,Predicted Class and the Probabilities all in one dataframe
fair_eval_male <- cbind(male_test, predicted = male_predict, probability = male_prob)
#view(fair_eval_male)

#head(fair_eval_male)

dpp <- prop_parity(data = fair_eval_male, 
                   group="age",#protected class
                   probs = "probability.1",
                   preds = "predicted",
                   cutoff = .5,#threshold,
                   base = 1
)



#dpp$Metric #We would want these to be 1 across the board, but it's looks like being female appears to be favored, but very little. 
dev.off()
#The below plots help to show this story a bit more.
ddp_metric_plot <- dpp$Metric_plot
ddp_metric_plot

```
As can be seen from the proportional parity graphic, the male model favors younger patients. Proportional parity is achieved if the proportion of positive predictions in the subgroups are close to each other. 
```{r}
prob_plot <- dpp$Probability_plot #as we can see there's some slight advantages to being female both before the 50% threshold but about the same after the cutoff.

prob_plot
```
Looking at the probability plot, younger patients are still favored for both positive and negative predictions


Female Patients

```{r}
#table(female_test$age)

female_prob <- predict(female_mdl,female_test, type = 'prob')
#female_prob
#female_predict
#Test,Predicted Class and the Probabilities all in one dataframe
fair_eval_female <- cbind(female_test, predicted = female_predict, probability = female_prob)
#view(fair_eval_female)

#head(fair_eval_female)

dppf <- prop_parity(data = fair_eval_female, 
                    group="age",#protected class
                    probs = "probability.1",
                    preds = "predicted",
                    cutoff = .5,#threshold,
                    base = 1
)
dppf$Metric #We would want these to be 1 across the board, but it's looks like being female appears to be favored, but very little. 
dev.off()
#The below plots help to show this story a bit more.
ddpf_metric_plot <- dppf$Metric_plot
ddpf_metric_plot

```
Looking at the Proportional Parity for the female dataset, younger female patients are more likely to be classified correctly. 

```{r}
prob_plotf <- dppf$Probability_plot
prob_plotf
```


### Equal Odds

Equalized odds are achieved if the sensitivities in the subgroups are close to each other. The group-specific sensitivities indicate the number of the true positives divided by the total number of positives in that group.

Male Patients

```{r}
#We can also look at equal odds measures

eqo_loan <- equal_odds(data = fair_eval_male, 
                       outcome = "target", 
                       group   = "age",
                       probs   = "probability.1", 
                       preds   = "predicted",
                       cutoff = 0.5,
                       base = 1
)

eqo_loan$Metric #This is interesting because here it seems the roles are slightly reversed. 

eqo_loan <- ggplotly(eqo_loan$Metric_plot)

eqo_loan

```

When we looked a the equalized odds, older male patients (subgroup 3) are favored.

Female Patients

```{r}
eqo_f <- equal_odds(data = fair_eval_female, 
                    outcome = "target", 
                    group   = "age",
                    probs   = "probability.1", 
                    preds   = "predicted",
                    cutoff = 0.5,
                    base = 2
)

eqo_f$Metric #This is interesting because here it seems the roles are slightly reversed. 

eqo_f <- ggplotly(eqo_f$Metric_plot)

eqo_f
```

Looking, at the equal odds of the female dataset, older women are more likely to be classified correctly. However, this model doesn't work well for this data set because there are no True Positive classifications for the youngest age bracket.

### Predictive rate parity

Predictive rate parity is achieved if the precisions (or positive predictive values) in the subgroups are close to each other.

Male Patients

```{r}
prp <- pred_rate_parity(data = fair_eval_male, 
           outcome = "target", 
           group   = "age",
           probs   = "probability.1", 
           preds   = "predicted",
           cutoff = 0.50,
           base = 1
           )

prp$Metric

#DT::datatable(prp$Metric, options = list(pageLength = 10))

prp$Metric_plot # As we can see in comparison to those of Japanese decent white and black individuals are being classified much less accurately when it comes to the positive outcome of getting a loan. Keeping in mind this is made up data.  

```

Looking at the predictive rate parity, we see that again, younger patients are favored by this model.

Female Patients

```{r}
prpf <- pred_rate_parity(data = fair_eval_female, 
           outcome = "target", 
           group   = "age",
           probs   = "probability.1", 
           preds   = "predicted",
           cutoff = 0.50,
           base = 2
           )
#prpf$Metric
#DT::datatable(prpf$Metric, options = list(pageLength = 10))
prpf$Metric_plot
```
A similar pattern appears for predictive rate parity again, because there are no true positive classifications for the youngest age group. 

### False Negative Parity

False negative rate parity is achieved if the false negative rates (the ratio between the number of false negatives and the total number of positives) in the subgroups are close to each other.

Male Patients


```{r}
fnr<-fnr_parity(data = fair_eval_male, 
           outcome = "target", 
           group   = "age",
           probs   = "probability.1", 
           preds   = "predicted",
           cutoff = 0.50,
           base = 1
           )
ggplotly(fnr$Metric_plot)
```
Since we are looking into factors that predict heart disease, false negatives are very important. Men in the youngest age bracket are most likely to have a false negative prediction. This means that this model would be unsuitable to catch heart disease early in younger patients.

Female Patients

```{r}
fnrf<-fnr_parity(data = fair_eval_female, 
           outcome = "target", 
           group   = "age",
           probs   = "probability.1", 
           preds   = "predicted",
           cutoff = 0.50,
           base = 2
           )
fnrf$Metric
ggplotly(fnrf$Metric_plot)
```
Looking at this bar chart, we see that women in the middle age bracket are most likely to have a false negative classification. But this is due to a small dataset, more data is needed to accurately determine the fairness of this model. 
