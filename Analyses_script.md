-   [Analysis of the Data for Isabel](#analysis-of-the-data-for-isabel)
    -   [Get the data](#get-the-data)
    -   [ET data](#et-data)
        -   [prediction](#prediction)
        -   [Learning for the first 10 trials, by
            participant](#learning-for-the-first-10-trials-by-participant)
        -   [Include the questionnaires](#include-the-questionnaires)
        -   [Plot the questionnaires by symptom
            severity](#plot-the-questionnaires-by-symptom-severity)
        -   [Analyze](#analyze)
        -   [Median Split](#median-split)

# Analysis of the Data for Isabel

Source and Initialize packages and functions

## Get the data

``` r
#------------------------------------------------------------------------------#
# get the data from the two versions and merge them
#------------------------------------------------------------------------------#
ds<-list.files("data")

version1<-read.csv(paste0('data/',ds[1]))
version2<-read.csv(paste0('data/',ds[2]))

# since we have two "participants two", delete the second one
# also the number 50 (which is me)
version1<-version1[version1$participant!=50 ,]

version2<-version2[version2$participant!=02  ,]

# before merging, check the variables of interest
vars<-names(version2)[names(version2) %in% names(version1)]

# delete the first that is just a "x'
vars<-vars[-1]

# subset these variables in the two versions
version1<-version1[, vars]
version2<-version2[, vars]

# merge them 
data_all<-rbind(version1, version2)

# how many participants?
participants<-unique(data_all$participant)

# count them by age group
count<-data_all %>%
  group_by(participant, age_group) %>%
  tally()

# select only young adults
data_all<-data_all[data_all$age_group=="YA",]
```

## ET data

### prediction

``` r
#------------------------------------------------------------------------------#
# have a look at the et data- 

# select only participants for which we have the eyetracking  data
sum_et<-data_all %>%
  group_by(participant) %>%
  dplyr::summarise(et = mean(fixation_error, na.rm = T))

# which participants have the et data?
part_incl<-sum_et$participant[!is.na(sum_et$et)]
part_incl
```

    ##  [1]  1  2  3  4  7  9 10 19 20 21 22 23 24 25 26 27 28 30 35 36 37 38

``` r
# subset
all_data_et<-data_all[data_all$participant %in% part_incl,]

# plot prediction - distance between where they look at and the most likely location

# aggregated by trial type
dat_summary_fix_type<- summarySEwithin(all_data_et,
                                       measurevar = "fixation_prediction",
                                       withinvars = c("type" ), 
                                       betweenvars = "age_group",
                                       idvar = "participant", 
                                       na.rm = T)
```

    ## Automatically converting the following non-factors to factors: age_group, type

``` r
# plot
ggplot(all_data_et %>%
         group_by(participant,
                  type)%>%
         dplyr::summarise(fixation_prediction=mean(fixation_prediction, na.rm=T)), 
       aes(x = type, y = fixation_prediction, colour = type ))+
  geom_point(alpha = 0.10, colour = "black" )+
  geom_line( aes(type, fixation_prediction,group = participant),
             size=1, alpha=0.05, stat="summary" , colour = 'black')+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  geom_point(stat="summary", size = 5, data = dat_summary_fix_type)+
  
  geom_errorbar(aes( y = fixation_prediction, ymin = fixation_prediction - ci, 
                     ymax = fixation_prediction + ci),
                width = 0.40, size = 1,data=dat_summary_fix_type)+
  
  facet_wrap(.~age_group)+
  custom_param()+
  theme_classic()+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) 
```

    ## `summarise()` has grouped output by 'participant'. You can override using the
    ## `.groups` argument.

    ## No summary function supplied, defaulting to `mean_se()`
    ## No summary function supplied, defaulting to `mean_se()`

![](Analyses_script_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Learning for the first 10 trials, by participant

``` r
# look at learning only from the first 10 trials
# by subject
ggplot(all_data_et[all_data_et$trial_n_block<10 & all_data_et$type!='singletons',], # we do not want the singletons
       aes(x = trial_n_block, y = fixation_prediction,
           #colour = age_group, 
           group = 1))+
  stat_summary(fun.y="mean",geom="line", size = 1.5)+
  #facet_wrap(participant~age_group)+
  facet_wrap(participant~.)+
  scale_x_continuous(breaks=seq(1, 100, 19))+
  scale_color_manual(values = c(c( "#AA4499" ,"#44AA99","#332288")))+
  theme_classic()+
  ggtitle("Fixation prediction and age group")
```

![](Analyses_script_files/figure-markdown_github/unnamed-chunk-4-1.png)
\### Learning for the first 10 trials: across participants

``` r
ggplot(all_data_et[all_data_et$trial_n_block<10,], aes( x=trial_n_block, y=fixation_prediction))+
  geom_line(stat="smooth",method = "lm", formula=y~x, alpha=0.5, se=F)+
  aes(colour = factor(participant))+
  geom_smooth(method="lm",formula=y~x, se=T, colour = "black" )+
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  facet_wrap(.~age_group)+
  #ggtitle("Experiment 2")+
  theme(legend.position = "none")
```

![](Analyses_script_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Include the questionnaires

``` r
# add the questionnaires
quest<-read.csv("Questionnaires.csv", sep = ";")

# check distribution of BDI and SHAPS
hist(quest$BDI_score)
```

![](Analyses_script_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
hist(quest$SHAPS_score)
```

![](Analyses_script_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
# attache the values to each participant
# relevant variables
rel_var<-c("BDI_score", "BDI_class", "SHAPS_score", "SHAPS_class")

# initialize them in the dataframe
for (var in rel_var){
  
  all_data_et[[var]]<-NA
  
}


for ( n in 1:nrow(all_data_et)){
  
  # get the scores for current participant
  curr_part<-quest[quest$ID == all_data_et$participant[n],]
  
  # if we have questionnaires data for that participant
  if (nrow(curr_part)>0 ){
  for (var in rel_var){
    
    all_data_et[[var]][n]<-curr_part[[var]]
    
  }
  }

}
```

### Plot the questionnaires by symptom severity

``` r
ggplot(all_data_et[all_data_et$trial_n_block<10,], aes( x=trial_n_block, y=fixation_prediction))+
  geom_line(stat="smooth",method = "lm", formula=y~x, alpha=0.5, se=F)+
  aes(colour = factor(participant))+
  geom_smooth(method="lm",formula=y~x, se=T, colour = "black" )+
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  facet_wrap(.~BDI_class)+
  #ggtitle("Experiment 2")+
  theme(legend.position = "none")
```

![](Analyses_script_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Analyze

``` r
# -----------------------------------------------------------------------------#
# center trial_n_block
all_data_et$trial_n_block_c<-scale(all_data_et$trial_n_block, center = T)

# analyze
mod_BDI<-lmer(fixation_prediction~BDI_score*trial_n_block_c +
                (1+trial_n_block_c|participant), 
              data=all_data_et[all_data_et$trial_n_block<10,])

model_parameters(mod_BDI)
```

    ## Package 'merDeriv' needs to be installed to compute confidence intervals
    ##   for random effect parameters.

    ## # Fixed Effects
    ## 
    ## Parameter                   | Coefficient |    SE |           95% CI |    t(308) |     p
    ## ----------------------------------------------------------------------------------------
    ## (Intercept)                 |      -42.97 | 45.25 | [-132.00, 46.06] |     -0.95 | 0.343
    ## BDI score                   |       -0.24 |  3.79 | [  -7.69,  7.21] |     -0.06 | 0.950
    ## trial n block c             |      -61.07 | 27.74 | [-115.66, -6.49] |     -2.20 | 0.028
    ## BDI score × trial n block c |       -0.02 |  2.36 | [  -4.67,  4.63] | -9.51e-03 | 0.992
    ## 
    ## # Random Effects
    ## 
    ## Parameter                                    | Coefficient
    ## ----------------------------------------------------------
    ## SD (Intercept: participant)                  |       60.07
    ## SD (trial_n_block_c: participant)            |       29.16
    ## Cor (Intercept~trial_n_block_c: participant) |        1.00
    ## SD (Residual)                                |       41.54

    ## 
    ## Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    ##   using a Wald t-distribution approximation. Uncertainty intervals for
    ##   random effect variances computed using a Wald z-distribution
    ##   approximation.

``` r
# mod shaps
mod_SHAPS<-lmer(fixation_prediction~SHAPS_score*trial_n_block_c +
                (1+trial_n_block_c|participant), 
              data=all_data_et[all_data_et$trial_n_block<10,])
```

    ## boundary (singular) fit: see help('isSingular')

``` r
model_parameters(mod_SHAPS)
```

    ## Package 'merDeriv' needs to be installed to compute confidence intervals
    ##   for random effect parameters.

    ## # Fixed Effects
    ## 
    ## Parameter                     | Coefficient |    SE |            95% CI | t(308) |     p
    ## ----------------------------------------------------------------------------------------
    ## (Intercept)                   |      -54.48 | 32.57 | [-118.56,   9.60] |  -1.67 | 0.095
    ## SHAPS score                   |        8.55 | 23.33 | [ -37.35,  54.44] |   0.37 | 0.714
    ## trial n block c               |      -64.65 | 19.94 | [-103.89, -25.40] |  -3.24 | 0.001
    ## SHAPS score × trial n block c |        2.41 | 14.53 | [ -26.19,  31.01] |   0.17 | 0.868
    ## 
    ## # Random Effects
    ## 
    ## Parameter                                    | Coefficient
    ## ----------------------------------------------------------
    ## SD (Intercept: participant)                  |       59.64
    ## SD (trial_n_block_c: participant)            |       29.76
    ## Cor (Intercept~trial_n_block_c: participant) |        1.00
    ## SD (Residual)                                |       41.54

    ## 
    ## Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    ##   using a Wald t-distribution approximation. Uncertainty intervals for
    ##   random effect variances computed using a Wald z-distribution
    ##   approximation.

``` r
# categorical
mod_BIDS_class<-lmer(fixation_prediction~BDI_class*trial_n_block_c +
                  (1|participant), 
                data=all_data_et[all_data_et$trial_n_block<10,])

model_parameters(mod_BIDS_class)
```

    ## Package 'merDeriv' needs to be installed to compute confidence intervals
    ##   for random effect parameters.

    ## # Fixed Effects
    ## 
    ## Parameter                              | Coefficient |    SE |            95% CI | t(308) |     p
    ## -------------------------------------------------------------------------------------------------
    ## (Intercept)                            |      -71.62 | 53.25 | [-176.40,  33.16] |  -1.34 | 0.180
    ## BDI class [moderate]                   |       50.33 | 95.00 | [-136.60, 237.26] |   0.53 | 0.597
    ## BDI class [no]                         |       24.63 | 59.87 | [ -93.16, 142.43] |   0.41 | 0.681
    ## trial n block c                        |      -72.11 | 34.90 | [-140.79,  -3.44] |  -2.07 | 0.040
    ## BDI class [moderate] × trial n block c |       20.05 | 62.99 | [-103.90, 144.01] |   0.32 | 0.750
    ## BDI class [no] × trial n block c       |        8.86 | 39.19 | [ -68.26,  85.99] |   0.23 | 0.821
    ## 
    ## # Random Effects
    ## 
    ## Parameter                   | Coefficient
    ## -----------------------------------------
    ## SD (Intercept: participant) |       16.08
    ## SD (Residual)               |       41.83

    ## 
    ## Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    ##   using a Wald t-distribution approximation. Uncertainty intervals for
    ##   random effect variances computed using a Wald z-distribution
    ##   approximation.

``` r
(anova(mod_BIDS_class))
```

    ## Analysis of Variance Table
    ##                           npar  Sum Sq Mean Sq F value
    ## BDI_class                    2  2488.8  1244.4  0.7111
    ## trial_n_block_c              1 31021.7 31021.7 17.7277
    ## BDI_class:trial_n_block_c    2   188.4    94.2  0.0538

``` r
mod_SHAPS_class<-lmer(fixation_prediction~SHAPS_class*trial_n_block_c +
                       (1|participant), 
                     data=all_data_et[all_data_et$trial_n_block<10,])

model_parameters(mod_SHAPS_class)
```

    ## Package 'merDeriv' needs to be installed to compute confidence intervals
    ##   for random effect parameters.

    ## # Fixed Effects
    ## 
    ## Parameter                          | Coefficient |    SE |            95% CI | t(310) |     p
    ## ---------------------------------------------------------------------------------------------
    ## (Intercept)                        |      -83.92 | 59.19 | [-200.39,  32.55] |  -1.42 | 0.157
    ## SHAPS class [no]                   |       40.80 | 64.34 | [ -85.80, 167.40] |   0.63 | 0.526
    ## trial n block c                    |      -89.54 | 39.11 | [-166.48, -12.59] |  -2.29 | 0.023
    ## SHAPS class [no] × trial n block c |       30.14 | 42.42 | [ -53.33, 113.61] |   0.71 | 0.478
    ## 
    ## # Random Effects
    ## 
    ## Parameter                   | Coefficient
    ## -----------------------------------------
    ## SD (Intercept: participant) |       16.56
    ## SD (Residual)               |       41.72

    ## 
    ## Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    ##   using a Wald t-distribution approximation. Uncertainty intervals for
    ##   random effect variances computed using a Wald z-distribution
    ##   approximation.

``` r
anova(mod_SHAPS_class)
```

    ## Analysis of Variance Table
    ##                             npar  Sum Sq Mean Sq F value
    ## SHAPS_class                    1   121.0   121.0  0.0695
    ## trial_n_block_c                1 30966.8 30966.8 17.7893
    ## SHAPS_class:trial_n_block_c    1   878.7   878.7  0.5048

### Median Split
