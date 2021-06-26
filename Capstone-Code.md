Capstone Project Code
================

``` r
#loading libraries necessary for project
library(nflfastR)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(readr)
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(tidyr)
library(gt)

#loading in the data for 2015-2020 seasons 
seasons<- c(seq(2015, 2020))
play_by_play <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})
NFL_Coaches_WL <- read_csv("NFL Coaches.csv")
```

    ## Warning: Duplicated column names deduplicated: 'W-L%' => 'W-L%_1' [15]

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   Coach = col_character(),
    ##   `Yr-Yr` = col_character()
    ## )
    ## i Use `spec()` for the full column specifications.

``` r
#trimming the data set down to plays only on fourth down and only include run, punt, pass, or field goal plays
#plays selected will avoid penalties, timeouts, time keeping information and other non relevant plays
play_types_wanted <-c('field_goal', 'pass', 'punt', 'run')
#selecting plays only on fourth down and in wanted plays
fourth_down_plays<-subset(play_by_play, play_by_play$down == 4)
fourth_down_plays<- subset(fourth_down_plays, fourth_down_plays$play_type %in% play_types_wanted)

#removing columns irrelevant to analysis
fourth_down_pred_table <- fourth_down_plays
fourth_down_pred_table <- fourth_down_plays[, -c(1,2,3,4,5,11,13,14,18,19,20, 22, 24, 25, 28, 31:48, 50:53, 56, 57, 61:63, 71:76, 81:92, 95:125, 128:154, 156:328, 330:333, 336:370)]
Coaches_WL<-NFL_Coaches_WL[,c(2,9)]

#Merging the Coaches Win Loss record onto the table
fourth_down_pred_table<-left_join(fourth_down_pred_table, Coaches_WL, by = c('home_coach' = "Coach"))
colnames(fourth_down_pred_table)[which(names(fourth_down_pred_table) == 'W-L%')] <- 'home_WL'
fourth_down_pred_table<-left_join(fourth_down_pred_table, Coaches_WL, by = c('away_coach' = "Coach"))
colnames(fourth_down_pred_table)[which(names(fourth_down_pred_table) == 'W-L%')] <- 'away_WL'


#count of how often all coaches went for it on 4th versus kicking
fourth_down_pred_table <- fourth_down_pred_table %>% 
  mutate(fourth_attempt = case_when(
    play_type == 'field_goal' ~ 0,
    play_type == 'punt' ~ 0,
    play_type == 'run' ~ 1,
    play_type == 'pass' ~ 1
  ))

#saving the fourth down attempts of the coaches
fourth_attempt<- as.data.frame(table(fourth_down_pred_table$fourth_attempt))

#creating a pie chart of coaches attempting to convert or kick the ball
Pie_All<- ggplot(fourth_attempt, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1") +
  ggtitle('Coaches Kicking versus Attempting on Fourth Down')+
  theme(plot.title = element_text(size = 20, hjust = .5))

Pie_All
```

![](Capstone-Code_files/figure-gfm/setup-1.png)<!-- -->

``` r
#Classifying Winning and Losing Record Coaches
#pos_team_winning indicates the team with possession's coach has a winning record
#winning is a coach who's record is greater than 0.500
fourth_down_pred_table$pos_team_winning <- with(fourth_down_pred_table, ifelse(posteam_type == 'home' & home_WL > 0.500, "TRUE", 
                                                                               ifelse(posteam_type == 'away' & away_WL > 0.500, "TRUE", "FALSE")))
table(fourth_down_pred_table$play_type, fourth_down_pred_table$pos_team_winning)
```

    ##             
    ##              FALSE TRUE
    ##   field_goal  1822 3805
    ##   pass         825 1360
    ##   punt        5159 8848
    ##   run          378  821

``` r
table(fourth_down_pred_table$pos_team_winning)
```

    ## 
    ## FALSE  TRUE 
    ##  8184 14834

``` r
#making tables for winning and losing coaches
Losing_Coach_Play_Type<-table(fourth_down_pred_table$fourth_attempt[fourth_down_pred_table$pos_team_winning =='FALSE'])
Winning_Coach_Play_Type <-table(fourth_down_pred_table$fourth_attempt[fourth_down_pred_table$pos_team_winning =='TRUE'])

#creating the data frame to be used in analysis of winning versus losing coaches
Winning.v.Losing<-data.frame("Losing Coaches" = Losing_Coach_Play_Type, "Winning Coaches" = Winning_Coach_Play_Type)
row.names(Winning.v.Losing) = Winning.v.Losing$Losing.Coaches.Var1
Winning.v.Losing<-Winning.v.Losing[,-c(1,3)]


#Chi Square test to see if winning and losing attempt to convert the same frequency
Chi_Test<- chisq.test(Winning.v.Losing)
Chi_Test
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  Winning.v.Losing
    ## X-squared = 3.8519e-28, df = 1, p-value = 1

``` r
#model 
Model_Set<-fourth_down_pred_table[,-c(3,4,5,14,15,19,20,35,36,37,39,40,41,42)]

All_Coaches_Pred_Table<-Model_Set[,-30]
Fourth_Down_Model_ALL<- glm(fourth_attempt ~., data= All_Coaches_Pred_Table, family = 'binomial')
summary(Fourth_Down_Model_ALL)
```

    ## 
    ## Call:
    ## glm(formula = fourth_attempt ~ ., family = "binomial", data = All_Coaches_Pred_Table)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0596  -0.2449  -0.0988  -0.0254   4.9308  
    ## 
    ## Coefficients: (4 not defined because of singularities)
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 1.505e+06  1.200e+06   1.254  0.20985    
    ## season_typeREG              3.013e-01  1.674e-01   1.800  0.07186 .  
    ## week                        2.809e-02  6.572e-03   4.275 1.91e-05 ***
    ## yardline_100               -2.760e-03  5.874e-03  -0.470  0.63845    
    ## half_seconds_remaining     -3.287e-04  2.425e-04  -1.356  0.17525    
    ## game_seconds_remaining     -2.993e-04  3.093e-04  -0.968  0.33322    
    ## game_halfHalf2              1.094e+00  3.788e-01   2.888  0.00388 ** 
    ## game_halfOvertime                  NA         NA      NA       NA    
    ## qtr                         4.953e-02  1.400e-01   0.354  0.72355    
    ## goal_to_go                 -4.980e-01  1.611e-01  -3.090  0.00200 ** 
    ## ydstogo                    -5.325e-02  7.115e-03  -7.485 7.18e-14 ***
    ## ydsnet                      5.529e-02  2.083e-03  26.548  < 2e-16 ***
    ## timeout                     2.460e+00  4.232e-01   5.812 6.18e-09 ***
    ## posteam_timeouts_remaining -3.067e-01  4.465e-02  -6.868 6.51e-12 ***
    ## defteam_timeouts_remaining  9.184e-02  4.443e-02   2.067  0.03873 *  
    ## score_differential          7.674e-03  6.496e-03   1.181  0.23745    
    ## no_score_prob              -1.505e+06  1.200e+06  -1.254  0.20985    
    ## opp_fg_prob                -1.505e+06  1.200e+06  -1.254  0.20985    
    ## opp_safety_prob            -1.505e+06  1.200e+06  -1.254  0.20987    
    ## opp_td_prob                -1.505e+06  1.200e+06  -1.254  0.20985    
    ## fg_prob                    -1.505e+06  1.200e+06  -1.254  0.20985    
    ## safety_prob                -1.505e+06  1.200e+06  -1.254  0.20979    
    ## td_prob                    -1.505e+06  1.200e+06  -1.254  0.20986    
    ## total_home_rush_epa         3.764e-03  5.540e-03   0.680  0.49681    
    ## total_away_rush_epa                NA         NA      NA       NA    
    ## total_home_pass_epa         2.472e-03  2.548e-03   0.970  0.33187    
    ## total_away_pass_epa                NA         NA      NA       NA    
    ## wp                         -3.811e+00  2.460e-01 -15.491  < 2e-16 ***
    ## def_wp                             NA         NA      NA       NA    
    ## div_game                    8.835e-02  6.638e-02   1.331  0.18320    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 19219.9  on 23017  degrees of freedom
    ## Residual deviance:  7046.6  on 22992  degrees of freedom
    ## AIC: 7098.6
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
plot(Fourth_Down_Model_ALL)
```

![](Capstone-Code_files/figure-gfm/setup-2.png)<!-- -->![](Capstone-Code_files/figure-gfm/setup-3.png)<!-- -->![](Capstone-Code_files/figure-gfm/setup-4.png)<!-- -->![](Capstone-Code_files/figure-gfm/setup-5.png)<!-- -->

``` r
#data table only with winning coaches included
Winning_Fourth_Pred_Table<-Model_Set[Model_Set$pos_team_winning == 'TRUE',-30]

#data table with only losing coaches included
Losing_Fourth_Pred_Table <- Model_Set[Model_Set$pos_team_winning == 'FALSE', -30]

#model temp 
Fourth_Down_Model_Winning<-glm(fourth_attempt ~., data= Winning_Fourth_Pred_Table, family = 'binomial')
summary(Fourth_Down_Model_Winning)
```

    ## 
    ## Call:
    ## glm(formula = fourth_attempt ~ ., family = "binomial", data = Winning_Fourth_Pred_Table)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.2108  -0.2495  -0.1036  -0.0270   4.9199  
    ## 
    ## Coefficients: (4 not defined because of singularities)
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 1.676e+06  1.516e+06   1.105  0.26902    
    ## season_typeREG              5.199e-01  1.848e-01   2.814  0.00490 ** 
    ## week                        3.475e-02  8.297e-03   4.188 2.81e-05 ***
    ## yardline_100               -1.351e-02  7.385e-03  -1.829  0.06736 .  
    ## half_seconds_remaining     -8.528e-05  3.128e-04  -0.273  0.78516    
    ## game_seconds_remaining     -5.208e-04  3.957e-04  -1.316  0.18809    
    ## game_halfHalf2              6.583e-01  4.932e-01   1.335  0.18197    
    ## game_halfOvertime                  NA         NA      NA       NA    
    ## qtr                         2.614e-02  1.755e-01   0.149  0.88164    
    ## goal_to_go                 -5.532e-01  1.986e-01  -2.785  0.00536 ** 
    ## ydstogo                    -6.038e-02  9.205e-03  -6.559 5.40e-11 ***
    ## ydsnet                      5.606e-02  2.565e-03  21.855  < 2e-16 ***
    ## timeout                     2.892e+00  6.020e-01   4.803 1.56e-06 ***
    ## posteam_timeouts_remaining -2.778e-01  5.522e-02  -5.031 4.88e-07 ***
    ## defteam_timeouts_remaining  8.917e-02  5.408e-02   1.649  0.09918 .  
    ## score_differential          1.117e-02  8.255e-03   1.353  0.17591    
    ## no_score_prob              -1.676e+06  1.516e+06  -1.105  0.26902    
    ## opp_fg_prob                -1.676e+06  1.516e+06  -1.105  0.26901    
    ## opp_safety_prob            -1.675e+06  1.516e+06  -1.105  0.26904    
    ## opp_td_prob                -1.676e+06  1.516e+06  -1.105  0.26901    
    ## fg_prob                    -1.676e+06  1.516e+06  -1.105  0.26901    
    ## safety_prob                -1.676e+06  1.516e+06  -1.105  0.26895    
    ## td_prob                    -1.676e+06  1.516e+06  -1.105  0.26902    
    ## total_home_rush_epa         1.491e-03  7.005e-03   0.213  0.83144    
    ## total_away_rush_epa                NA         NA      NA       NA    
    ## total_home_pass_epa         3.605e-03  3.302e-03   1.092  0.27493    
    ## total_away_pass_epa                NA         NA      NA       NA    
    ## wp                         -3.706e+00  3.054e-01 -12.134  < 2e-16 ***
    ## def_wp                             NA         NA      NA       NA    
    ## div_game                    3.100e-02  8.361e-02   0.371  0.71080    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 12387  on 14833  degrees of freedom
    ## Residual deviance:  4565  on 14808  degrees of freedom
    ## AIC: 4617
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
Fourth_Down_Model_Losing<-glm(fourth_attempt ~., data= Losing_Fourth_Pred_Table, family = 'binomial')
summary(Fourth_Down_Model_Losing)
```

    ## 
    ## Call:
    ## glm(formula = fourth_attempt ~ ., family = "binomial", data = Losing_Fourth_Pred_Table)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8933  -0.2292  -0.0852  -0.0209   3.9517  
    ## 
    ## Coefficients: (4 not defined because of singularities)
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 6.184e+05  2.000e+06   0.309  0.75716    
    ## season_typeREG             -7.002e-01  4.994e-01  -1.402  0.16085    
    ## week                        1.675e-02  1.096e-02   1.528  0.12645    
    ## yardline_100                1.672e-02  1.014e-02   1.649  0.09920 .  
    ## half_seconds_remaining     -6.099e-04  3.877e-04  -1.573  0.11565    
    ## game_seconds_remaining     -4.368e-05  5.025e-04  -0.087  0.93073    
    ## game_halfHalf2              1.527e+00  5.945e-01   2.569  0.01019 *  
    ## game_halfOvertime                  NA         NA      NA       NA    
    ## qtr                         1.135e-01  2.354e-01   0.482  0.62967    
    ## goal_to_go                 -4.132e-01  2.798e-01  -1.477  0.13977    
    ## ydstogo                    -4.556e-02  1.151e-02  -3.958 7.54e-05 ***
    ## ydsnet                      5.446e-02  3.657e-03  14.891  < 2e-16 ***
    ## timeout                     1.903e+00  6.438e-01   2.956  0.00312 ** 
    ## posteam_timeouts_remaining -3.435e-01  7.758e-02  -4.427 9.54e-06 ***
    ## defteam_timeouts_remaining  9.823e-02  8.022e-02   1.224  0.22077    
    ## score_differential          6.157e-03  1.073e-02   0.574  0.56615    
    ## no_score_prob              -6.184e+05  2.000e+06  -0.309  0.75716    
    ## opp_fg_prob                -6.184e+05  2.000e+06  -0.309  0.75715    
    ## opp_safety_prob            -6.183e+05  2.000e+06  -0.309  0.75717    
    ## opp_td_prob                -6.184e+05  2.000e+06  -0.309  0.75715    
    ## fg_prob                    -6.184e+05  2.000e+06  -0.309  0.75716    
    ## safety_prob                -6.185e+05  2.000e+06  -0.309  0.75709    
    ## td_prob                    -6.183e+05  2.000e+06  -0.309  0.75717    
    ## total_home_rush_epa         6.470e-03  9.217e-03   0.702  0.48271    
    ## total_away_rush_epa                NA         NA      NA       NA    
    ## total_home_pass_epa         1.431e-03  4.084e-03   0.350  0.72599    
    ## total_away_pass_epa                NA         NA      NA       NA    
    ## wp                         -4.486e+00  4.366e-01 -10.274  < 2e-16 ***
    ## def_wp                             NA         NA      NA       NA    
    ## div_game                    1.657e-01  1.115e-01   1.486  0.13715    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6833.0  on 8183  degrees of freedom
    ## Residual deviance: 2426.5  on 8158  degrees of freedom
    ## AIC: 2478.5
    ## 
    ## Number of Fisher Scoring iterations: 8

``` r
#coefficient table for all of the coaches
coef_table_All<-as.data.frame(summary(Fourth_Down_Model_ALL)$coefficients[-27,1])
coef_table_All$name<-rownames(coef_table_All)

coef_table_All
```

    ##                            summary(Fourth_Down_Model_ALL)$coefficients[-27, 1]
    ## (Intercept)                                                       1.505090e+06
    ## season_typeREG                                                    3.013024e-01
    ## week                                                              2.809085e-02
    ## yardline_100                                                     -2.760046e-03
    ## half_seconds_remaining                                           -3.286620e-04
    ## game_seconds_remaining                                           -2.992694e-04
    ## game_halfHalf2                                                    1.093834e+00
    ## qtr                                                               4.952850e-02
    ## goal_to_go                                                       -4.979822e-01
    ## ydstogo                                                          -5.325192e-02
    ## ydsnet                                                            5.529353e-02
    ## timeout                                                           2.459658e+00
    ## posteam_timeouts_remaining                                       -3.066881e-01
    ## defteam_timeouts_remaining                                        9.183911e-02
    ## score_differential                                                7.673987e-03
    ## no_score_prob                                                    -1.505090e+06
    ## opp_fg_prob                                                      -1.505106e+06
    ## opp_safety_prob                                                  -1.505038e+06
    ## opp_td_prob                                                      -1.505105e+06
    ## fg_prob                                                          -1.505095e+06
    ## safety_prob                                                      -1.505294e+06
    ## td_prob                                                          -1.505061e+06
    ## total_home_rush_epa                                               3.764224e-03
    ## total_home_pass_epa                                               2.472222e-03
    ## wp                                                               -3.811434e+00
    ## div_game                                                          8.835313e-02
    ##                                                  name
    ## (Intercept)                               (Intercept)
    ## season_typeREG                         season_typeREG
    ## week                                             week
    ## yardline_100                             yardline_100
    ## half_seconds_remaining         half_seconds_remaining
    ## game_seconds_remaining         game_seconds_remaining
    ## game_halfHalf2                         game_halfHalf2
    ## qtr                                               qtr
    ## goal_to_go                                 goal_to_go
    ## ydstogo                                       ydstogo
    ## ydsnet                                         ydsnet
    ## timeout                                       timeout
    ## posteam_timeouts_remaining posteam_timeouts_remaining
    ## defteam_timeouts_remaining defteam_timeouts_remaining
    ## score_differential                 score_differential
    ## no_score_prob                           no_score_prob
    ## opp_fg_prob                               opp_fg_prob
    ## opp_safety_prob                       opp_safety_prob
    ## opp_td_prob                               opp_td_prob
    ## fg_prob                                       fg_prob
    ## safety_prob                               safety_prob
    ## td_prob                                       td_prob
    ## total_home_rush_epa               total_home_rush_epa
    ## total_home_pass_epa               total_home_pass_epa
    ## wp                                                 wp
    ## div_game                                     div_game

``` r
#coefficient table for all of the winning coaches
coef_table_Win<-as.data.frame(summary(Fourth_Down_Model_Winning)$coefficients[,1])
coef_table_Win$name<-rownames(coef_table_All)

coef_table_Win
```

    ##                            summary(Fourth_Down_Model_Winning)$coefficients[, 1]
    ## (Intercept)                                                        1.675556e+06
    ## season_typeREG                                                     5.198506e-01
    ## week                                                               3.474930e-02
    ## yardline_100                                                      -1.350910e-02
    ## half_seconds_remaining                                            -8.528135e-05
    ## game_seconds_remaining                                            -5.208297e-04
    ## game_halfHalf2                                                     6.582627e-01
    ## qtr                                                                2.613600e-02
    ## goal_to_go                                                        -5.531941e-01
    ## ydstogo                                                           -6.038100e-02
    ## ydsnet                                                             5.605508e-02
    ## timeout                                                            2.891525e+00
    ## posteam_timeouts_remaining                                        -2.777915e-01
    ## defteam_timeouts_remaining                                         8.916855e-02
    ## score_differential                                                 1.117336e-02
    ## no_score_prob                                                     -1.675555e+06
    ## opp_fg_prob                                                       -1.675570e+06
    ## opp_safety_prob                                                   -1.675485e+06
    ## opp_td_prob                                                       -1.675570e+06
    ## fg_prob                                                           -1.675560e+06
    ## safety_prob                                                       -1.675771e+06
    ## td_prob                                                           -1.675527e+06
    ## total_home_rush_epa                                                1.491031e-03
    ## total_home_pass_epa                                                3.604723e-03
    ## wp                                                                -3.705729e+00
    ## div_game                                                           3.100131e-02
    ##                                                  name
    ## (Intercept)                               (Intercept)
    ## season_typeREG                         season_typeREG
    ## week                                             week
    ## yardline_100                             yardline_100
    ## half_seconds_remaining         half_seconds_remaining
    ## game_seconds_remaining         game_seconds_remaining
    ## game_halfHalf2                         game_halfHalf2
    ## qtr                                               qtr
    ## goal_to_go                                 goal_to_go
    ## ydstogo                                       ydstogo
    ## ydsnet                                         ydsnet
    ## timeout                                       timeout
    ## posteam_timeouts_remaining posteam_timeouts_remaining
    ## defteam_timeouts_remaining defteam_timeouts_remaining
    ## score_differential                 score_differential
    ## no_score_prob                           no_score_prob
    ## opp_fg_prob                               opp_fg_prob
    ## opp_safety_prob                       opp_safety_prob
    ## opp_td_prob                               opp_td_prob
    ## fg_prob                                       fg_prob
    ## safety_prob                               safety_prob
    ## td_prob                                       td_prob
    ## total_home_rush_epa               total_home_rush_epa
    ## total_home_pass_epa               total_home_pass_epa
    ## wp                                                 wp
    ## div_game                                     div_game

``` r
#coefficient table for all of the losing coaches
coef_table_Loss<-as.data.frame(summary(Fourth_Down_Model_Losing)$coefficients[,1])
coef_table_Loss$name<-rownames(coef_table_All)

coef_table_Loss
```

    ##                            summary(Fourth_Down_Model_Losing)$coefficients[, 1]
    ## (Intercept)                                                       6.183532e+05
    ## season_typeREG                                                   -7.002421e-01
    ## week                                                              1.675180e-02
    ## yardline_100                                                      1.672005e-02
    ## half_seconds_remaining                                           -6.099265e-04
    ## game_seconds_remaining                                           -4.368298e-05
    ## game_halfHalf2                                                    1.527466e+00
    ## qtr                                                               1.135290e-01
    ## goal_to_go                                                       -4.132200e-01
    ## ydstogo                                                          -4.555703e-02
    ## ydsnet                                                            5.445849e-02
    ## timeout                                                           1.902841e+00
    ## posteam_timeouts_remaining                                       -3.434685e-01
    ## defteam_timeouts_remaining                                        9.823326e-02
    ## score_differential                                                6.157311e-03
    ## no_score_prob                                                    -6.183533e+05
    ## opp_fg_prob                                                      -6.183704e+05
    ## opp_safety_prob                                                  -6.183340e+05
    ## opp_td_prob                                                      -6.183698e+05
    ## fg_prob                                                          -6.183572e+05
    ## safety_prob                                                      -6.185304e+05
    ## td_prob                                                          -6.183232e+05
    ## total_home_rush_epa                                               6.469865e-03
    ## total_home_pass_epa                                               1.431162e-03
    ## wp                                                               -4.486017e+00
    ## div_game                                                          1.656861e-01
    ##                                                  name
    ## (Intercept)                               (Intercept)
    ## season_typeREG                         season_typeREG
    ## week                                             week
    ## yardline_100                             yardline_100
    ## half_seconds_remaining         half_seconds_remaining
    ## game_seconds_remaining         game_seconds_remaining
    ## game_halfHalf2                         game_halfHalf2
    ## qtr                                               qtr
    ## goal_to_go                                 goal_to_go
    ## ydstogo                                       ydstogo
    ## ydsnet                                         ydsnet
    ## timeout                                       timeout
    ## posteam_timeouts_remaining posteam_timeouts_remaining
    ## defteam_timeouts_remaining defteam_timeouts_remaining
    ## score_differential                 score_differential
    ## no_score_prob                           no_score_prob
    ## opp_fg_prob                               opp_fg_prob
    ## opp_safety_prob                       opp_safety_prob
    ## opp_td_prob                               opp_td_prob
    ## fg_prob                                       fg_prob
    ## safety_prob                               safety_prob
    ## td_prob                                       td_prob
    ## total_home_rush_epa               total_home_rush_epa
    ## total_home_pass_epa               total_home_pass_epa
    ## wp                                                 wp
    ## div_game                                     div_game

``` r
#joining up the wining and losing coach coefficient tables for comparison
Comparison_Table<-left_join(coef_table_Win, coef_table_Loss, by = 'name')
Comparison_Table<-Comparison_Table[c(2:4,9:14,25),c(2,1,3)]
colnames(Comparison_Table)<-c('Name', 'Winning_Coaches', 'Losing_Coaches')
Comparison_Table[c(1,2,4,9),3]<-0
Comparison_Table[,1]<-c('Regular Season Game', 'Week', 'Yard line', '4th and Goal', 'Yards to Go', 'Net Yards on Drive', 'Timeout', 'Pos Team Timeout Left', 'Def Team Timeouts Left', 'WP')

#bar plot of significant coefficients
BarPlot_Coef<- Comparison_Table %>% gather(key = Group, value = Value, 2:3)

Coef_Bar_Plot<-ggplot(BarPlot_Coef, aes(Name, Value, fill = Group)) +geom_col(position = 'dodge') + 
  labs(title='Winning Versus Losing Coach Decision Factors', 
       x= 'Variable Name',
       y = 'Estimate',
       subtitle = 'Estimate Excluded if p > 0.1')+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  coord_flip()


Coef_Bar_Plot
```

![](Capstone-Code_files/figure-gfm/setup-6.png)<!-- -->

``` r
#making the tables more readable
Winning_Versus_Losing<-Winning.v.Losing
Winning_Versus_Losing$All.Coaches<-Winning_Versus_Losing$Losing.Coaches.Freq+Winning_Versus_Losing$Winning.Coaches.Freq
colnames(Winning_Versus_Losing)<- c('Losing Coaches','Winning Coaches','All Coaches')


Winning_Versus_Losing$Decision<- c("Kick","Attempt")  
Winning_Versus_Losing<- Winning_Versus_Losing[,c(4,1:3)]

CT_Winning_Versus_Losing<- gt(Winning_Versus_Losing)

CT_Winning_Versus_Losing <- CT_Winning_Versus_Losing %>% tab_header(
  title = "Coaching Decision by Group"
)

CT_Winning_Versus_Losing
```

<div id="vxkekjbwfn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vxkekjbwfn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vxkekjbwfn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vxkekjbwfn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vxkekjbwfn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vxkekjbwfn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxkekjbwfn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vxkekjbwfn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vxkekjbwfn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vxkekjbwfn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vxkekjbwfn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vxkekjbwfn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vxkekjbwfn .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#vxkekjbwfn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vxkekjbwfn .gt_from_md > :first-child {
  margin-top: 0;
}

#vxkekjbwfn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vxkekjbwfn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vxkekjbwfn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#vxkekjbwfn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxkekjbwfn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#vxkekjbwfn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxkekjbwfn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vxkekjbwfn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vxkekjbwfn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxkekjbwfn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vxkekjbwfn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#vxkekjbwfn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vxkekjbwfn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#vxkekjbwfn .gt_left {
  text-align: left;
}

#vxkekjbwfn .gt_center {
  text-align: center;
}

#vxkekjbwfn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vxkekjbwfn .gt_font_normal {
  font-weight: normal;
}

#vxkekjbwfn .gt_font_bold {
  font-weight: bold;
}

#vxkekjbwfn .gt_font_italic {
  font-style: italic;
}

#vxkekjbwfn .gt_super {
  font-size: 65%;
}

#vxkekjbwfn .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Coaching Decision by Group</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Decision</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Losing Coaches</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Winning Coaches</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">All Coaches</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Kick</td>
<td class="gt_row gt_right">6981</td>
<td class="gt_row gt_right">12653</td>
<td class="gt_row gt_right">19634</td></tr>
    <tr><td class="gt_row gt_left">Attempt</td>
<td class="gt_row gt_right">1203</td>
<td class="gt_row gt_right">2181</td>
<td class="gt_row gt_right">3384</td></tr>
  </tbody>
  
  
</table>
</div>
