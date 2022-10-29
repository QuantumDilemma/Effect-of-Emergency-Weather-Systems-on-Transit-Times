Effect of emergency weather systems on transit times
================

## Background

As of the year 2018, 55% of the world’s population lived in urban areas.
By 2050, the United Nations expects this number to jump to 68%. Major
metropolitan areas will have to adapt and become more efficient in order
to accommodate billions of new residents in the coming decades. One
infrastructure system in particular that will feel the burden of growing
urban populations is transportation.

Imagine a large, growing city that is located in an area that gets heavy
snowfall in the fall and winter seasons. City officials have partnered
with a large tech company to collect anonymous navigation data about
residents’ transit times during peak rush hours.

Preliminary data seems to verify what city officials suspected: transit
times are substantially longer in the winter months when the city gets a
lot of snow. The city officials decide to institute a “snow emergency”
system between the months of November and March intended to reduce
average transit times during severe weather. A snow emergency will be
issued whenever the city experiences at least 4 inches of snow in a
24-hour period. During snow emergencies, the city will restrict car
traffic to only major roads and will waive fares for public
transportation.

The leaders hope that these actions will incentivize people to use the
city’s efficient train system instead of commuting via car, which will
reduce traffic congestion and transit times. After three winter seasons,
the city decides to analyze the navigation data to see whether the snow
emergency system was effective at reducing transit times.

``` r
#import data
snow_df <- read.csv("snow.csv")
```

``` r
#inspect data
head(snow_df)
```

    ##         date snowfall    emergency  minutes
    ## 1 11/01/2017 2.218687 No Emergency 63.13641
    ## 2 11/02/2017 4.519099    Emergency 45.51861
    ## 3 11/03/2017 5.124571    Emergency 41.83613
    ## 4 11/04/2017 1.301896 No Emergency 51.25552
    ## 5 11/05/2017 5.314226    Emergency 37.57589
    ## 6 11/06/2017 1.636324 No Emergency 43.83927

snowfall = forcing variable - amount of snowfall (inches) emergency =
treatment variable - whether or not an emergency was declared minutes =
outcome variable - average commute time (minutes)

``` r
#create scatter plot of the data
scatter_base <- ggplot(snow_df, 
                       aes(x = snowfall, y = minutes, color = emergency, shape = emergency)) +
                  geom_point()

print(scatter_base)
```

![](Effect-of-emergency-weather-systems-2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#add cutpointot the plot
scatter_cutpoint <- scatter_base +
  geom_vline(xintercept = 4, linetype = "dashed")

print(scatter_cutpoint)
```

![](Effect-of-emergency-weather-systems-2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#plot best fit lines for each group to check for discontinuity
scatter_fit <- scatter_cutpoint +
  geom_smooth(aes(group = emergency), method = "lm", se = FALSE)

print(scatter_fit)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Effect-of-emergency-weather-systems-2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#calculate IKbandwidth
snow_ik_bw <- IKbandwidth(
  X = snow_df$snowfall,
  Y = snow_df$minutes,
  cutpoint = 4
)

print(snow_ik_bw)
```

    ## [1] 2.628204

A bandwidth of 2.6 is relatively large compared to the scale of the
forcing variable

``` r
#plot bandwidth on scatter plot
scatter_bw <- scatter_cutpoint +
  geom_vline(xintercept = 4 - c(-snow_ik_bw, snow_ik_bw))

print(scatter_bw)
```

![](Effect-of-emergency-weather-systems-2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#plot local linear regression model
snow_rdd <- RDestimate(
  formula = minutes ~ snowfall,
  data = snow_df,
  cutpoint = 4,
  bw = snow_ik_bw,
)

summary(snow_rdd)
```

    ## 
    ## Call:
    ## RDestimate(formula = minutes ~ snowfall, data = snow_df, cutpoint = 4, 
    ##     bw = snow_ik_bw)
    ## 
    ## Type:
    ## sharp 
    ## 
    ## Estimates:
    ##            Bandwidth  Observations  Estimate  Std. Error  z value  Pr(>|z|) 
    ## LATE       2.628      204           -11.04    2.974       -3.711   2.067e-04
    ## Half-BW    1.314       93           -12.34    3.925       -3.143   1.673e-03
    ## Double-BW  5.256      360           -10.44    2.471       -4.222   2.419e-05
    ##               
    ## LATE       ***
    ## Half-BW    ** 
    ## Double-BW  ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## F-statistics:
    ##            F       Num. DoF  Denom. DoF  p        
    ## LATE       12.576  3         200         2.892e-07
    ## Half-BW     8.323  3          89         1.227e-04
    ## Double-BW  11.097  3         356         1.115e-06

``` r
#print number of observations in each bandwidth
snow_rdd$obs
```

    ## [1] 204  93 360

``` r
#print standard errors
snow_rdd$se
```

    ## [1] 2.974090 3.925439 2.471460

Shows as the sample size increased the std.error decreased.

## Conclusions

Overall we showed that the LATE (Local Area Treatment Effect) for
emergency measures on commuting time was -11.04 minutes. In other words,
the use of the emergency system, which kicked in once there was over 4
inches on snowfall, resulted in an average reduction in commuting time
of 11.04 minutes.

The bandwidth for this model is wide, covering +-2.6 inches of snowfall
on each side of the emergency system cutoff. This covers a large number
of snowfall events giving us confidence that this causal effect is
generalizable over a lot fo the data. The small differences in estimates
between half and double bandwidths further confirms this causal effect
being a reliable estimate
