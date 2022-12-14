
```{r, include = FALSE}
#import libraries
library(ggplot2)
library(dplyr)
library(rdd)
```

```{r}
#import data
snow_df <- read.csv("snow.csv")
```

```{r}
#inspect data
head(snow_df)
```

snowfall = forcing variable - amount of snowfall (inches)
emergency = treatment variable - whether or not an emergency was declared
minutes = outcome variable - average commute time (minutes)

```{r}
#create scatter plot of the data
scatter_base <- ggplot(snow_df, 
                       aes(x = snowfall, y = minutes, color = emergency, shape = emergency)) +
  geom_point()

print(scatter_base)
```

```{r}
#add cutpointot the plot
scatter_cutpoint <- scatter_base +
  geom_vline(xintercept = 4, linetype = "dashed")

print(scatter_cutpoint)
```


```{r}
#plot best fit lines for each group to check for discontinuity
scatter_fit <- scatter_cutpoint +
  geom_smooth(aes(group = emergency), method = "lm", se = FALSE)

print(scatter_fit)
```

```{r}
#calculate IKbandwidth
snow_ik_bw <- IKbandwidth(
  X = snow_df$snowfall,
  Y = snow_df$minutes,
  cutpoint = 4
)

print(snow_ik_bw)
```

A bandwidth of 2.6 is relatively large compared to the scale of the forcing variable

```{r}
#plot bandwidth on scatter plot
scatter_bw <- scatter_cutpoint +
  geom_vline(xintercept = 4 - c(-snow_ik_bw, snow_ik_bw))

print(scatter_bw)
```
```{r}
#plot local linear regression model
snow_rdd <- RDestimate(
  formula = minutes ~ snowfall,
  data = snow_df,
  cutpoint = 4,
  bw = snow_ik_bw,
)

summary(snow_rdd)
```

```{r}
#print number of observations in each bandwidth
snow_rdd$obs
```

```{r}
#print standard errors
snow_rdd$se
```

Shows as the sample size increased the std.error decreased.

## Conclusions
Overall we showed that the LATE (Local Area Treatment Effect) for emergency measures on commuting time was -11.04 minutes. In other words, the use of the emergency system, which kicked in once there was over 4 inches on snowfall, resulted in an average reduction in commuting time of 11.04 minutes.

The bandwidth for this model is wide, covering +-2.6 inches of snowfall on each side of the emergency system cutoff. This covers a large number of snowfall events giving us confidence that this causal effect is generalizable over a lot fo the data. The small differences in estimates between half and double bandwidths further confirms this causal effect being a reliable estimate
