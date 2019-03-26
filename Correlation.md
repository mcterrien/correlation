Correlation
================
CT
March 13, 2019

### Loading libraries and data

``` r
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggcorrplot)
```

### Reading data

``` r
# Listing the data structure
str(mtcars)
```

    ## 'data.frame':    32 obs. of  11 variables:
    ##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
    ##  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
    ##  $ disp: num  160 160 108 258 360 ...
    ##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
    ##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
    ##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
    ##  $ qsec: num  16.5 17 18.6 19.4 17 ...
    ##  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
    ##  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
    ##  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
    ##  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...

``` r
# First rows
head(mtcars)
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

``` r
# Data summary
summary(mtcars)
```

    ##       mpg             cyl             disp             hp       
    ##  Min.   :10.40   Min.   :4.000   Min.   : 71.1   Min.   : 52.0  
    ##  1st Qu.:15.43   1st Qu.:4.000   1st Qu.:120.8   1st Qu.: 96.5  
    ##  Median :19.20   Median :6.000   Median :196.3   Median :123.0  
    ##  Mean   :20.09   Mean   :6.188   Mean   :230.7   Mean   :146.7  
    ##  3rd Qu.:22.80   3rd Qu.:8.000   3rd Qu.:326.0   3rd Qu.:180.0  
    ##  Max.   :33.90   Max.   :8.000   Max.   :472.0   Max.   :335.0  
    ##       drat             wt             qsec             vs        
    ##  Min.   :2.760   Min.   :1.513   Min.   :14.50   Min.   :0.0000  
    ##  1st Qu.:3.080   1st Qu.:2.581   1st Qu.:16.89   1st Qu.:0.0000  
    ##  Median :3.695   Median :3.325   Median :17.71   Median :0.0000  
    ##  Mean   :3.597   Mean   :3.217   Mean   :17.85   Mean   :0.4375  
    ##  3rd Qu.:3.920   3rd Qu.:3.610   3rd Qu.:18.90   3rd Qu.:1.0000  
    ##  Max.   :4.930   Max.   :5.424   Max.   :22.90   Max.   :1.0000  
    ##        am              gear            carb      
    ##  Min.   :0.0000   Min.   :3.000   Min.   :1.000  
    ##  1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:2.000  
    ##  Median :0.0000   Median :4.000   Median :2.000  
    ##  Mean   :0.4062   Mean   :3.688   Mean   :2.812  
    ##  3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:4.000  
    ##  Max.   :1.0000   Max.   :5.000   Max.   :8.000

### Plotting Correlation

![](Correlation_files/figure-markdown_github-ascii_identifiers/plotcorrelation-1.png)

### Investigating mpg and hp relation

``` r
# Limiting the dataset to two columns
mymtcars <- mtcars %>%
  select(mpg, hp)

# Assumptions before calculating correlation
# 1st - The relation between the variables is linear
# 2nd - The data follow a normal distribution

gg <- ggplot(mymtcars, aes(mpg)) + 
  geom_density(kernel = "gaussian") +
  labs(subtitle="Miles/(US) gallon")
plot(gg)
```

![](Correlation_files/figure-markdown_github-ascii_identifiers/plotting-1.png)

``` r
gg <- ggplot(mymtcars, aes(hp)) + 
  geom_density(kernel = "gaussian") +
  labs(subtitle="Gross horsepower")
plot(gg)
```

![](Correlation_files/figure-markdown_github-ascii_identifiers/plotting-2.png)

``` r
gg <- ggplot(mymtcars, aes(x=mpg, y=hp)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) + 
  labs(subtitle="Miles/(US) gallon Vs Gross horsepower", 
       y="mpg", 
       x="hp", 
       title="Scatterplot", 
       caption = "Source: mtcars")
plot(gg)
```

![](Correlation_files/figure-markdown_github-ascii_identifiers/plotting-3.png)

### Correlation test

``` r
# cor.test() test for association/correlation between paired samples. It returns both the correlation coefficient and the significance level(or p-value) of the correlation.

print(cor.test(mymtcars$mpg,mymtcars$hp))
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  mymtcars$mpg and mymtcars$hp
    ## t = -6.7424, df = 30, p-value = 1.788e-07
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8852686 -0.5860994
    ## sample estimates:
    ##        cor 
    ## -0.7761684

###### Based on the correlation test, we reject the null hypothesis that true correlation is equal to zero.
