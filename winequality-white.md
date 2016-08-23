White Wine Quality
================

Preparation
-----------

Install and load packages:

``` r
library(class)
library(gmodels)
library(corrplot)
```

Import data:
------------

``` r
wine<- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"), stringsAsFactors=FALSE, sep=";", header=TRUE)
```

Explore the data structure and type:
------------------------------------

``` r
head(wine)
```

    ##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
    ## 1           7.0             0.27        0.36           20.7     0.045
    ## 2           6.3             0.30        0.34            1.6     0.049
    ## 3           8.1             0.28        0.40            6.9     0.050
    ## 4           7.2             0.23        0.32            8.5     0.058
    ## 5           7.2             0.23        0.32            8.5     0.058
    ## 6           8.1             0.28        0.40            6.9     0.050
    ##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
    ## 1                  45                  170  1.0010 3.00      0.45     8.8
    ## 2                  14                  132  0.9940 3.30      0.49     9.5
    ## 3                  30                   97  0.9951 3.26      0.44    10.1
    ## 4                  47                  186  0.9956 3.19      0.40     9.9
    ## 5                  47                  186  0.9956 3.19      0.40     9.9
    ## 6                  30                   97  0.9951 3.26      0.44    10.1
    ##   quality
    ## 1       6
    ## 2       6
    ## 3       6
    ## 4       6
    ## 5       6
    ## 6       6

``` r
tail(wine)
```

    ##      fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
    ## 4893           6.5             0.23        0.38            1.3     0.032
    ## 4894           6.2             0.21        0.29            1.6     0.039
    ## 4895           6.6             0.32        0.36            8.0     0.047
    ## 4896           6.5             0.24        0.19            1.2     0.041
    ## 4897           5.5             0.29        0.30            1.1     0.022
    ## 4898           6.0             0.21        0.38            0.8     0.020
    ##      free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates
    ## 4893                  29                  112 0.99298 3.29      0.54
    ## 4894                  24                   92 0.99114 3.27      0.50
    ## 4895                  57                  168 0.99490 3.15      0.46
    ## 4896                  30                  111 0.99254 2.99      0.46
    ## 4897                  20                  110 0.98869 3.34      0.38
    ## 4898                  22                   98 0.98941 3.26      0.32
    ##      alcohol quality
    ## 4893     9.7       5
    ## 4894    11.2       6
    ## 4895     9.6       5
    ## 4896     9.4       6
    ## 4897    12.8       7
    ## 4898    11.8       6

``` r
str(wine)
```

    ## 'data.frame':    4898 obs. of  12 variables:
    ##  $ fixed.acidity       : num  7 6.3 8.1 7.2 7.2 8.1 6.2 7 6.3 8.1 ...
    ##  $ volatile.acidity    : num  0.27 0.3 0.28 0.23 0.23 0.28 0.32 0.27 0.3 0.22 ...
    ##  $ citric.acid         : num  0.36 0.34 0.4 0.32 0.32 0.4 0.16 0.36 0.34 0.43 ...
    ##  $ residual.sugar      : num  20.7 1.6 6.9 8.5 8.5 6.9 7 20.7 1.6 1.5 ...
    ##  $ chlorides           : num  0.045 0.049 0.05 0.058 0.058 0.05 0.045 0.045 0.049 0.044 ...
    ##  $ free.sulfur.dioxide : num  45 14 30 47 47 30 30 45 14 28 ...
    ##  $ total.sulfur.dioxide: num  170 132 97 186 186 97 136 170 132 129 ...
    ##  $ density             : num  1.001 0.994 0.995 0.996 0.996 ...
    ##  $ pH                  : num  3 3.3 3.26 3.19 3.19 3.26 3.18 3 3.3 3.22 ...
    ##  $ sulphates           : num  0.45 0.49 0.44 0.4 0.4 0.44 0.47 0.45 0.49 0.45 ...
    ##  $ alcohol             : num  8.8 9.5 10.1 9.9 9.9 10.1 9.6 8.8 9.5 11 ...
    ##  $ quality             : int  6 6 6 6 6 6 6 6 6 6 ...

``` r
sapply(wine, class)
```

    ##        fixed.acidity     volatile.acidity          citric.acid 
    ##            "numeric"            "numeric"            "numeric" 
    ##       residual.sugar            chlorides  free.sulfur.dioxide 
    ##            "numeric"            "numeric"            "numeric" 
    ## total.sulfur.dioxide              density                   pH 
    ##            "numeric"            "numeric"            "numeric" 
    ##            sulphates              alcohol              quality 
    ##            "numeric"            "numeric"            "integer"

Check data characteristics. Is there missing data?
--------------------------------------------------

``` r
na_count <-function (x) sapply(x, function(y) sum(is.na(y)))
na_count(wine)
```

    ##        fixed.acidity     volatile.acidity          citric.acid 
    ##                    0                    0                    0 
    ##       residual.sugar            chlorides  free.sulfur.dioxide 
    ##                    0                    0                    0 
    ## total.sulfur.dioxide              density                   pH 
    ##                    0                    0                    0 
    ##            sulphates              alcohol              quality 
    ##                    0                    0                    0

``` r
#No missing data
```

What is the correlation between attributes other than wine quality?
-------------------------------------------------------------------

``` r
cor(wine[,1:11])
```

    ##                      fixed.acidity volatile.acidity citric.acid
    ## fixed.acidity           1.00000000      -0.02269729  0.28918070
    ## volatile.acidity       -0.02269729       1.00000000 -0.14947181
    ## citric.acid             0.28918070      -0.14947181  1.00000000
    ## residual.sugar          0.08902070       0.06428606  0.09421162
    ## chlorides               0.02308564       0.07051157  0.11436445
    ## free.sulfur.dioxide    -0.04939586      -0.09701194  0.09407722
    ## total.sulfur.dioxide    0.09106976       0.08926050  0.12113080
    ## density                 0.26533101       0.02711385  0.14950257
    ## pH                     -0.42585829      -0.03191537 -0.16374821
    ## sulphates              -0.01714299      -0.03572815  0.06233094
    ## alcohol                -0.12088112       0.06771794 -0.07572873
    ##                      residual.sugar   chlorides free.sulfur.dioxide
    ## fixed.acidity            0.08902070  0.02308564       -0.0493958591
    ## volatile.acidity         0.06428606  0.07051157       -0.0970119393
    ## citric.acid              0.09421162  0.11436445        0.0940772210
    ## residual.sugar           1.00000000  0.08868454        0.2990983537
    ## chlorides                0.08868454  1.00000000        0.1013923521
    ## free.sulfur.dioxide      0.29909835  0.10139235        1.0000000000
    ## total.sulfur.dioxide     0.40143931  0.19891030        0.6155009650
    ## density                  0.83896645  0.25721132        0.2942104109
    ## pH                      -0.19413345 -0.09043946       -0.0006177961
    ## sulphates               -0.02666437  0.01676288        0.0592172458
    ## alcohol                 -0.45063122 -0.36018871       -0.2501039415
    ##                      total.sulfur.dioxide     density            pH
    ## fixed.acidity                 0.091069756  0.26533101 -0.4258582910
    ## volatile.acidity              0.089260504  0.02711385 -0.0319153683
    ## citric.acid                   0.121130798  0.14950257 -0.1637482114
    ## residual.sugar                0.401439311  0.83896645 -0.1941334540
    ## chlorides                     0.198910300  0.25721132 -0.0904394560
    ## free.sulfur.dioxide           0.615500965  0.29421041 -0.0006177961
    ## total.sulfur.dioxide          1.000000000  0.52988132  0.0023209718
    ## density                       0.529881324  1.00000000 -0.0935914935
    ## pH                            0.002320972 -0.09359149  1.0000000000
    ## sulphates                     0.134562367  0.07449315  0.1559514973
    ## alcohol                      -0.448892102 -0.78013762  0.1214320987
    ##                        sulphates     alcohol
    ## fixed.acidity        -0.01714299 -0.12088112
    ## volatile.acidity     -0.03572815  0.06771794
    ## citric.acid           0.06233094 -0.07572873
    ## residual.sugar       -0.02666437 -0.45063122
    ## chlorides             0.01676288 -0.36018871
    ## free.sulfur.dioxide   0.05921725 -0.25010394
    ## total.sulfur.dioxide  0.13456237 -0.44889210
    ## density               0.07449315 -0.78013762
    ## pH                    0.15595150  0.12143210
    ## sulphates             1.00000000 -0.01743277
    ## alcohol              -0.01743277  1.00000000

``` r
pairs(wine[,1:11])
```

![](assignment3_github_files/figure-markdown_github/graph01-1.png)

``` r
corrplot(cor(wine), method="circle")
```

![](assignment3_github_files/figure-markdown_github/graph02-1.png)

Graph the distribution of wine quality.
---------------------------------------

``` r
hist(wine$quality)
```

![](assignment3_github_files/figure-markdown_github/graph03-1.png)

``` r
barplot(table(wine$quality))
```

![](assignment3_github_files/figure-markdown_github/graph04-1.png)

As we can see, there are a lot of wines with a quality of 6 as compared to the others. The dataset description states there are a lot more normal wines than excellent or poor ones.

Reduce the levels of rating for quality to three levels as high, medium and low.
--------------------------------------------------------------------------------

High(7-10);Medium(6);Low(1-5).

``` r
wine$quality_rating <- cut(
  wine$quality, breaks=c(1,5,6,10), labels = c("Low", "Medium", "High"))

table(wine$quality_rating)
```

    ## 
    ##    Low Medium   High 
    ##   1640   2198   1060

Normalize the dataset
---------------------

``` r
normalize <- function(x) {
               return ((x - min(x)) / (max(x) - min(x))) }

wine_n <- as.data.frame(lapply(wine[1:11], normalize))

summary(wine_n$fixed.acidity)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.2404  0.2885  0.2937  0.3365  1.0000

Divide the data to training and testing groups 70/30
----------------------------------------------------

``` r
wine_indx <- sample(nrow(wine_n), floor(nrow(wine_n) * 0.7))

wine_train <- wine_n[wine_indx, ]
wine_test <- wine_n[-wine_indx, ]
```

Use KNN algorithm to predict the quality of wine using its attributes
---------------------------------------------------------------------

``` r
wine_train_labels <- wine[wine_indx, 13]
wine_test_labels <- wine[-wine_indx, 13]

wine_test_pred <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=5)
```

Evaluate the model performance
------------------------------

``` r
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  1470 
    ## 
    ##  
    ##                  | wine_test_pred 
    ## wine_test_labels |       Low |    Medium |      High | Row Total | 
    ## -----------------|-----------|-----------|-----------|-----------|
    ##              Low |       301 |       149 |        32 |       482 | 
    ##                  |     0.624 |     0.309 |     0.066 |     0.328 | 
    ##                  |     0.622 |     0.231 |     0.094 |           | 
    ##                  |     0.205 |     0.101 |     0.022 |           | 
    ## -----------------|-----------|-----------|-----------|-----------|
    ##           Medium |       169 |       375 |       123 |       667 | 
    ##                  |     0.253 |     0.562 |     0.184 |     0.454 | 
    ##                  |     0.349 |     0.582 |     0.360 |           | 
    ##                  |     0.115 |     0.255 |     0.084 |           | 
    ## -----------------|-----------|-----------|-----------|-----------|
    ##             High |        14 |       120 |       187 |       321 | 
    ##                  |     0.044 |     0.374 |     0.583 |     0.218 | 
    ##                  |     0.029 |     0.186 |     0.547 |           | 
    ##                  |     0.010 |     0.082 |     0.127 |           | 
    ## -----------------|-----------|-----------|-----------|-----------|
    ##     Column Total |       484 |       644 |       342 |      1470 | 
    ##                  |     0.329 |     0.438 |     0.233 |           | 
    ## -----------------|-----------|-----------|-----------|-----------|
    ## 
    ## 

For k=5, test data misclassification rate is lowest, when all predictors are being used.
