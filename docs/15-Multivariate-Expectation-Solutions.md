# Multivariate Expectation {#MULTIEXP}




## Objectives

1) Given a joint pmf/pdf, obtain means and variances of random variables and functions of random variables.  

2) Define the terms *covariance* and *correlation*, and given a joint pmf/pdf, obtain the covariance and correlation between two random variables.  

3) Given a joint pmf/pdf, determine whether random variables are *independent* of one another.   

4) Find conditional expectations.  


## Homework  

### Problem 1

Let $X$ and $Y$ be continuous random variables with joint pdf: 
$$
f_{X,Y}(x,y)=x + y
$$

where $0 \leq x \leq 1$ and $0 \leq y \leq 1$. 

a. Find $\E(X)$ and $\E(Y)$.  We will use the marginal pdfs found in the Application 14 solution. 

$$
\E(X)=\int_0^1 x\left(x+\frac{1}{2}\right)\diff x=\frac{x^3}{3}+\frac{x^2}{4}\bigg|_0^1=\frac{1}{3}+\frac{1}{4}=\frac{7}{12}=0.583
$$
Or numerically:


```r
f <- function(x) { x[1]*(x[1] + x[2]) } # "x" is vector
adaptIntegrate(f, lowerLimit = c(0, 0), upperLimit = c(1, 1))
```

```
## $integral
## [1] 0.5833333
## 
## $error
## [1] 1.110223e-16
## 
## $functionEvaluations
## [1] 17
## 
## $returnCode
## [1] 0
```

$$
\E(Y)=\int_0^1 y\left(y+\frac{1}{2}\right)\diff y = 0.583
$$

b. Find $\Var(X)$ and $\Var(Y)$.  

$$
\Var(X)=\E(X^2)-\E(X)^2
$$
$$
\E(X^2)=\int_0^1 x^2\left(x+\frac{1}{2}\right)\diff x = \frac{x^4}{4}+\frac{x^3}{6}\bigg|_0^1=\frac{1}{ 4}+\frac{1}{6}=\frac{5}{12}=0.417
$$
As a check: 


```r
f <- function(x) { x[1]^2*(x[1] + x[2]) } # "x" is vector
round(adaptIntegrate(f, lowerLimit = c(0, 0), upperLimit = c(1, 1))$integral,3)
```

```
## [1] 0.417
```


So, $\Var(X)=0.417-0.583^2=0.076$. 

Similarly, $\Var(Y)=0.076$. 

c. Find $\Cov(X,Y)$ and $\rho$. Are $X$ and $Y$ independent? 
$$
\Cov(X,Y)=\E(XY)-\E(X)\E(Y)
$$
$$
\E(XY)=\int_0^1\int_0^1 xy(x+y)\diff y \diff x = \int_0^1 \frac{x^2y^2}{2}+\frac{xy^3}{3}\bigg|_0^1 \diff x = \int_0^1 \frac{x^2}{2}+\frac{x}{3}\diff x
$$
$$
=\frac{x^3}{6}+\frac{x^2}{6}\bigg|_0^1=\frac{1}{ 3}=0.333
$$

As a check: 


```r
f <- function(x) { x[1]*x[2]*(x[1] + x[2]) } # "x" is vector
round(adaptIntegrate(f, lowerLimit = c(0, 0), upperLimit = c(1, 1))$integral,3)
```

```
## [1] 0.333
```


So, 
$$
\Cov(X,Y)=\frac{1}{3}-\left(\frac{7}{12}\right)^2=-0.007
$$

$$
\rho=\frac{\Cov(X,Y)}{\sqrt{\Var(X)\Var(Y)}}=\frac{-0.007}{\sqrt{0.076\times0.076}}=-0.0909
$$

As a check:


```r
-0.007/sqrt(.076^2)
```

```
## [1] -0.09210526
```
Using exact values:


```r
(1/3-(7/12)^2)/sqrt((5/12-(7/12)^2)^2)
```

```
## [1] -0.09090909
```


With a non-zero covariance, $X$ and $Y$ are not independent. 

d. Find $\Var(3X+2Y)$. 
$$
\Var(3X+2Y)=\Var(3X)+\Var(2Y)+2\Cov(3X,2Y)=
$$
$$
9\Var(X)+4\Var(Y)+12\Cov(X,Y) =
$$
$$
9*0.076+4*0.076+12*-0.007 = 0.910
$$

### Problem 2

Optional - not difficult but does have small Calc III idea. Let $X$ and $Y$ be continuous random variables with joint pmf: 
$$
f_{X,Y}(x,y)=1
$$

where $0 \leq x \leq 1$ and $0 \leq y \leq 2x$. 

a. Find $\E(X)$ and $\E(Y)$. 
$$
\E(X)=\int_0^1 x\cdot 2x\diff x = \frac{2x^3}{3}\bigg|_0^1=0.667
$$

$$
\E(Y)=\int_0^2 y\left(1-\frac{y}{2}\right)\diff y = \frac{y^2}{2}-\frac{y^3}{6}\bigg|_0^2=2-\frac{8}{ 6}=0.667
$$

b. Find $\Var(X)$ and $\Var(Y)$. 
$$
\E(X^2)=\int_0^1 x^2\cdot 2x\diff x = \frac{x^4}{2}\bigg|_0^1=0.5
$$

So, $\Var(X)=0.5-\left(\frac{2}{3}\right)^2=\frac{1}{ 18}=0.056$

$$
\E(Y^2)=\int_0^2 y^2\left(1-\frac{y}{2}\right)\diff y = \frac{y^3}{3}-\frac{y^4}{8}\bigg|_0^2=\frac{8}{ 3}-2=0.667
$$

So, $\Var(Y)=\frac{2}{ 3}-\left(\frac{2}{3}\right)^2=\frac{2}{9}=0.222$

c. Find $\Cov(X,Y)$ and $\rho$. Are $X$ and $Y$ independent?  

$$
\E(XY)=\int_0^1\int_0^{2x} xy\diff y \diff x = \int_0^1 \frac{xy^2}{2}\bigg|_0^{2x}\diff x = \int_0^1 2x^3\diff x = \frac{x^4}{2}\bigg|_0^1=\frac{1}{2}
$$

So,
$$
\Cov(X,Y)=\frac{1}{2}-\frac{2}{3}\frac{2}{3}=\frac{1}{18}=0.056
$$

$$
\rho=\frac{\Cov(X,Y)}{ \sqrt{\Var(X)\Var(Y)}}=\frac{\frac{1}{ 18}}{\sqrt{\frac{1}{18}\frac{2}{9}}}=0.5
$$

$X$ and $Y$ appear to be positively correlated (thus not independent). 

d. Find $\Var\left(\frac{X}{2}+2Y\right)$. 
$$
\Var\left(\frac{X}{2}+2Y\right) = \frac{1}{ 4}\Var(X)+4\Var(Y)+2\Cov(X,Y)=\frac{1}{72}+\frac{8}{ 9}+\frac{1}{9}=1.014
$$

### Problem 3  

Suppose $X$ and $Y$ are *independent* random variables. Show that $\E(XY)=\E(X)\E(Y)$. 

If $X$ and $Y$ are independent, then $\Cov(X,Y)=0$. So,
$$
\Cov(X,Y)=\E(XY)-\E(X)\E(Y)=0
$$

Thus,
$$
\E(XY)=\E(X)\E(Y)
$$

### Problem 4 

You are playing a game with a friend. Each of you roll a fair sided die and record the result.

a. Write the joint probability mass function.

Let $X$ be the number on your die and $Y$ be the number on your friend's die.

$$
\begin{array}{cc|ccc} & & &\textbf{X} & \\ 
& & 1 & 2 & 3 & 4 & 5 & 6 \\
&\hline  1 & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} \\
 & 2 & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} \\
\textbf{Y}& 3 & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} \\
& 4 & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} \\
& 5 & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} \\
& 6 & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36} & \frac{1}{36}  \\
\end{array} 
$$



b. Find the expected value of the product of your score and your friend's score.

To find $E[XY]$, we determine all 36 values of the product of $X$ and $Y$ and multiply by the associated probabilities. Since the probabilities are all equal, we will take the $\frac{1}{36}$ out of the summation. Now

$$E[XY]=\frac{1}{36}(1+2+3+4+5+6+2+4+$$
$$6+8+10+12+3+6+9+12+15+18+4+8+12+16+20+24+$$
$$5+10+15+20+25+30+6+12+18+24+30+36)$$
$$=12.25$$

c. Verify the previous part using simulation.


```r
set.seed(1012)
(do(100000)*(sample(1:6,size=2,replace=TRUE))) %>%
  mutate(prod=V1*V2) %>%
  summarize(Expec=mean(prod))
```

```
##      Expec
## 1 12.25016
```


d. Using simulation, find the expected value of the maximum number on the two roles. 


```r
(do(100000)*max(sample(1:6,size=2,replace=TRUE))) %>%
    summarize(Expec=mean(max))
```

```
##    Expec
## 1 4.4737
```

### Problem 5 

A miner is trapped in a mine containing three doors. The first door leads to a tunnel that takes him to safety after two hours of travel. The second door leads to a tunnel that returns him to the mine after three hours of travel. The third door leads to a tunnel that returns him to his mine after five hours. Assuming that the miner is at all times equally likely to choose any one of the doors, yes a bad assumption but it makes for a nice problem, what is the expected length of time until the miner reaches safety?

Simulating this is a little more challenging because we need a conditional but we try it first before going to the mathematical solution.

Let's write a function that takes a vector and returns the sum of the values up to the first time the number 2 appears, we are using the time values as our sample space. Anytime you are repeating something more than 5 times, it might make sense to write a function.


```r
miner_time <- function(x){
  index <- which(x==2)[1]
  total<-cumsum(x)
  return(total[index])
}
```



```r
set.seed(113)
(do(10000)*miner_time(sample(c(2,3,5),size=20,replace=TRUE))) %>% 
  summarise(Exp=mean(miner_time))
```

```
##       Exp
## 1 10.0092
```

Now let's find it mathematically.

Let $X$ be the time it takes and $Y$ the door. Then we have 

$$E[X] = E[E[X|Y]] $$
$$ = \frac{1}{3}E[X|Y=1]+\frac{1}{3}E[X|Y=2]+\frac{1}{3}E[X|Y=3]$$
Now if door 2 is selected
$$E[X|Y=2]=E[X]+3$$
since the miner will travel for 3 hours and then be back at the starting point.

Likewise if door 3 is select
$$E[X|Y=2]=E[X]+5$$
So
$$ E[x]= \frac{1}{3}2+\frac{1}{3}\left( E[X]+3 \right)+\frac{1}{3}\left( E[X]+5 \right)$$

$$E[x] -  \frac{2}{3}E[X] = \frac{2}{3}+\frac{3}{3}+\frac{5}{3}$$
$$\frac{1}{3}E[X]=\frac{10}{3}$$

$$E[X]=10$$

### Problem 6 

ADVANCED: Let $X_1,X_2,...,X_n$ be independent, identically distributed random variables. (This is often abbreviated as "iid"). Each $X_i$ has mean $\mu$ and variance $\sigma^2$ (i.e., for all $i$, $\E(X_i)=\mu$ and $\Var(X_i)=\sigma^2$). 

Let $S=X_1+X_2+...+X_n=\sum_{i=1}^n X_i$. And let $\bar{X}={\sum_{i=1}^n \frac{X_i}{n}}$. 

Find $\E(S)$, $\Var(S)$, $\E(\bar{X})$ and $\Var(\bar{X})$. 
$$
\E(S)=\E(X_1+X_2+...+X_n)=\E(X_1)+\E(X_2)+...+\E(X_n)=\mu+\mu+...+\mu=n\mu
$$

Since the $X_i$s are all independent:
$$
\Var(S)=\Var(X_1+X_2+...+X_n)=\Var(X_1)+\Var(X_2)+...+\Var(X_n)=n\sigma^2
$$

$$
\E(\bar{X})=\frac{1}{n}\E(X_1+X_2+...+X_n)=\frac{1}{n}n\mu=\mu
$$
$$
\Var(\bar{X})=\frac{1}{n^2}\Var(X_1+X_2+...+X_n)=\frac{1}{n^2}n\sigma^2=\frac{\sigma^2}{n}
$$


