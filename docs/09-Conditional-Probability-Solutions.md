# Conditional Probability {#CONDPROB}


## Objectives

1) Define conditional probability and distinguish it from joint probability.  

2) Find a conditional probability using its definition.   

3) Using conditional probability, determine whether two events are independent.   

4) Apply Bayes' Rule mathematically and via simulation.  


## Homework  

### Problem 1  

Consider Exercise 1 from Lesson 2. Recall: $A$, $B$ and $C$ are events such that $\Prob(A)=0.5$, $\Prob(B)=0.3$, $\Prob(C)=0.4$, $\Prob(A \cap B)=0.2$, $\Prob(B \cap C)=0.12$, $\Prob(A \cap C)=0.1$, and $\Prob(A \cap B \cap C)=0.05$.

a. Are $A$ and $B$ independent?  

No. $\Prob(A)\Prob(B)=0.15\neq \Prob(A\cap B)$. 

b. Are $B$ and $C$ independent? 

Yes. $\Prob(B)\Prob(C)=0.12 = \Prob(B\cap C)$. Also, 
$$
\Prob(B|C)=\frac{\Prob(B\cap C)}{\Prob(C)}= 0.12/0.4 = 0.3 =\Prob(B)
$$

### Problem 2  

Suppose I have a biased coin (the probability I flip a heads is 0.6). I flip that coin twice. Assume that the coin is memoryless (flips are independent of one another). 

a. What is the probability that the second flip results in heads? 

0.6

b. What is the probability that the second flip results in heads, given the first also resulted in heads? 

The coin is memoryless. So,
$$
\Prob(\mbox{2nd flip heads}|\mbox{1st flip heads}) = 0.6
$$

c. What is the probability both flips result in heads? 

Since the flips are independent, 
$$
\Prob(\mbox{both heads})=\Prob(\mbox{1st flip heads})\Prob(\mbox{2nd flip heads}) = 0.6*0.6=0.36
$$

d. What is the probability exactly one coin flip results in heads? 

This could happen in two ways. The first could be heads OR the second could be heads. 
$$
\Prob(\mbox{exactly one heads})=\Prob(\mbox{1st flip heads})\Prob(\mbox{2nd flip tails}) + \Prob(\mbox{1st flip tails})\Prob(\mbox{2nd flip heads})
$$
$$
0.6*0.4+0.4*0.6 = 0.48
$$

e. Now assume I flip the coin five times. What is the probability the result is 5 heads? 
$$
\Prob(\mbox{5 heads})= 0.6^5 = 0.0778
$$


```r
0.6^5
```

```
## [1] 0.07776
```

f. What is the probability the result is exactly 2 heads (out of 5 flips)? 

There are $\binom{5}{2} = 10$ ways for this to happen (\{HHTTT\},\{HTHTT\},...). So,
$$
\Prob(\mbox{2 heads out of 5 flips})=\binom{5}{2} 0.6^2(1-0.6)^3 = 0.2304
$$


```r
choose(5,2)*0.6^2*0.4^3
```

```
## [1] 0.2304
```

### Problem 3  

(Adapted from IPSUR, [@ipsur]). Suppose there are three assistants working at a company: Moe, Larry and Curly. All three assist with a filing process. Only one filing assistant is needed at a time. Moe assists 60% of the time, Larry assists 30% of the time and Curly assists the remaining 10% of the time. Occasionally, they make errors (misfiles); Moe has a misfile rate of 0.01, Larry has a misfile rate of 0.025, and Curly has a rate of 0.05. Suppose a misfile was discovered, but it is unknown who was on schedule when it occurred. Who is most likely to have committed the misfile? Calculate the probabilities for each of the three assistants. 

Let $E$ be the event a misfile was committed. Also, let $M$, $L$, and $C$ denote the events that Moe, Larry and Curly was the assistant at the time, respectively. 

$$
\Prob(E)=\Prob(E \cap M)+\Prob(E \cap L)+\Prob(E\cap C) 
$$
$$
= \Prob(E|M)\Prob(M)+\Prob(E|L)\Prob(L)+\Prob(E|C)\Prob(C) = 0.01*0.6+0.025*0.3+0.05*0.1 = 0.0185
$$

Thus,
$$
\Prob(M|E)=\frac{\Prob(E \cap M)}{\Prob(E)}= \frac{0.01*0.6}{0.0185}=0.3243
$$

Similarly, $\Prob(L|E)=0.4054$ and $\Prob(C|E)=0.2702$. 

Larry is the assistant most likely to have committed the error. 

### Problem 4

You are playing a game where there are two coins. One coin is fair and the other comes up *heads* 80% of the time. One coin is flipped 3 times and the result is three *heads*, what is the probability that the coin flipped is the fair coin? You will need to make an assumption about the probability of either coin being selected.

a. Use Bayes formula to solve this problem.

I will assume either coin is selected with a 50% probability.

$$
\Prob(Fair) = \Prob(Biased) = .5
$$
$$
\Prob(3 Heads|Fair)=\frac{1}{2}^3=\frac{1}{8}
$$
$$
\Prob(3 Heads|Biased)=.8^3=0.512
$$

Now

$$
\Prob(Fair | 3 Heads) = \frac{\Prob(3 Heads | Fair)\Prob(Fair)}{\Prob(3 Heads | Fair)\Prob(Fair)+\Prob(3 Heads| Biased)\Prob(Biased)}
$$

Which is 

$$
\Prob(Fair | 3 Heads) =  \frac{\frac{1}{8}\frac{1}{2}}{\frac{1}{8}\frac{1}{2}+.8^{3}\frac{1}{2}} = 0.196
$$

```r
.125*.5/(.125*.5+.8^3*.5)
```

```
## [1] 0.1962323
```


b. Use simulation to solve this problem.

Let's use the same assumptions. We could do this problem in two ways. We could flip each coin a fixed number of times and combine the information or use a random process to pick a flipped coin and then flip it three times. Let's do the first.

Let's flip a fair coin 50,000 times and count how many heads we get.


```r
set.seed(1154)
data.frame(do(50000)*rflip(3)) %>%
  filter(heads==3) %>%
  summarise(count=n()) %>%
  pull()
```

```
## [1] 6157
```

Now flip the biased coin.


```r
data.frame(do(50000)*rflip(3,prob=0.8)) %>%
  filter(heads==3) %>%
  summarise(count=n()) %>%
  pull()
```

```
## [1] 25743
```

So we have a total 6157 + 25743 heads of which 6157 came from the fair coin.

Thus the probability of the coin being fair given 3 heads on the flips is:


```r
6157/(6157 + 25743)
```

```
## [1] 0.1930094
```

Or 19.3%.


Next pick a one of the coins with equal probability 100,000 times.


```r
set.seed(501)
results <- rflip(100000,summarize = TRUE)
results
```

```
##       n heads tails prob
## 1 1e+05 50226 49774  0.5
```

Now the fair coin was flipped 50226 times.

Let's see how many times we get 3 heads when we flip that coin 3 times.

```r
data.frame(do(50226)*rflip(3)) %>%
  filter(heads==3) %>%
  summarise(count=n()) %>%
  pull()
```

```
## [1] 6270
```

We have 6270 cases with 3 heads. Now for the biased coin.


```r
data.frame(do(49774)*rflip(3,prob=0.8)) %>%
  filter(heads==3) %>%
  summarise(count=n()) %>%
  pull()
```

```
## [1] 25512
```

Now we can determine the probability of a fair coin given 3 heads.


```r
6270/(6270+25512)
```

```
## [1] 0.1972815
```

This code we could easily adapt if we don't think each coin is being selected with the same frequency. Suppose we think the fair coin has a 75% chance of being selected. The analysis would look like this:



```r
set.seed(9021)
results <- rflip(100000,prob=.75,summarize = TRUE)
results
```

```
##       n heads tails prob
## 1 1e+05 75023 24977 0.75
```

Now the fair coin was flipped 75023 times.

Let's see how many times we get 3 heads when we flip that coin 3 times.

```r
data.frame(do(75023)*rflip(3)) %>%
  filter(heads==3) %>%
  summarise(count=n()) %>%
  pull()
```

```
## [1] 9579
```

We have 9579 cases with 3 heads. Now for the biased coin.


```r
data.frame(do(24977)*rflip(3,prob=0.8)) %>%
  filter(heads==3) %>%
  summarise(count=n()) %>%
  pull()
```

```
## [1] 12789
```

Now we can determine the probability of a fair coin given 3 heads.


```r
9579/(9579+12789)
```

```
## [1] 0.4282457
```

A much different answer. That is because prior to getting the data we believed the fair coin would be selected with a 75% probability. The data indicates that we need to update and lower this probability. We only flipped 3 times but the evidence is so in favor of the biased coin, that our probability dropped substantially. This is why Bayes is such a powerful tool.

Think about what we just did with this problem. We started with a subjective believe that either coin would be selected with equal probability. This is called the prior probability. We then collected data on three flips of the coin. We used this empirical data to update our belief into a posterior probability. This is the basis for Bayesian statistical analysis. Bayesian statistics is an entire discipline unto itself.
