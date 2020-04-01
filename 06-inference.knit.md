# Inference: Comparing Two Groups {#comparing-groups}

Adapted from @Zieffler:2011



<br /><br />

:::note
This chapter assumes a working knowledge of **dplyr** and **ggplot2** functionality to work with and plot data. 
:::

<br /><br />

In the [chapter on exploration](#exploration), differences between two groups were examined. Specifically, the question of whether there were differences in the annual household per capita expenditures between the rural and urban populations in Vietnam was addressed. In that chapter, exploratory methods, such as graphical and numerical summarizations, were used to quantify the differences in the two distributions of household per capita expenditures. Exploration is often only the starting point for examining research questions involving group differences. 

These methods, however, do not always provide a complete answer to the research question. For example, most educational and behavioral researchers also want to determine whether the differences that might have shown up in the exploration phase are "real," and to what population(s) the "real" effect can be attributed. A "real" effect is a sample effect that is caused by an actual difference in the population of interest. For example, suppose the mean per capita household expenditures for the entirety of Vietnam is actually less for rural regions. Then a sample result would be expected to reflect this, provided the sample was obtained in a particular way, namely, randomly (see below). In addition to evaluating whether effects are "real", it is important to estimate the size of the effect. Uncertainty is always involved in this endeavor, which relates to the *precision* of the estimate.

Questions of whether or not group differences are "real", estimates of the size of group differences, and the precision of these estimates are typically problems of *statistical inference*. In this chapter, we begin to present some useful methods to answer these types of inferential questions. To do so, we will examine a research question related to the efficacy of after-school programs. 

<br /><br />


## Research Question and Preparation

Demands for accountability and delinquency prevention in recent years have led to rising popularity of after-school programs in the United States. The intuitive appeal of these programs is based on the perception that adolescents left unsupervised will either  simply waste time or, worse, engage in delinquent and dangerous behaviors. To empirically study the effects of attending an after-school program, @gottfredson randomly assigned middle-school students to either a treatment group or control group. The treatment consisted of participation in an after-school program, whereas the control group engaged in their usual routine, but control students were invited to attend one after-school activity per month. In this chapter we will be addressing the following reseacrh question:

> Is there a difference in delinquent behaviors between students who participated in the after-school treatment program and students that did not?

To address these research questions, we will use the data in [after-school.csv](https://raw.githubusercontent.com/zief0002/musings/master/data/after-school.csv) to explore and compare the level of delinquency between the two groups. To begin, we will load two packages that we will use in this analysis.


```r
# Load libraries
library(e1071)
library(tidyverse)
```

The data contains outcome measures for 78 middle-school students that were randomly assigned to either a treatment group (i.e., invited to participate in an after school program) or control group ('treatment as usual' except that members of the control group were invited to attend one after-school activity per month). The outcome measures were T-scaled scores^[T-scaled measurements are scaled to have a mean of 50 and a standrd deviation of 10.] and included measures of the student's aggression, delinquent behavior, and level of victimization. For each measure, higher scal scores indicate higher levels of the outcome in question.

We will import this data using the `read_csv()` function from the **tidyverse** package. 


```r
# Import data
after_school = read_csv("https://raw.githubusercontent.com/zief0002/musings/master/data/after-school.csv")
head(after_school)
```

```
## # A tibble: 6 x 4
##   treatment aggress delinq victim
##   <chr>       <dbl>  <dbl>  <dbl>
## 1 Control      74.5   70.3   58.7
## 2 Control      40.5   44.5   53  
## 3 Control      63.2   57.4   47.3
## 4 Control      40.5   44.5   41.5
## 5 Control      54.1   57.4   47.3
## 6 Control      38.2   44.5   41.5
```


<br /><br />



## Graphically and Numerically Summarizing the Conditional Distributions

Any group comparion should begin with a graphical and numerical exploration of the sample data. Here we will plot the KDE for the distribution of delinquent behaviors for the control and treatment groups.


```r
# Create plot of KDE
ggplot(data = after_school, aes(x = delinq)) +
  geom_density(aes(fill = treatment), alpha = 0.6, bw = 3) +
  theme_bw() +
  xlab("T-scaled delinquency measure") +
  ylab("Probability density") +
  scale_fill_manual(
    name = "",
    values = c("#003366", "#ffcc00")
  )
```

<div class="figure" style="text-align: center">
<img src="06-inference_files/figure-html/fig-01-1.png" alt="Kernel density plots for the distribution of the T-scaled delinquency measure conditioned on treatment group." width="60%" />
<p class="caption">(\#fig:fig-01)Kernel density plots for the distribution of the T-scaled delinquency measure conditioned on treatment group.</p>
</div>

```r
# Compute summary measures
after_school %>%
  group_by(treatment) %>%
  summarize(
    M = mean(delinq),
    SD = sd(delinq),
    G1 = skewness(delinq, type = 2),
    G2 = kurtosis(delinq, type = 2),
    N = n()
  )
```

```
## # A tibble: 2 x 6
##   treatment     M    SD    G1      G2     N
##   <chr>     <dbl> <dbl> <dbl>   <dbl> <int>
## 1 Control    54.9 13.7   1.08  0.0973    41
## 2 Treatment  48.0  8.29  3.89 18.2       37
```

Examination of Figure \@ref(fig:fig-01) reveals similarities in the shape of the distribution for the two groups---both are right skewed. The sample means suggest that the students in the control group had, on average, a level of delinquent behaviors that was 6.9 points higher than the students in the treatment group ($54.9 - 48.0 = 6.9$). The distribution of scores for students in the treatment group also had less variation, were more skewed, and more peaked (leptokurtic) than the distribution of scores in the control group.

<br /><br />


## Is there an Effect of Treatment?

Based on the sample data, it looks as though there may be an effect of treatment on delinquent behaviors. After all, the students in the treatment group have a lower mean level of delinquent behaviors than the students in the control group. However, this difference might be a function of the random assingment of the 78 students into the two different group. It is possible that there is *no effect of treatment* and just by chance students with lower levels of delinquent behaviors were assigned to the treatment group. These two competing hypotheses are foundational to comparing two groups via statistical inference.

- There is *no effect of treatment*; the difference in sample means is due to the random assignment.
- There is an *effect of treatment*; the difference in sample means is due to something more than just the random assignment.

There are several methods applied researchers use to evaluate these competing hypothesis using statistical inference. The most common method, introduced in this chapter, is to use a two-sample *t*-test. However, the exact same results can be obtained from carrying out an analysis of variance (ANOVA), or regression analysis. Some researcher would use a permutation or randomization test to analyze whether the sample differences were due to chance. 

<br /><br />

## The Two-Sample t-Test: Model and Hypotheses

The justification for the two-sample *t*-test lies in the assumption of a specific statistical model underlying the relationship between the observed scores and population parameters. This statistical model is,

$$
Y_{ij} = \mu + \alpha_j + \epsilon_{ij}
$$

In this model, 

- $Y_{ij}$ is the observed measurement or score for the $i$th individual in group (or treatment) $j$;
- $\mu$ is the marginal mean across all individuals and groups;
- $\alpha_j$ is the treatment effect for the group $j$ (defined as $\alpha_j = \mu_j − \mu$, where $\mu_j$ is the mean for group $j$); and 
- $\epsilon_{ij}$ is the amount of random error or vartiation for the $i$th individual in group $j$.

This model states that a person's measurement on the outcome is a function of the marginal mean, an effect of being in a particular group, and some amount of randomness. For example, if a student was in the treatment group, this model would state that the student's delinquency score is a function of the marginal mean delinquency level, an effect of being in the treatment group, and some amount of randomness. If, on the other hand, the student was in the control group, this model would state that the student's delinquency score is a function of the marginal mean delinquency level, an effect of being in the control group, and some amount of randomness.

In this model, parameters with a $j$-subscript vary across groups but not within groups. For example the group effect ($\alpha_j$) would be the same value for every member of the control group. Similarly this parameter would be the same value for every member of the treatment group. But, the parameter values for control and treatment groups could be different.

If the parameter has both an $i$- and a $j$-subscript, it means that the parameter value can vary for individual within the same group. For example students in the control group could have different outcome values and different random error values. Lastly, if the parameter has no subscripts ($\mu$), it means that the parameter value is the same for all individuals in all groups.



### Statistical Hypotheses and the Statistical Model

Recall the two competing hypotheses underlying the statistical inference:

- There is *no effect of treatment*; the difference in sample means is due to the random assignment.
- There is an *effect of treatment*; the difference in sample means is due to something more than just the random assignment.

The first hypothesis of no effect of treatment specifies that the $\alpha_j$ parameter in the statistical model is 0. If $\alpha_j=0$, then the model reduces to:

$$
\begin{split}
Y_{ij} &= \mu + 0 + \epsilon_{ij} \\
&= \mu + \epsilon_{ij}
\end{split}
$$

This model states that regardless of whether a student is in the control or the treatment group their delinquency score is a function of the marginal mean and random error. Since the only parameter that varies on the right-hand side of this equation is $\epsilon_{ij}$, this implies that the only reason there are differences in students' delinquency scores is because of random error.

In the other competing hypothesis, that there is an effect of treatment, $\alpha_j \neq 0$. That corresponds to the full statistical model presented earlier:

$$
Y_{ij} = \mu + \alpha_j + \epsilon_{ij}
$$

In this model there are two parameters that vary on the right-hand side of this equation, namely $\alpha_j$ and $\epsilon_{ij}$, this implies that the reason there are differences in students' delinquency scores is because of both a group effect and random error.

This is to say that we can evaluate the two competing hypotheses by focusing on the parameter $\alpha_j$. Mathematically we can express these hypotheses as:

$$
\begin{split}
H_0:&~ \alpha_j=0 \\
H_1:&~\alpha_j \neq 0
\end{split}
$$

The first mathematical hypothesis ($H_0$) is referred to as the *null hypothesis* and corresponds to the hypothesis that there is no effect of treatment. The second hypothesis ($H_1$) is referred to as the *alternative hypothesis* and corresponds to the hypothesis that there is an effect of treatment.

<br /><br />

### Another Expression of the Hypotheses

When we first presented the statistical model, we defined $\alpha_j$ as the difference between the group mean and the marginal mean,

$$
\alpha_j = \mu_j − \mu
$$

where $\mu_j$ is the mean for group $j$ and $\mu$ is the marginal mean. If we substitute this into the statistical model we get,

$$
\begin{split}
Y_{ij} &= \mu + \alpha_j + \epsilon_{ij} \\
&= \mu + \mu_j − \mu + \epsilon_{ij} \\
&= \mu_j + \epsilon_{ij}
\end{split}
$$

This expression of the model states that students' delinquency scores are a function of their group mean and individual random error. 

Compare this model (which corresponds to the alternative hyptheis) with the reduced model (corresponding to the null hypothesis):

$$
\begin{split}
\mathbf{Null~Model:}&~~Y_{ij} = \mu + \epsilon_{ij} \\
\mathbf{Alternative~Model:}&~~Y_{ij} = \mu_j + \epsilon_{ij}
\end{split}
$$

These two models are quite similar in that both say students' delinquency scores arew a function of a mean and individual random error. The difference is whether the mean is the same for both groups ($\mu$ in the null model) or is allowed to vary by group ($\mu_j$ in the alternative model). Taking advantage of this equivalence (or non-equivalence) of group means we can mathematically express the null and alternative hypotheses using means as:

$$
\begin{split}
H_0:&~ \mu_{\mathrm{Treatment}} = \mu_{\mathrm{Control}}\\
H_1:&~ \mu_{\mathrm{Treatment}} \neq \mu_{\mathrm{Control}}
\end{split}
$$

We can also express these two competing hypotheses as the difference in means as:

$$
\begin{split}
H_0:&~ \mu_{\mathrm{Treatment}} - \mu_{\mathrm{Control}} = 0\\
H_1:&~ \mu_{\mathrm{Treatment}} - \mu_{\mathrm{Control}} \neq 0
\end{split}
$$

This last expression makes it clear why we looked at the sample difference in means as evidence of group differences. The differrence of 6.9 we observed in the sample data appears to support the alternative hypothesis (at least prior to formally testing it). Since all three forms of the mathematical hypotheses are equivalent, it doesn't matter which version we use in practice. 

<br /><br />

## Testing the Null Hypothesis

We begin by assuming that the null hypothesis is true ($H_0:~\mu_{\mathrm{Treatment}} - \mu_{\mathrm{Control}} = 0$), that there are no population differences between the control and treatment groups in the average amount of delinquent behavior. Remember, this model implies that the sample differences we observed are completely due to chance, not to any real improvement by the after school program. This assumption makes it mathematically easier to carry out a statistical test.

The sample data give us an estimate of this difference,

$$
\begin{split}
\hat\mu_{\mathrm{Treatment}} - \hat\mu_{\mathrm{Control}} &= 48.0 - 54.9\\
& = -6.9
\end{split}
$$

Is this difference more than we expect because of sampling error? To find out, we need to determine how big of a difference we would expect in the sample means if the null hypothesis was true. 

To understand this idea, we are going to carry out a thought experiment. Imagine you have two populations of delinquency scores that are both infinitely large. One of these populations will be labelled "Control" and the other "Treatment". The observations in both populations follow some probability distribution. (This distribution is typically unknown in practice, but for now, let's pretend we know what that distribution is.) For our purposes, let's assume both populations are normally distributed with a mean of $\mu$ and a standard deviation of $\sigma$.

Since the mean value is the same in both populations, they correspond to the null hypothesis being true, that there is no difference in the average values between treatment scores and control scores. The assumptions that both distributions are normally distributed and both have the same standard deviation are simplifying assumptions that we employ. (In practice these may or may not be true and need to be examined to ensure that our results are valid!) 

The astute reader may realize that under the null hypothesis, the control and teatment populations are identical; after all they have the same shape (normal), the same mean value and the same standard deviation value. If the alternative hypothesis is true, on the other hand, the means of the two populations would be different and the two populations would be distinct (see Figure \@ref(fig:fig-02)). When researchers compare two groups, they are essentially asking the statistical question of whether the two samples were drawn from two distinct populations.

<div class="figure" style="text-align: center">
<img src="06-inference_files/figure-html/fig-02-1.png" alt="LEFT: Graphical depiction of the control and treatment populations of delinqunecy scores under the null hypothesis. RIGHT: Oone possible graphical depiction of the control and treatment populations of delinqunecy scores under the alternative hypothesis." width="50%" />
<p class="caption">(\#fig:fig-02)LEFT: Graphical depiction of the control and treatment populations of delinqunecy scores under the null hypothesis. RIGHT: Oone possible graphical depiction of the control and treatment populations of delinqunecy scores under the alternative hypothesis.</p>
</div>

The thought experiment asks us to imagine that the null hypothesis is true and to sample observations from that population, and create two samples with those observations, a "control" group and a "treatment" group. To mimic the original experiment, we will make these groups the same size as those in the observed data; namely we will sample 41 scores for the "control" group and 37 scores for the "treatment" group.

We will then compute the mean for each group of sampled scores and find the difference between the two mean values, in the same fashion we did for the observed data. In all likelihood, this difference will not be 0 (the difference in the actual population. This variation from the population difference of 0 is not because there is any effect of treatment, we assumed there was none, but solely attributable to sampling error.

Repeat this process; sample another 41 observations from the population (assuming  the null hypothesis is true) for the "control" group and 37 observations for the "treatment" group and find the mean difference. Again, this difference probably varies from the population difference of 0, and may be different from the first mean difference we obtained as well. Continue to repeat this process:

- Randomly sample 41 observations for the "control" group,
- Randomly sample 37 observations for the "treatment" group, and
- Find the difference in means between the two groups.

If we were to collect and plot all of these different differences in means, we would get what is called a *sampling distribution*^[A sampling distribution is a distribution of statistics. In other words, each "observation" in this distribution is a statistic or sample estimate, in our case the sampling distribution is composed of differences in means.]. The sampling distribution for our differences in means has the following properties,

- It is normally distributed.
- It has a mean of 0.
- It has a standard deviation that is a function of the population variance and the two sample sizes.




