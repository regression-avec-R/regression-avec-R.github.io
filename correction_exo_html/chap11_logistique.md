Chapitre 11 : régression logistique
================

# Essai

## Exercice 1

1.  A
2.  A
3.  B
4.  A
5.  A
6.  A
7.  B
8.  A
9.  C
10. D
11. A, B
12. C

## Exercice 2

1.  
<!-- end list -->

``` r
n <- 100
set.seed(48967365)
X <- sample(c("A","B","C"),100,replace=TRUE)
Y <- rep(0,n)
set.seed(487365)
Y[X=="A"] <- rbinom(sum(X=="A"),size=1,prob=0.95)
set.seed(4878365)
Y[X=="B"] <- rbinom(sum(X=="B"),size=1,prob=0.95)
set.seed(4653965)
Y[X=="C"] <- rbinom(sum(X=="C"),size=1,prob=0.05)
Y <- factor(Y)
donnees<-data.frame(Y,X)
```

2.  
<!-- end list -->

``` r
model1 <- glm(Y~X,data=donnees,family=binomial)
summary(model1)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ X, family = binomial, data = donnees)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4267  -0.2561   0.3288   0.4512   2.6207  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   2.2336     0.6075   3.677 0.000236 ***
    ## XB            0.6568     0.9470   0.694 0.487977    
    ## XC           -5.6348     1.1842  -4.758 1.95e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 129.489  on 99  degrees of freedom
    ## Residual deviance:  44.218  on 97  degrees of freedom
    ## AIC: 50.218
    ## 
    ## Number of Fisher Scoring iterations: 6

On obtient les résultats du **test de Wald** sur la nullité des
paramètres \(\beta_0,\beta_2\) et \(\beta_3\).

3.  
<!-- end list -->

``` r
model2 <- glm(Y~C(X,base=3),data=donnees,family=binomial)
summary(model2)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ C(X, base = 3), family = binomial, data = donnees)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4267  -0.2561   0.3288   0.4512   2.6207  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -3.401      1.017  -3.346  0.00082 ***
    ## C(X, base = 3)1    5.635      1.184   4.758 1.95e-06 ***
    ## C(X, base = 3)2    6.292      1.249   5.035 4.77e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 129.489  on 99  degrees of freedom
    ## Residual deviance:  44.218  on 97  degrees of freedom
    ## AIC: 50.218
    ## 
    ## Number of Fisher Scoring iterations: 6

On obtient les résultats du **test de Wald** sur la nullité des
paramètres \(\beta_0,\beta_1\) et \(\beta_2\).

4.  On remarque que dans **model1** on accepte la nullité de \(\beta_2\)
    alors qu’on la rejette dans **model2**. Ceci est logique dans la
    mesure où ces tests dépendent de la contrainte identifiante choisie.
    Dans **model1** le test de nullité de \(\beta_2\) permet de vérifier
    si \(B\) à un effet similaire à \(A\) sur \(Y\). Dans **model2**, on
    compare l’effet de \(B\) à celui de \(C\). On peut donc conclure
    \(A\) et \(B\) ont des effets proches sur \(Y\) alors que \(B\) et
    \(C\) ont un impact différent. Ceci est logique vu la façon dont les
    données ont été générées.

5.  Tester l’effet global de \(X\) sur \(Y\) revient à tester si les
    coefficients \(\beta_1,\beta_2\) et \(\beta_3\) sont égaux, ce qui,
    compte tenu des contraintes revient à considérer les hypothèses
    nulles :

<!-- end list -->

  - \(\beta_2=\beta_3=0\) dans **model1** ;
  - \(\beta_1=\beta_2=0\) dans **model2**.

On peut effectuer les tests de **Wald** ou du **rapport de
vraisemblance**. On obtient les résultats du **rapport de
vraisemblance** avec :

``` r
library(car)
Anova(model1,type=3,test.statistic="LR")
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: Y
    ##   LR Chisq Df Pr(>Chisq)    
    ## X   85.271  2  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Anova(model2,type=3,test.statistic="LR")
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: Y
    ##                LR Chisq Df Pr(>Chisq)    
    ## C(X, base = 3)   85.271  2  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

On remarque ici que ces deux tests sont identiques : ils ne dépendent
pas de la contrainte identifiante choisie.

## Exercice 3

1.  
<!-- end list -->

``` r
set.seed(1234)
X <- c(runif(50,-1,0),runif(50,0,1))
set.seed(5678)
Y <- c(rep(0,50),rep(1,50))
df <- data.frame(X,Y)
```

2.  
<!-- end list -->

``` r
beta <- seq(0,100,by=0.01)
log_vrais <- function(X,Y,beta){
  LV <- rep(0,length(beta))
  for (i in 1:length(beta)){
    Pbeta <- exp(beta[i]*X)/(1+exp(beta[i]*X))
    LV[i] <- sum(Y*X*beta[i]-log(1+exp(X*beta[i])))
#    gradln[i] <- t(Xb)%*%(Yb-Pbeta)
  }
  return(LV)
}
LL <- log_vrais(X,Y,beta)
plot(beta,LL,type="l")
```

![](chap11_logistique_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

3.  
<!-- end list -->

``` r
model <- glm(Y~X-1,data=df,family="binomial")
```

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
model$coef
```

    ##        X 
    ## 1999.371

On obtient un avertissement qui nous dit que l’algorithme d’optimisation
n’a pas convergé.

4.  
<!-- end list -->

``` r
Y1 <- Y;Y1[1] <- 1
LL1 <- log_vrais(X,Y1,beta)
plot(beta,LL1,type="l")
```

![](chap11_logistique_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
model1 <- glm(Y1~X-1,family="binomial")
model1$coef
```

    ##        X 
    ## 10.17868

## Exercice 4

## Exercice 5

On importe les données

``` r
panne <- read.table("panne.txt",header=T)
head(panne)
```

    ##   etat age marque
    ## 1    0   4      A
    ## 2    0   2      C
    ## 3    0   3      C
    ## 4    0   9      B
    ## 5    0   7      B
    ## 6    0   6      A

1.  La commande

<!-- end list -->

``` r
model <- glm(etat~.,data=panne,family=binomial)
model
```

    ## 
    ## Call:  glm(formula = etat ~ ., family = binomial, data = panne)
    ## 
    ## Coefficients:
    ## (Intercept)          age      marqueB      marqueC  
    ##     0.47808      0.01388     -0.41941     -1.45608  
    ## 
    ## Degrees of Freedom: 32 Total (i.e. Null);  29 Residual
    ## Null Deviance:       45.72 
    ## Residual Deviance: 43.5  AIC: 51.5

ajuste le
modèle

\[\log\left(\frac{p_\beta(x)}{1-p_\beta(x)}\right)=\beta_0+\beta_1x_1+\beta_2\mathsf{1}_{x_2=B}+\beta_3\mathsf{1}_{x_2=C}\]

où \(x_1\) et \(x_2\) désigne respectivement les variables **age** et
**marque**. On obtient les estimateurs avec

``` r
coef(model)
```

    ## (Intercept)         age     marqueB     marqueC 
    ##  0.47808311  0.01388395 -0.41941071 -1.45608147

2.  
Il s’agit des tests de Wald pour tester l’effet des variables dans le
modèle. Pour l’effet de marque, on va par exemple tester
\[H_0:\beta_2=\beta_3=0\quad\text{contre}\quad H_1:\beta_2\neq 0\text{ ou }\beta_3\neq 0.\]
Sous \(H_0\) la statistique de Wald suit une loi du \(\chi^2\) à 4-2=2
degrés de liberté. Pour le test de la variable **age** le nombre de
degrés de liberté manquant est 1. On retrouve cela dans la sortie

``` r
library(car)
Anova(model,type=3,test.statistic="Wald")
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: etat
    ##             Df  Chisq Pr(>Chisq)
    ## (Intercept)  1 0.3294     0.5660
    ## age          1 0.0218     0.8826
    ## marque       2 1.9307     0.3809

3.  Il s’agit cett fois du test du rapport de vraisemblance. Les degrés
    de liberté manquants sont identiques.

<!-- end list -->

``` r
Anova(model,type=3,test.statistic="LR")
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: etat
    ##        LR Chisq Df Pr(>Chisq)
    ## age     0.02189  1     0.8824
    ## marque  2.09562  2     0.3507

4.  a). Le modèle s’écrit
    \[\log\left(\frac{p_\beta(x)}{1-p_\beta(x)}\right)=\beta_0+\beta_1\mathsf{1}_{x_2=A}+\beta_2\mathsf{1}_{x_2=B}.\]

b). Le modèle ajusté ici est
\[\log\left(\frac{p_\beta(x)}{1-p_\beta(x)}\right)=\gamma_0+\gamma_1\mathsf{1}_{x_2=B}+\gamma_2\mathsf{1}_{x_2=C}.\]

Par identification on a \[\begin{cases}
\beta_0+\beta_1=\gamma_0 \\
\beta_0+\beta_2=\gamma_0+\gamma_1 \\
\beta_0=\gamma_0+\gamma_2 \\
\end{cases}
\Longleftrightarrow
\begin{cases}
\beta_0=\gamma_0+\gamma_2 \\
\beta_1=-\gamma_2 \\
\beta_2=\gamma_1-\gamma_2 \\
\end{cases}
\Longrightarrow
\begin{cases}
\widehat\beta_0=-0.92 \\
\widehat\beta_1=1.48 \\
\widehat\beta_2=1.05 \\
\end{cases}\]

On peut retrouver ces résultats avec

``` r
glm(etat~C(marque,base=3),data=panne,family="binomial")
```

    ## 
    ## Call:  glm(formula = etat ~ C(marque, base = 3), family = "binomial", 
    ##     data = panne)
    ## 
    ## Coefficients:
    ##          (Intercept)  C(marque, base = 3)1  C(marque, base = 3)2  
    ##              -0.9163                1.4759                1.0498  
    ## 
    ## Degrees of Freedom: 32 Total (i.e. Null);  30 Residual
    ## Null Deviance:       45.72 
    ## Residual Deviance: 43.52     AIC: 49.52

5.  Il y a interaction si l’age agit différemment sur la panne en
    fonction de la marque.

6.
