                                        # CHD

## Les données
library(bestglm)
data(SAheart)
maladie <- SAheart
maladie$chd <- as.factor(maladie$chd)
dim(maladie)
table(maladie$chd)
## Méthodes et comparaison
SCORE <- data.frame(Y = maladie$chd)

nbbloc <- 10
bloc <- 1:nrow(maladie)
ind0 <- which(maladie$chd==0)
ind1 <- which(maladie$chd==1)

set.seed(1234)
bloc[ind0] <- sample(rep(1:nbbloc, length = length(ind0)))
bloc[ind1] <- sample(rep(1:nbbloc, length = length(ind1)))

models <- list()
for(i in 1:nbbloc){
  print(i)
  maladieA <- maladie[bloc!=i,]
  maladieT <- maladie[bloc==i,]
  ###Logistique global
  reglog <- glm(chd~.,data=maladieA,family="binomial")
  SCORE[bloc==i,"glm"] <- predict(reglog,maladieT,type="response")
  ###Logistique choix
  choix <- bestglm(maladieA,family=binomial)
  SCORE[bloc==i,"choix"] <- predict(choix$BestModel,maladieT,type="response")
  tmp <- choix$BestModels[1, -ncol(maladie)]
  var <- names(tmp)[tmp==TRUE]
  models[[i]] <- var
}

## Lasso, ridge et elasticnet
maladie.X <- model.matrix(chd~.,data=maladie)[,-1]
maladie.Y <- maladie[,"chd"]
library(glmnet)
for(i in 1:nbbloc){
  print(i)
  XA <- maladie.X[bloc!=i,]
  YA <- maladie.Y[bloc!=i]
  XT <- maladie.X[bloc==i,]
  ### ridge
  mod <- cv.glmnet(XA,YA,alpha=0,family="binomial")
  SCORE[bloc==i,"ridge"] <- as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ### lasso
  mod <- cv.glmnet(XA,YA,alpha=1,family="binomial")
  SCORE[bloc==i,"lasso"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))
  ### elast
  mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial")
  SCORE[bloc==i,"elast"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))   
}


SCORE[1:4,]

#taux de mal classé
tauxMC  <- function(X,Y,seuil){
    Xc <- X*0
    Xc[X>=seuil] <- 1
    return(sum(Xc!=Y))
}
round(apply(SCORE[,-1],2,tauxMC,Y=SCORE$Y,seuil=.5)/nrow(SCORE)*100,2)


for(i in 1:nbbloc){
  print(i)
  XA <- maladie.X[bloc!=i,]
  YA <- maladie.Y[bloc!=i]
  XT <- maladie.X[bloc==i,]
  ### ridge measure class
  mod <- cv.glmnet(XA,YA,alpha=0,family="binomial",type.measure="class")
  SCORE[bloc==i,"ridgeC"] <- as.vector(predict(mod,XT, "lambda.1se", type="response")) 
  ### lasso measure class
  mod <- cv.glmnet(XA,YA,alpha=1,family="binomial",type.measure="class")
  SCORE[bloc==i,"lassoC"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response")) 
  ### elast measure class
  mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial",type.measure="class")
  SCORE[bloc==i,"elastC"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))  
  ### ridge measure AUC
  mod <- cv.glmnet(XA,YA,alpha=0,family="binomial",type.measure="auc")
  lridgeAUC[i] <- mod$lambda.1se
  SCORE[bloc==i,"ridgeA"] <- as.vector(predict(mod,XT, "lambda.1se", type="response"))
  ### lasso
  mod <- cv.glmnet(XA,YA,alpha=1,family="binomial",type.measure="auc")
  llassoAUC[i] <- mod$lambda.1se
  SCORE[bloc==i,"lassoA"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))
  ### elast
  mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial",type.measure="auc")
  lelastAUC[i] <- mod$lambda.1se
  SCORE[bloc==i,"elastA"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response")) 
}


library(pROC)
rocCV <- roc(Y~.,data=SCORE)
sort(round(unlist(lapply(rocCV,auc)),3),decreasing=TRUE)[1:6]

ind <- order(aucmodele,decreasing=TRUE)
mapply(plot,rocCV[ind[1:4]],col=1:4,lty=1:4,
       lwd=3,legacy.axes=T,add=c(F,T,T,T))
legend("bottomright",legend=names(SCORE)[ind[1:4]],col=1:4,
       lty=1:4,lwd=3,cex=1)

tmp <- lapply(rocCV, FUN=coords, x="best", ret=c("threshold","tp","fp",
              "fn","tn","sensitivity","specificity", "ac"), transpose = TRUE)
mat <- do.call(rbind,tmp)
round(mat,3)

## Pour aller plus loin
### Stabilité des résultats
library(glmnet) ; library(bestglm)
data(SAheart)
maladie <- SAheart
maladie$chd <- as.factor(maladie$chd)
maladie.X <- model.matrix(chd~.,data=maladie)[,-1]
maladie.Y <- maladie[,"chd"]
ind0 <- which(maladie$chd==0)
ind1 <- which(maladie$chd==1)
niter <- 20
SCORE <- array(0,dim=c(nrow(maladie),11,niter),
               dimnames=list(1:nrow(maladie),
                             c("glm","choix","ridge","lasso","elast",
                               "ridgeC","lassoC","elastC",
                               "ridgeA","lassoA","elastA"),
                             1:niter))
nbbloc <- 10
bloc <- rep(0, nrow(maladie))
for(j in 1:niter){
print(j)
set.seed(1233+j)
bloc[ind0] <- sample(rep(1:nbbloc, length = length(ind0)))
bloc[ind1] <- sample(rep(1:nbbloc, length = length(ind1)))
for(i in 1:nbbloc){
  print(i)
  maladieA <- maladie[bloc!=i,]
  maladieT <- maladie[bloc==i,]
  ## Logistique global
  reglog <- glm(chd~.,data=maladieA,family="binomial")
  SCORE[bloc==i,"glm",j] <- predict(reglog,maladieT,type="response")
  ## Logistique choix
  choix <- bestglm(maladieA,family=binomial)
  SCORE[bloc==i,"choix",j] <- predict(choix$BestModel, maladieT, type="response")
  ##
  XA <- maladie.X[bloc!=i,]
  YA <- maladie.Y[bloc!=i]
  XT <- maladie.X[bloc==i,]
  ## ridge
  mod <- cv.glmnet(XA,YA,alpha=0,family="binomial")
  SCORE[bloc==i,"ridge",j] <- as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ## lasso
  mod <- cv.glmnet(XA,YA,alpha=1,family="binomial")
  SCORE[bloc==i,"lasso",j] <-  as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ## elast
  mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial")
  SCORE[bloc==i,"elast",j] <-  as.vector(predict(mod, XT, "lambda.1se", type="response"))  
  ## ridge
  mod <- cv.glmnet(XA,YA,alpha=0,family="binomial",type.measure="class")
  SCORE[bloc==i,"ridgeC",j] <- as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ## lasso
  mod <- cv.glmnet(XA,YA,alpha=1,family="binomial",type.measure="class")
  SCORE[bloc==i,"lassoC",j] <-  as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ## elast
  mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial",type.measure="class")
  SCORE[bloc==i,"elastC",j] <-  as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ## ridge
  mod <- cv.glmnet(XA,YA,alpha=0,family="binomial",type.measure="auc")
  SCORE[bloc==i,"ridgeA",j] <- as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ## lasso
  mod <- cv.glmnet(XA,YA,alpha=1,family="binomial",type.measure="auc")
  SCORE[bloc==i,"lassoA",j] <-  as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ## elast
  mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial",type.measure="auc")
  SCORE[bloc==i,"elastA",j] <-  as.vector(predict(mod, XT, "lambda.1se", type="response"))
}
}


library(pROC)
res <- matrix(0,nrow=niter,ncol=5)
colnames(res) <- dimnames(SCORE)[[2]]
for(j in 1:niter){
    tmp <- SCORE[,,j]
    res[j,]  <- apply(tmp, 2, auc, response=maladie$chd)
}
round(apply(res, 2, mean), 3)

                                        

boxplot(res,main="Distribution des AUC par méthode")

## Créations de variables
### Modèle de prévision avec interactions
##########################
SCOREint <- data.frame(Y = maladie$chd)
maladie.X <- model.matrix(chd~.^2,data=maladie)[,-1]
maladie.chd <- maladie[,"chd"]
for(i in 1:nbbloc){
  print(i)
  maladieA <- maladie[bloc!=i,]
  maladieT <- maladie[bloc==i,]
  ###Logistique global
  reglog <- glm(chd~.^2,data=maladieA,family="binomial")
  SCOREint[bloc==i,"glm"] <- predict(reglog,maladieT,type="response")
  ###Logistique choix
  choix <- step(reglog,trace=0)
  SCOREint[bloc==i,"choix"] <- predict(choix,maladieT,type="response")
  ###
  XA <- maladie.X[bloc!=i,]
  YA <- maladie.chd[bloc!=i]
  XT <- maladie.X[bloc==i,]
  ###ridge
    mod <- cv.glmnet(XA,YA,alpha=0,family="binomial")
    lridge[i] <- mod$lambda.1se
    SCOREint[bloc==i,"ridge"] <- as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ### lasso
    mod <- cv.glmnet(XA,YA,alpha=1,family="binomial")
    llasso[i] <- mod$lambda.1se
    SCOREint[bloc==i,"lasso"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))
   ### elast
    mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial")
    lelast[i] <- mod$lambda.1se
    SCOREint[bloc==i,"elast"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))   
}
rocCVint <- roc(Y~., data=SCOREint)
sort(round(unlist(lapply(rocCVint,auc)),3),decreasing=TRUE)

## Modèle de prévision avec des polynômes
SCOREpoly <- data.frame(Y = maladie$chd)
typevar <- sapply(maladie,class) 
X <- maladie[,typevar!="factor"]
maladieP <- data.frame(maladie[,1:9],X^2,X^3,chd=maladie$chd)
maladie.X <- model.matrix(chd~.,data=maladieP)
maladie.Y <- maladie[,"chd"]
for(i in 1:nbbloc){
  print(i)
  maladieA <- maladieP[bloc!=i,]
  maladieT <- maladieP[bloc==i,]
  ###Logistique global
  reglog <- glm(chd~.,data=maladieA,family="binomial")
  SCOREpoly[bloc==i,"glm"] <- predict(reglog,maladieT,type="response")
  ###Logistique choix
  choix <- step(reglog,trace=0)
  SCOREpoly[bloc==i,"choix"] <- predict(choix,maladieT,type="response")
  ###
  XA <- maladie.X[bloc!=i,]
  YA <- maladie.Y[bloc!=i]
  XT <- maladie.X[bloc==i,]
  ###ridge
    mod <- cv.glmnet(XA,YA,alpha=0,family="binomial")
    lridge[i] <- mod$lambda.1se
    SCOREpoly[bloc==i,"ridge"] <- as.vector(predict(mod, XT, "lambda.1se", type="response"))
  ### lasso
    mod <- cv.glmnet(XA,YA,alpha=1,family="binomial")
    llasso[i] <- mod$lambda.1se
    SCOREpoly[bloc==i,"lasso"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))
   ### elast
    mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial")
    lelast[i] <- mod$lambda.1se
    SCOREpoly[bloc==i,"elast"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))
}
rocCVpoly <- roc(Y~., data=SCOREpoly)
sort(round(unlist(lapply(rocCVpoly,auc)),3),decreasing=TRUE)

## Modèle de prévision avec des splines
xyplot(y~x|z,data=rrdt,scale=list(x="free"),
       panel = function(x, y) {
           panel.xyplot(x, y, pch=3, cex=0.5)
           smooth <- smooth.spline(x, y)
           panel.lines(x = smooth$x, y = smooth$y)
       })

xyplot(y~x,data=rrdt[rrdt[,"z"]=="tobacco",],scale=list(x="free"),
       panel = function(x, y) {
           panel.xyplot(x, y, pch=3)
           smooth <- smooth.spline(x, y)
           panel.lines(x = smooth$x, y = smooth$y)
           panel.abline(v=c(2.5,5,7.5,10),lty=2)
       })

deg <- 2
xi <- c(2.5,5,7.5,10)
K <- length(xi)
var <- maladie$tobacco
BX1 <- bs(var,knots=xi,degre=deg,
                Boundary.knots=c(min(var),max(var)),intercept =F)
colnames(BX1) <- paste("tabac","b",1:(K+deg),sep="")
maladieS1 <- cbind.data.frame(maladie,BX1)
maladieS1.X <- model.matrix(chd~.,data=maladieS1)[,-1]
maladieS1.Y <- maladieS[,"chd"]
####

SCOREspline <- data.frame(Y = maladieS1$chd)
formule1 <- paste("chd~", paste(colnames(BX1),collapse="+"),"+ldl+famhist+typea+age")
for(i in 1:nbbloc){
  print(i)
  maladieSA <- maladieS1[bloc!=i,]
  maladieST <- maladieS1[bloc==i,]
  ###Logistique global
  reglog <- glm(chd~.,data=maladieSA,family="binomial")
  SCOREspline[bloc==i,"glm_bs1"] <- predict(reglog,maladieST,type="response")

   ###Logistique sel
  reglogc <- glm(formula(formule1),data=maladieSA,family="binomial")
  SCOREspline[bloc==i,"glm_bs1_restr"] <- predict(reglogc,maladieST,type="response")

  ### Logistique choix
  choix <- step(reglog,trace=0)
  SCOREspline[bloc==i,"choix_bs1"] <- predict(choix,maladieST,type="response")


  ###
  XA <- maladieS1.X[bloc!=i,]
  YA <- maladieS1.Y[bloc!=i]
  XT <- maladieS1.X[bloc==i,]
  ###ridge
    mod <- cv.glmnet(XA,YA,alpha=0,family="binomial")
    SCOREspline[bloc==i,"ridge_bs1"] <- as.vector(predict(mod,XT, "lambda.1se", type="response"))
  #### lasso
    mod <- cv.glmnet(XA,YA,alpha=1,family="binomial")
    SCOREspline[bloc==i,"lasso_bs1"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))
  #### elast
    mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial")
    SCOREspline[bloc==i,"elast_bs1"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))   

}
rocspline <- roc(Y~.,data=SCOREspline)
sort(round(unlist(lapply(rocspline,auc)),3),decreasing=TRUE)

### seconde base
xi <- c(5,7.5,10)
K <- length(xi)
BX2 <- bs(var,knots=xi,degre=deg,
                Boundary.knots=c(min(var),max(var)),intercept =F)
colnames(BX2) <- paste("tabac","b",1:(K+deg),sep="")
maladieS2 <- cbind.data.frame(maladie,BX2)
maladieS2.X <- model.matrix(chd~.,data=maladieS2)[,-1]
maladieS2.Y <- maladieS[,"chd"]


SCOREspline2 <- data.frame(Y = maladieS2$chd)
formule2 <- paste("chd~", paste(colnames(BX2),collapse="+"),"+ldl+famhist+typea+age")
for(i in 1:nbbloc){
  print(i)
  maladieSA <- maladieS2[bloc!=i,]
  maladieST <- maladieS2[bloc==i,]
  ###Logistique global
  reglog <- glm(chd~.,data=maladieSA,family="binomial")
  SCOREspline2[bloc==i,"glm_bs2"] <- predict(reglog,maladieST,type="response")

   ###Logistique sel
  reglogc <- glm(formula(formule2),data=maladieSA,family="binomial")
  SCOREspline2[bloc==i,"glm_bs2_restr"] <- predict(reglogc,maladieST,type="response")
}
rocspline2 <- roc(Y~.,data=SCOREspline2)
sort(round(unlist(lapply(rocspline2,auc)),3),decreasing=TRUE)



####################################################
#### Publicité
####################################################
                                        # Application : détection d'images publicitaires sur internet
## Les données
pub  <- read.table("ad_data.txt", header = FALSE, sep = ",",
                 dec = ".", na.strings = "?", strip.white = TRUE)
names(pub)[ncol(pub)] <- "Y"
table(pub$Y)

pub$Y <- factor(pub$Y,labels=c("1","0"))
pub$Y <- relevel(pub$Y, "0")
pub <- na.omit(pub)
table(pub$Y)

######## bloc
ind0 <- which(pub$Y==0)
ind1 <- which(pub$Y==1)
set.seed(1234)
nbbloc <- 10
bloc <- 1:nrow(pub)
bloc[ind0] <- sample(rep(1:nbbloc, length = length(ind0)))
bloc[ind1] <- sample(rep(1:nbbloc, length = length(ind1)))

library(glmnet)
SCORE <- data.frame(Y = pub$Y)
pub.X <- model.matrix(Y~.,data=pub)[,-1]
pub.Y <- pub[,"Y"]
for(i in 1:nbbloc){
    print(i)
    pubA <- pub[bloc!=i,]
    pubT <- pub[bloc==i,]
    ## Logistique global
    reglog <- glm(Y~.,data=pubA,family="binomial")
    SCORE[bloc==i,"glm"] <- predict(reglog,pubT,type="response")
    ## 
    XA <- pub.X[bloc!=i,]
    YA <- pub.Y[bloc!=i]
    XT <- pub.X[bloc==i,]
    ## ridge
    mod <- cv.glmnet(XA,YA,alpha=0,family="binomial")
    SCORE[bloc==i,"ridge"] <- as.vector(predict(mod, XT, "lambda.1se", type="response"))
    ##  lasso
    mod <- cv.glmnet(XA,YA,alpha=1,family="binomial")
    SCORE[bloc==i,"lasso"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))
    ##  elast
    mod <- cv.glmnet(XA,YA,alpha=.5,family="binomial")
    SCORE[bloc==i,"elast"] <-  as.vector(predict(mod,XT, "lambda.1se", type="response"))  
}
