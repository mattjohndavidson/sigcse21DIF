## This code shows how to run a DIF analysis for a sample dataset
## including evaluating basic item statistics and IRT assumptions

library(tidyverse)
library(mirt)
library(psych)
library(lavaan)
library(difR)

#add description of verbal dataset, from the DifR package
data(verbal)
dat <- verbal %>% dplyr::select(-Anger)
items <- colnames(dat[1:24])

#create df with just item responses
itemsonly <- dat %>% dplyr::select(-Gender)

## Basic Item statistics

#item difficulty
item_descriptives <- describe(itemsonly) %>% as.data.frame()
itemstats <- tibble(.rows=length(items))
itemstats$item <- items
itemstats$difficulty <- item_descriptives$mean

#adding discrimination and reliability
items_alpha <- alpha(itemsonly)
itemstats$discrimination <- items_alpha$item.stats$r.cor
itemstats$alpha_drop <- items_alpha$alpha.drop$raw_alpha
colnames(itemstats) <- c("Difficulty","Item-total Correlation", "Alpha Drop")

print(itemstats, n=24)

## More on reliability
#the function alpha() returns an overall Cronbach's alpha, as well as how alpha would change if each item were excluded.
#It also includes item-total correlations, in the 'r.cor' column.
#above, the item-total correlations and alpha drop values were stored in itemstats
alpha(itemsonly)$total

#splitHalf gives various split-half reliabilities. These help contextualize the alpha result
#note that alpha is equivalent to Guttman lambda 3. 
splitHalf(itemsonly)

#Finally McDonalds omega is suggested as a better measure of reliability than either alpha or glb.
#only the value in Omega Total is relevant, since we assume this exam to have one factor only.
omega(itemsonly,fm="pa",nfactors=1)


### Evaluate IRT assumptions ###

### Test the dimensionality using CFA ###
items_rhs <- paste(items,collapse=" +")
one_d_model <- paste0("factor =~ ",items_rhs)
cfa_1dfit <- cfa(one_d_model,data=itemsonly,std.lv=T)
summary(cfa_1dfit,fit.measures=T)
#these show that a single factor model is not a good fit for this data.
#even if it showed good fit, I'd compare it to another factor structure
#either based on exploratory analysis or ideally based on knowledge of the items/construct
#then the two models can be compared directly with a likelihood ratio test
#anova(model_object_1, model_object_2)

### Functional Form ###
#this lets you check the functional form assumption of IRT models before using an IRT DIF method
#it is advised to fit a few models and compare the model fit
twopl_fit <- mirt(itemsonly,model=1,itemtype="2PL",SE=T)
threepl_fit <- mirt(itemsonly,model=1,itemtype="3PL",SE=T)

#using the coef command lets you inspect the item parameter estimates
coef(twopl_fit,IRTpars=T,simplify=T,printSE=T)
coef(threepl_fit,IRTpars=T,simplify=T,printSE=T)

#comparing the model fits
anova(twopl_fit,threepl_fit)
#for this data, a 3PL seems to be a better fit, because of the significant chi-square test
#for wider applicability, I use a 2PL below, since the 2PL fits most test data well

### Logistic Regression DIF ###
difLogistic(dat,group="Gender",focal.name=1,purify=T,p.adjust.method = "BH",type="udif")

PVAL <- NA #this value is only set due to a bug in the difR package with the verbal dataset.
#most likely, when running this analysis on another dataset, this value will not need to be set. 

difLogistic(dat,group="Gender",focal.name=1,purify=T,p.adjust.method = "BH",type="nudif")
#it's also possible to store the result of the calls above as an object
#this makes creating tables of results easier


## LRT DIF ##
#run using the mirt package
#requires fitting a `multipleGroup` IRT model to the data, which needs a character vector of group membership
gender_groups <- as.character(dat$Gender)

#fitting the model
dif_gender_model <- multipleGroup(itemsonly, model=1, gender_groups, SE=T)

#running the DIF analysis
DIF(dif_gender_model, which.par = c("a1","d"), p.adjust = "fdr")
#the second argument, c("a1","d") tells the function which kinds of DIF to check for
#'a1' refers to the item slope (non-uniform DIF) while 'd' refers to the item difficulty (uniform DIF)
#so the above checks for "both" kinds of DIF

#can re-run for each type individually
DIF(dif_gender_model, which.par = "a1", p.adjust = "fdr")
DIF(dif_gender_model, which.par = "d", p.adjust = "fdr")

#the uniform DIF results suggests that S2DoCurse, S2DoScold, S3DoCurse, and S3DoScold may all be DIF items
#it's important to look at effect sizes (they are items 16, 17, 19, and 20 in the table below)

empirical_ES(dif_gender_model)
#there's no standard way to interpret these expected scores
#Dorans and Kulick (1986) developed a criterion suggesting items with effect size > .10 might be problematic

#this generates item traces for four DIF items
#there is one trace per group, making visualizing DIF easy
itemplot(dif_gender_model, item="S2DoCurse")
itemplot(dif_gender_model, item="S2DoScold")
itemplot(dif_gender_model, item="S3DoCurse")
itemplot(dif_gender_model, item="S3DoScold")
