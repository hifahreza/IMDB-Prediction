######## Imports ########

library(ggplot2)
library(car)
library(splines)
library(boot)
library(psych)
library(gridExtra)
require(methods)
library(moments)
require(ggplot2)
require(broom)
require(lmtest)
require(plm)

######### Load data ########

df = read.csv('/Users/sg/projects/imdb-prediction-github/films_fall_2021_training_data - films_fall_2021.csv')
df = read.csv("C:/Users/louis/OneDrive - McGill University/MMA Fall 2021 Notes/MGSC661/Midterm/movies_data.csv")
attach(df)

# Remove columns
df <- subset(df, select=-c(genre_realitytv,genre_shortfilm))
attach(df)

########### EDA ###########

head(df)
summary(df)

boxplot(total_number_of_actors, main="Total Number of Actors")
boxplot(total_number_of_production_companies, main='Number of Production Companies')
boxplot(total_number_of_production_countries, main='Number of Production Countries')
boxplot(total_number_of_producers, main='Number of Producers')

#Get dataframe with no dummy variables
dummies <- apply(df,2,function(x) { all(x %in% 0:1) })
movies_nodummies <- df[!dummies]
movies_dummies <- df[dummies]


#Getting a dataframe with all numeric variables and with dummies 
nums <- unlist(lapply(df, is.numeric))  
movies_numeric_dummies <- df[ ,nums]

# Getting a dataframe of only numeric variables and no dummies
nums <- unlist(lapply(movies_nodummies, is.numeric))  
movies_numeric <- movies_nodummies[ , nums]
summary(movies_numeric)

# Generating a grid of histograms with all numerical columns
histplot = function (data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(fill = "lightsteelblue4") +
    xlab(column) +
    annotate("text",
             x=Inf,y=Inf, 
             label=paste0("Skewness: ", round(skewness(data[,column]), digits=2)), vjust=1, hjust=1) +
    coord_cartesian(clip="on")
}
numhistplots <- lapply(colnames(movies_numeric), histplot, data = movies_numeric)
names(numhistplots) <- colnames(movies_numeric)

n <- length(numhistplots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(numhistplots, ncol=nCol))

########### Correlation Plots ##########
install.packages("corrplot")
library(corrplot)

numericcor = cor(movies_numeric)
corrplot(numericcor, method='circle', is.corr=FALSE, type="lower", diag=FALSE, tl.col="black", tl.cex=0.5, tl.srt = 45, cl.cex = .65)

#Adding imdb_score to movies_dummies for the correlation plot
dummycor = cor(movies_dummies)
corrplot(dummycor, method='circle', is.corr=FALSE, type='lower', diag=FALSE, tl.col="black", tl.cex=0.5, tl.srt = 45, cl.cex = .65)

fulldatacor = cor(movies_numeric_dummies)
corrplot(fulldatacor, method="circle", is.corr=FALSE, type="lower", diag=FALSE, tl.col="black", tl.cex=0.5, tl.srt = 45, cl.cex = .65)

rowfullcor <- cor(x=movies_numeric_dummies, y=df$imdb_score)
corrplot(rowfullcor, method="circle", col="black", tl.col="black",  tl.cex=0.5)

########## Outliers ############

lm1 = lm(imdb_score ~ budget_in_millions)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ month_of_release)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ year_of_release)
outlierTest(lm1)
#outliers 633, 895, 2310, 2718, 2045, 526

lm1 = lm(imdb_score ~ duration_in_hours)
outlierTest(lm1)
#outliers 633, 895

df$main_lang=as.factor(df$main_lang)
attach(df)
lm1 = lm(imdb_score ~ main_lang)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ total_number_languages)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ budget_in_millions)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_action)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_adventure)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_animation)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_biography)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_comedy)
outlierTest(lm1)
#outliers 633, 895, 2045

lm1 = lm(imdb_score ~ genre_crime)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_documentary)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_drama)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_family)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_fantasy)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_filmnoir)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_history)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_horror)
outlierTest(lm1)
#outliers 633, 895, 575, 2045

lm1 = lm(imdb_score ~ genre_music)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_musical)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_mystery)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score ~ genre_realitytv)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~genre_romance)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~genre_scifi)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~genre_shortfilm)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~genre_sport)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~genre_thriller)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~genre_war)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~genre_western)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~main_actor1_name)
outlierTest(lm1)
#outliers 895

lm1 = lm(imdb_score~main_actor1_is_female)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~main_actor2_is_female)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~main_actor3_name)
outlierTest(lm1)
#outliers 126

lm1 = lm(imdb_score~main_actor3_is_female)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~total_number_of_actors)
outlierTest(lm1)
#outliers 633, 895, 2045

lm1 = lm(imdb_score~main_director_name)
outlierTest(lm1)
#outliers 895

lm1 = lm(imdb_score~main_director_is_female)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~total_number_of_directors)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~main_producer_name)
outlierTest(lm1)
#outliers 1945

lm1 = lm(imdb_score~total_number_of_producers)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~editor_name)
outlierTest(lm1)
#outliers 895, 575, 439

lm1 = lm(imdb_score~main_production_company)
outlierTest(lm1)
#outliers 575

lm1 = lm(imdb_score~total_number_of_production_companies)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~main_production_country)
outlierTest(lm1)
#outliers 633, 895

lm1 = lm(imdb_score~total_number_of_production_countries)
outlierTest(lm1)
#outliers 633, 895

########## Remove outliers ##############

# Remove observations 633, 895 from the dataset because they are identified
# as outliers
excluded_obs = -c(633, 895, 197)
df <- df[excluded_obs, ]
movies_dummies <- movies_dummies[excluded_obs, ]
movies_nodummies <- movies_nodummies[excluded_obs, ]
movies_numeric <- movies_numeric[excluded_obs, ]
movies_numeric_dummies <- movies_numeric_dummies[excluded_obs, ]
attach(df)

# Remove observation 197 because it has 312 actors
excluded_obs = -c(197)
df <- df[excluded_obs, ]
movies_dummies <- movies_dummies[excluded_obs, ]
movies_nodummies <- movies_nodummies[excluded_obs, ]
movies_numeric <- movies_numeric[excluded_obs, ]
movies_numeric_dummies <- movies_numeric_dummies[excluded_obs, ]
attach(df)

########### Categorical Variables #############

df$main_lang=as.factor(df$main_lang)
df$main_director_name=as.factor(df$main_director_name)
df$main_actor1_name=as.factor(df$main_actor1_name)
df$main_actor2_name=as.factor(df$main_actor2_name)
df$main_actor3_name=as.factor(df$main_actor3_name)
df$main_producer_name=as.factor(df$main_producer_name)
df$editor_name=as.factor(df$editor_name)
df$main_production_company=as.factor(df$main_production_company)
df$main_production_country=as.factor(df$main_production_country)

lm = lm(imdb_score~main_lang)
summary(lm)
lm = lm(imdb_score~main_actor1_name)
summary(lm)
lm = lm(imdb_score~main_actor2_name)
summary(lm)
lm = lm(imdb_score~main_actor3_name)
summary(lm)
lm = lm(imdb_score~main_director_name)
summary(lm)
lm = lm(imdb_score~main_producer_name)
summary(lm)
lm = lm(imdb_score~editor_name)
summary(lm)
lm = lm(imdb_score~main_production_company)
summary(lm)
lm = lm(imdb_score~main_production_country)
summary(lm)

########## Heteroskedasticity #############3

mreg_p1=lm(imdb_score~budget_in_millions)
mreg_p2=lm(imdb_score~year_of_release)
mreg_p3=lm(imdb_score~ duration_in_hours)
mreg_p4=lm(imdb_score~total_number_of_actors)
mreg_p5=lm(imdb_score~total_number_of_producers)
mreg_p6=lm(imdb_score~total_number_of_production_companies)

# Heteroskedastic if p-value < 0.05
mreg_ncv <- vector()
mreg_ncv[1] <- ncvTest(mreg_p1)[5]
mreg_ncv[2] <- ncvTest(mreg_p2)[5]
mreg_ncv[3] <- ncvTest(mreg_p3)[5]
mreg_ncv[4] <- ncvTest(mreg_p4)[5]
mreg_ncv[5] <- ncvTest(mreg_p5)[5]
mreg_ncv[6] <- ncvTest(mreg_p6)[5]

# Need to generate table off the ncv test results
name_mreg_ncv <- c("budget_in_millions","year_of_release",
                   "duration_in_hours","total_number_of_actors",
                   "total_number_of_producers","total_number_of_production_companies")

ncv_df <- data.frame(matrix(ncol=2, nrow=length(mreg_ncv)))
x <- c("predictor","ncv_pval")
colnames(ncv_df) <- x
ncv_df$predictor <- name_mreg_ncv
ncv_df$ncv_pval <- mreg_ncv

library(stargazer)
library(tidyverse)
staroutput_ncv <- stargazer(ncv_df, summary=FALSE, digits=2, align=TRUE)
#write_lines(staroutput_ncv, file="stargazer_ncv.txt")

############ Residual Plots ##########
cont.pred <- subset(df, select = c(budget_in_millions,month_of_release,year_of_release,
                                       duration_in_hours,total_number_languages,total_number_of_actors,
                                       total_number_of_directors,total_number_of_producers,
                                       total_number_of_production_companies,total_number_of_production_countries))
col_names <- names(cont.pred)

mreg1=lm(imdb_score~budget_in_millions)
p1 <- ggplot(df, aes(y =residuals(mreg1) , x =  predict(mreg1))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[1]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg2=lm(imdb_score~month_of_release)
p2 <- ggplot(df, aes(y = residuals(mreg2), x =  predict(mreg2))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[2]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg3=lm(imdb_score~year_of_release)
p3 <- ggplot(df, aes(y = residuals(mreg3), x =  predict(mreg3))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[3]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg4=lm(imdb_score~duration_in_hours)
p4 <- ggplot(df, aes(y = residuals(mreg4), x =  predict(mreg4))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[4]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg5=lm(imdb_score~total_number_languages)
p5 <- ggplot(df, aes(y = residuals(mreg5), x =  predict(mreg5))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[5]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg6=lm(imdb_score~total_number_of_actors)
p6 <- ggplot(df, aes(y = residuals(mreg6), x =  predict(mreg6))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[6]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg7=lm(imdb_score~total_number_of_directors)
p7 <- ggplot(df, aes(y = residuals(mreg7), x =  predict(mreg7))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[7]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg8=lm(imdb_score~total_number_of_producers)
p8 <- ggplot(df, aes(y = residuals(mreg8), x =  predict(mreg8))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[8]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg9=lm(imdb_score~total_number_of_production_companies)
p9 <- ggplot(df, aes(y = residuals(mreg9), x =  predict(mreg9))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[9]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)
mreg10=lm(imdb_score~total_number_of_production_countries)
p10 <- ggplot(df, aes(y = residuals(mreg10), x =  predict(mreg10))) +geom_point(size=0.5, col='lightsteelblue4') + xlab(col_names[10]) + ylab("imdb_score")+geom_abline(slope=0, intercept=0, lty=2)

PlotsList <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
grid.arrange(grobs = PlotsList, ncol = 4)

############ Scatter Plots ###############

######Scatter plots for numeric variables
scatplot = function (data, column) {
  ggplot(data, aes_string(y=df$imdb_score, x=column)) +
    geom_point(size=0.5, col="lightsteelblue4") +
    geom_smooth(formula=y~x, col="lightsteelblue2") +
    xlab(column) +
    ylab("imdb_score")
  
}

#Only scatter plots for continuous x's
vec.cont <- c(2,4,5,6,7,8,9,10,11)
numscatplots_cont <- lapply(colnames(movies_numeric[,vec.cont]), scatplot, data = movies_numeric)
names(numscatplots_cont) <- colnames(movies_numeric[,vec.cont])

n <- length(numscatplots_cont)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(numscatplots_cont, ncol=nCol))

######### Simple Linear Regressions ########

len.all <- length(movies_numeric_dummies)
simplelinreg_lst <- lapply(2:len.all, function(x) lm(movies_numeric_dummies$imdb_score ~ movies_numeric_dummies[,x]))
names(simplelinreg_lst) <- colnames(movies_numeric_dummies[,2:len.all])
simplelinreg_summaries <- lapply(simplelinreg_lst, summary)

# Get lists for the rsquared and the pvalues
rsquared_lst <- lapply(simplelinreg_summaries, function(x) round((x$r.squared),5))
pvalue_lst <- lapply(simplelinreg_summaries, function(x) round(x$coefficient[2,4],5))
coeff_lst <- lapply(simplelinreg_summaries, function(x) round(x$coefficient[2,1],5))

#Getting a dataframe with coefficients, pvalues and rsquared for each model
results_df <- data.frame(matrix(ncol = 4, nrow = length(simplelinreg_lst)))
x <- c("predictor", "coefficient","pval", "rsq")
colnames(results_df) <- x
results_df$predictor = unlist(names(simplelinreg_lst))
results_df$rsq = unlist(rsquared_lst)
results_df$pval = unlist(pvalue_lst)
results_df$coefficient = unlist(coeff_lst)

#Order the results: increasing p-value and decreasing r-squared
results_df <- results_df[order(results_df$pval, -results_df$rsq),] #Order by increasing pvalue and decreasing rsquared

#Get a stargazer table for our report
library(stargazer)
library(tidyverse)
staroutput <- stargazer(results_df, summary=FALSE, digits=2, align=TRUE)
#write_lines(staroutput, file="stargazer_df.txt")

#Getting the same simple linear regression table but with robust pvalues using coeftest
simplelinregrobust_summaries <- lapply(simplelinreg_lst, function(x) coeftest(x, vcov=vcovHC(x, type="HC1")))
robust_pvalue_lst <- lapply(simplelinregrobust_summaries, function(x) round(x[8],5))
robust_coef_lst <- lapply(simplelinregrobust_summaries, function(x) x[2])

robust_df <- data.frame(matrix(ncol = 3, nrow = length(simplelinreg_lst)))
x <- c("predictor", "coefficient","robust_pval")
colnames(robust_df) <- x
robust_df$predictor = unlist(names(simplelinreg_lst))
robust_df$robust_pval = unlist(robust_pvalue_lst)
robust_df$coefficient = unlist(robust_coef_lst)

#Order the results: increasing p-value
robust_df <- robust_df[order(robust_df$robust_pval),] #Order by increasing pvalue and decreasing rsquared

#Get a stargazer table for our robust pvalue report
staroutput_robust <- stargazer(robust_df, summary=FALSE, digits=2, align=TRUE)
#write_lines(staroutput_robust, file="stargazer_robust_df.txt")

####### Multiple Linear Regression - Numerical Variables #######
mreg = lm(imdb_score ~ budget_in_millions + month_of_release + year_of_release + duration_in_hours + total_number_languages + total_number_of_actors + total_number_of_producers)
summary(mreg)

####### Model Selection - Anova Tests #######

m1 = lm(imdb_score ~ budget_in_millions)
m2 = lm(imdb_score ~ poly(budget_in_millions,2))
m3 = lm(imdb_score ~ poly(budget_in_millions,3))
m4 = lm(imdb_score ~ poly(budget_in_millions,4))
m5 = lm(imdb_score ~ poly(budget_in_millions,5))
#m6 = lm(imdb_score ~ poly(budget_in_millions,6)) - high pvalue
anova(m1,m2,m3,m4,m5) #degree 5 is a good fit
#check last degree to see fit

plot=ggplot(df, aes(y=imdb_score, x=budget_in_millions)) # aes = aesthetics
scatter = geom_point(color='lightsteelblue4')
line1 = geom_smooth(method="lm", formula=y~poly(x,5), color='pink')
plot_budget=plot+scatter+line1+ggtitle('Budget in millions (polynomial regression with degree 5)')
plot_budget
#create scatter for each and combine (lec 5,66)

# Month of release
m1 = lm(imdb_score ~ month_of_release)
m2 = lm(imdb_score ~ poly(month_of_release,2))
m3 = lm(imdb_score ~ poly(month_of_release,3))
m4 = lm(imdb_score ~ poly(month_of_release,4))
m5 = lm(imdb_score ~ poly(month_of_release,5))
anova(m1,m2,m3,m4,m5) #degree 3 is a good fit
#check last degree to see fit

# Year of release
m1 = lm(imdb_score ~ year_of_release)
m2 = lm(imdb_score ~ poly(year_of_release, 2))
summary(m2)
m3 = lm(imdb_score ~ poly(year_of_release, 3))
m4 = lm(imdb_score ~ poly(year_of_release, 4))
m5 = lm(imdb_score ~ poly(year_of_release, 5))
anova(m1,m2,m3,m4,m5) #degree 2 is a good fit
#check last degree to see fit

plot=ggplot(df, aes(y=imdb_score, x=year_of_release)) # aes = aesthetics
scatter = geom_point(color='lightsteelblue4')
line1 = geom_smooth(method="lm", formula=y~poly(x,2), color='pink')
plot_year=plot+scatter+line1+ggtitle('Year of release (polynomial regression with degree 2)')

# Duration in hours
m1 = lm(imdb_score ~ duration_in_hours)
m2 = lm(imdb_score ~ poly(duration_in_hours, 2))
m3 = lm(imdb_score ~ poly(duration_in_hours, 3))
m4 = lm(imdb_score ~ poly(duration_in_hours, 4))
m5 = lm(imdb_score ~ poly(duration_in_hours, 5))
summary(m5)
anova(m1,m2,m3,m4,m5) #degree 5 is a good fit
#check last degree to see fit

plot=ggplot(df, aes(y=imdb_score, x=duration_in_hours)) # aes = aesthetics
scatter = geom_point(color='lightsteelblue4')
line1 = geom_smooth(method="lm", formula=y~poly(x,5), color='pink')
plot_duration = plot+scatter+line1+ggtitle('Duration in hours (polynomial regression with degree 5)')
          
# Total Number of Languages                    
reg2=lm(imdb_score~poly(total_number_languages, 2))
summary(reg2)
reg3=lm(imdb_score~poly(total_number_languages, 3))
summary(reg3)
reg4=lm(imdb_score~poly(total_number_languages, 4))
summary(reg4)
reg5=lm(imdb_score~poly(total_number_languages, 5))
summary(reg5)
# plot
plot = ggplot(df, aes(y=imdb_score, x=total_number_languages))
scatter = geom_point(color="lightsteelblue4")
line_poly2 = geom_smooth(method="lm", formula=y~x, color='pink')
plot_languages = plot+scatter+line_poly2+ggtitle('Total number of languages (polynomial regression with degree 1)')
# anova
anova(reg2, reg3, reg4, reg5)
# keep linear model

# Total number of actors
reg2=lm(imdb_score~poly(total_number_of_actors,2))
summary(reg2)
reg3=lm(imdb_score~poly(total_number_of_actors,3))
summary(reg2)
reg4=lm(imdb_score~poly(total_number_of_actors,4))
summary(reg2)
reg5=lm(imdb_score~poly(total_number_of_actors,5))
summary(reg5)
# plot
plot = ggplot(df, aes(y=imdb_score, x=total_number_of_actors))
scatter = geom_point(color="lightsteelblue4")
line_poly2 = geom_smooth(method="lm", formula=y~poly(x, 2), color='pink')
plot_actors = plot+scatter+line_poly2+ggtitle('Total number of actors (polynomial regression with degree 2)')
# anova
anova(reg2, reg3, reg4, reg5)
# keep degree 2

# Total number of producers
reg2=lm(imdb_score~poly(total_number_of_producers,2))
summary(reg2)
reg3=lm(imdb_score~poly(total_number_of_producers,3))
summary(reg2)
reg4=lm(imdb_score~poly(total_number_of_producers,4))
summary(reg2)
reg5=lm(imdb_score~poly(total_number_of_producers,5))
summary(reg2)
# plot
plot = ggplot(df, aes(y=imdb_score, x=total_number_of_producers))
scatter = geom_point(color="lightsteelblue4")
line_poly2 = geom_smooth(method="lm", formula=y~poly(x, 5), color='pink')
plot_producers=plot+scatter+line_poly2+ggtitle('Total number of producers (polynomial regression with degree 5)')
# anova
anova(reg2, reg3, reg4, reg5)
# keep degree 5

grid.arrange(plot_budget, plot_duration, plot_actors, plot_producers, plot_languages, plot_year)
########### Splines ##############

#Spline for duration in hours
simplelinreg_summaries[4]
spline1.duration <- lm(imdb_score ~ bs(duration_in_hours, knots=c(2.2), degree=1), data=df)
spline2.duration <- lm(imdb_score ~ bs(duration_in_hours, knots=c(2.2), degree=2), data=df)
spline3.duration <- lm(imdb_score ~ bs(duration_in_hours, knots=c(2.2), degree=3), data=df)
spline4.duration <- lm(imdb_score ~ bs(duration_in_hours, knots=c(2.2), degree=4), data=df)

summary(spline1.duration)
summary(spline2.duration)
summary(spline3.duration)
summary(spline4.duration)

spline_1= geom_smooth(method = "lm", formula = y~bs(x,knots=c(2.2), degree=1), aes(color="red"))
spline_2= geom_smooth(method = "lm", formula = y~bs(x,knots=c(2.2), degree=2), aes(color="blue"))
spline_3= geom_smooth(method = "lm", formula = y~bs(x,knots=c(2.2), degree=3), aes(color="green"))
spline_4= geom_smooth(method = "lm", formula = y~bs(x,knots=c(2.2), degree=4), aes(color="purple"))

spline.duration.plot <- ggplot(df, aes(y=imdb_score, x=duration_in_hours)) +
  geom_point(color='lightsteelblue4') + spline_1 + spline_2 + spline_3 + spline_4 + ggtitle("Duration in hours spline with varying degrees")
spline.duration.plot +scale_color_identity(name = "Splines",
                                           breaks = c("red", "blue", "green", "purple"),
                                           labels = c("d=1", "d=2", "d=3", "d=4"),
                                           guide = "legend") + geom_vline(xintercept=2.2)

#Spline for year of release
spline1.year <- lm(imdb_score ~ bs(year_of_release, knots=c(1970), degree=1), data=df)
spline2.year <- lm(imdb_score ~ bs(year_of_release, knots=c(1970), degree=2), data=df)
spline3.year <- lm(imdb_score ~ bs(year_of_release, knots=c(1970), degree=3), data=df)
spline4.year <- lm(imdb_score ~ bs(year_of_release, knots=c(1970), degree=4), data=df)

summary(spline1.year)
summary(spline2.year)
summary(spline3.year)
summary(spline4.year)

spline_1= geom_smooth(method = "lm", formula = y~bs(x,knots=c(1970), degree=1), aes(color="red"))
spline_2= geom_smooth(method = "lm", formula = y~bs(x,knots=c(1970), degree=2), aes(color="blue"))
spline_3= geom_smooth(method = "lm", formula = y~bs(x,knots=c(1970), degree=3), aes(color="green"))
spline_4= geom_smooth(method = "lm", formula = y~bs(x,knots=c(1970), degree=4), aes(color="purple"))

spline.year.plot <- ggplot(df, aes(y=imdb_score, x=year_of_release)) +
  geom_point(color='lightsteelblue4') + spline_1 + spline_2 + spline_3 + spline_4 + ggtitle("Year of release spline with varying degrees")
spline.year.plot +scale_color_identity(name = "Splines",
                                       breaks = c("red", "blue", "green", "purple"),
                                       labels = c("d=1", "d=2", "d=3", "d=4"),
                                       guide = "legend") + geom_vline(xintercept=1970)


########### Model Selection - Initial Model ##############

# basic cross validation with our initial model
fit=glm(imdb_score~poly(budget_in_millions, 5) + poly(year_of_release, 2) + poly(duration_in_hours, 2) + poly(total_number_of_actors, 2) + poly(total_number_of_producers, 5) + poly(month_of_release, 3), data=df)
mse=cv.glm(df, fit, K=10)$delta[1]
mse
# about 0.65-0.79

# cross validation with just the dummies 
movies_dummies$imdb_score <- df$imdb_score #Adding imdb_score to movies_dummiess
fit=glm(imdb_score~ ., data=movies_dummies)
mse=cv.glm(df, fit, K=10)$delta[1]
mse

########### Importing modified dataset with our new predictors ##############

movies2 <- read.csv(file = 'C:\\Users\\louis\\OneDrive\\Documents\\GitHub\\imdb-prediction\\results_new.csv')
attach(movies2)

#Remove genre_realitytv and genre_shortfilm: no information, all 0 values
movies2 <- subset(movies2, select=-c(genre_realitytv,genre_shortfilm))

#Remove outliers: 197, 633 and 895
movies2 <- movies2[-c(197,633,895),]

#Get dataframe with no dummy variables
dummies2 <- apply(movies2,2,function(x) { all(x %in% 0:1) })
movies_nodummies2 <- movies2[!dummies]

#Getting a dataframe with all numeric variables and with dummies 
nums2 <- unlist(lapply(movies2, is.numeric))  
movies_numeric_dummies2 <- movies2[ ,nums]

#Getting a dataframe of only numeric variables and no dummies
nums2 <- unlist(lapply(movies_nodummies2, is.numeric))  
movies_numeric2 <- movies_nodummies2[ , nums]

#Getting a dataframe with only the dummy variables
movies_dummies2 <- movies2[dummies]

#Getting a dataframe with only our computed variables
movies_computed <- movies2[,51:62]
#Removing the columns with null values - only interested in the dummy variables
movies_computed <- movies_computed[,-c(1,3,5,7,9,11)]


########### Correlation matrix for computed variables ##############
library(corrplot)

computedcor = cor(movies_computed)
corrplot(computedcor, method='circle', is.corr=FALSE, type="lower", diag=FALSE, tl.col="black", tl.cex=0.75, tl.srt = 45, cl.cex = .65)

########### Adjusted R-squared testing for computed variables ##############
lm.compute1 <- lm(imdb_score ~ is_high_avg_score_main_actor1_name, data=movies2)
summary(lm.compute1)
lm.compute2 <- lm(imdb_score ~is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name, data=movies2)
summary(lm.compute2)
lm.compute3 <- lm(imdb_score ~is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                  + is_high_avg_score_main_actor3_name, data=movies2)
summary(lm.compute3)
lm.compute4 <- lm(imdb_score ~is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name + 
                    is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name, data=movies2)
summary(lm.compute4)
lm.compute5 <- lm(imdb_score ~is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name + 
                    is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                  + is_high_avg_score_main_producer_name, data=movies2)
summary(lm.compute5)
lm.compute6 <- lm(imdb_score ~is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name + 
                    is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                  + is_high_avg_score_main_producer_name + is_high_avg_score_main_production_company, data=movies2)
summary(lm.compute6)

########### Selecting dummy variables ##############
mreg.dummies <- lm(imdb_score ~ ., data=movies_dummies)
summary(mreg.dummies)

df_signif_dummies_05 <- as.data.frame(summary(mreg.dummies)$coef[summary(mreg.dummies)$coef[,4] <= .05, 4])
signif_dummies_05 <- rownames(df_signif_dummies_05)[-1]

df_signif_dummies_01 <- as.data.frame(summary(mreg.dummies)$coef[summary(mreg.dummies)$coef[,4] <= .01, 4])
signif_dummies_01 <- rownames(df_signif_dummies_01)[-1]

########### Building on the base model for predictions - lowering MSE ##############

#Taking base model and optimizing polynomial degrees using cv
cv.error=array(0, dim=c(4,4,4,4,4,4))

for (a in 2:5) {
  for (b in 2:5) {
    for (c in 2:5) {
      for (d in 2:5) {
        for (e in 2:5) {
          for (f in 2:5) {
            mreg.kfold <- glm(imdb_score~poly(budget_in_millions, a) + poly(year_of_release, b) 
                              + poly(duration_in_hours, c) + poly(total_number_of_actors, d) 
                              + poly(total_number_of_producers, e) + poly(month_of_release, f), data=df)
            cv.error[a-1,b-1,c-1,d-1,e-1,f-1] <- cv.glm(df, mreg.kfold, K=10)$delta[1]
}}}}}}
cv.error
arrayInd(which.min(cv.error), dim(cv.error))

cv.error[4,2,4,2,2,1]  #add 1 so actually 5,3,5,3,3,2 gives 0.6574906

#Add splines: duration in hours with knot at 2.2

mreg.predictive <- glm(imdb_score~poly(budget_in_millions,5)+ poly(year_of_release,3) +bs(duration_in_hours, knots=c(2.2), degree=4)
                       +poly(total_number_of_actors,3)+poly(total_number_of_producers,3)+poly(month_of_release,2), data=df)
kfold <- mreg.predictive
mse <- cv.glm(df, kfold, K=10)$delta[1]
mse
#Add splines: year of release

mreg.predictive <- glm(imdb_score~poly(budget_in_millions,5) +bs(duration_in_hours, knots=c(2.2), degree=4) 
                       +bs(year_of_release, knots=c(1970), degree=5) +poly(total_number_of_actors,3)+poly(total_number_of_producers,3)
                       +poly(month_of_release,2), data=df)
kfold <- mreg.predictive
mse <- cv.glm(df, kfold, K=10)$delta[1]
mse

#Add selected dummy variables - p-value <0.01

mreg.predictive <- glm(imdb_score~poly(budget_in_millions,5) +bs(duration_in_hours, knots=c(2.2), degree=4) 
                       +bs(year_of_release, knots=c(1970), degree=5) +poly(total_number_of_actors,3)
                       +poly(total_number_of_producers,3)+poly(month_of_release,4)
                       + genre_action + genre_comedy + genre_drama + genre_family + genre_fantasy
                       +  genre_filmnoir + genre_horror + genre_musical + genre_scifi + genre_thriller
                       + main_actor1_is_female + main_actor2_is_female, data=df)

kfold <- mreg.predictive
mse <- cv.glm(df, kfold, K=10)$delta[1]
mse

#Add selected dummy variables - p-value <0.05
mreg.predictive <- glm(imdb_score~poly(budget_in_millions,5) +bs(duration_in_hours, knots=c(2.2), degree=4) 
                       +bs(year_of_release, knots=c(1970), degree=5) +poly(total_number_of_actors,3)
                       +poly(total_number_of_producers,3) +poly(month_of_release,4)
                       + genre_action + genre_animation + genre_biography + genre_comedy 
                       + genre_crime + genre_drama + genre_family + genre_fantasy 
                       + genre_filmnoir + genre_horror + genre_music + genre_musical 
                       + genre_romance + genre_scifi + genre_sport + genre_thriller 
                       + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female, data=df)
kfold <- mreg.predictive
mse <- cv.glm(df, kfold, K=10)$delta[1]
mse

#Adding the new predictors we computed
mreg.predictive <- glm(imdb_score~poly(budget_in_millions,5) +bs(duration_in_hours, knots=c(2.2), degree=4) 
                       +bs(year_of_release, knots=c(1970), degree=5) +poly(total_number_of_actors,3)
                       +poly(total_number_of_producers,3) +poly(month_of_release,4)
                       + genre_action + genre_animation + genre_biography + genre_comedy 
                       + genre_crime + genre_drama + genre_family + genre_fantasy 
                       + genre_filmnoir + genre_horror + genre_music + genre_musical 
                       + genre_romance + genre_scifi + genre_sport + genre_thriller 
                       + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                       + is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                       + is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                       + is_high_avg_score_main_producer_name
                       + is_high_avg_score_main_production_company, data=movies2)
kfold <- mreg.predictive
mse <- cv.glm(movies2, kfold, K=10)$delta[1]
mse

# Adding an interaction term: budget * year of release
plot(year_of_release, budget_in_millions, col="lightsteelblue4", main="Interaction between budget and year of release") #See that as year increases budget increases massively

mreg.predictive <- glm(imdb_score~poly(budget_in_millions,5) +bs(duration_in_hours, knots=c(2.2), degree=4) 
                       +bs(year_of_release, knots=c(1970), degree=5) +poly(total_number_of_actors,3)
                       +poly(total_number_of_producers,3) +poly(month_of_release,4)
                       + genre_action + genre_animation + genre_biography + genre_comedy 
                       + genre_crime + genre_drama + genre_family + genre_fantasy 
                       + genre_filmnoir + genre_horror + genre_music + genre_musical 
                       + genre_romance + genre_scifi + genre_sport + genre_thriller 
                       + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                       + is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                       + is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                       + is_high_avg_score_main_producer_name
                       + is_high_avg_score_main_production_company
                       + budget_in_millions*year_of_release, data=movies2)
kfold <- mreg.predictive
mse <- cv.glm(movies2, kfold, K=10)$delta[1]
mse

# Removing year of release to see if MSE increase: might be overfitting since added interaction term
mreg.predictive <- glm(imdb_score~poly(budget_in_millions,5) +bs(duration_in_hours, knots=c(2.2), degree=4) 
                       +poly(total_number_of_actors,3)
                       +poly(total_number_of_producers,3) +poly(month_of_release,4)
                       + genre_action + genre_animation + genre_biography + genre_comedy 
                       + genre_crime + genre_drama + genre_family + genre_fantasy 
                       + genre_filmnoir + genre_horror + genre_music + genre_musical 
                       + genre_romance + genre_scifi + genre_sport + genre_thriller 
                       + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                       + is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                       + is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                       + is_high_avg_score_main_producer_name
                       + is_high_avg_score_main_production_company
                       + budget_in_millions*year_of_release, data=movies2)
kfold <- mreg.predictive
mse <- cv.glm(movies2, kfold, K=10)$delta[1]
mse

# Removing month of release to see if MSE increase: might be overfitting since added interaction term
mreg.predictive <- glm(imdb_score~poly(budget_in_millions,5) +bs(duration_in_hours, knots=c(2.2), degree=4) 
                       +poly(total_number_of_actors,3)
                       +poly(total_number_of_producers,3)
                       + genre_action + genre_animation + genre_biography + genre_comedy 
                       + genre_crime + genre_drama + genre_family + genre_fantasy 
                       + genre_filmnoir + genre_horror + genre_music + genre_musical 
                       + genre_romance + genre_scifi + genre_sport + genre_thriller 
                       + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                       + is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                       + is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                       + is_high_avg_score_main_producer_name
                       + is_high_avg_score_main_production_company
                       + budget_in_millions*year_of_release, data=movies2)
kfold <- mreg.predictive
mse <- cv.glm(movies2, kfold, K=10)$delta[1]
mse


#Trying log transformations: first on budget in millions
par(mfrow=c(2,2))
hist(budget_in_millions, main="Budget in millions histogram")
hist(log(budget_in_millions), main="Budget in millions (log) histogram")
plot(budget_in_millions, imdb_score, main="Budget in millions scatterplot")
plot(log(budget_in_millions), imdb_score,main="Budget in millions (log) scatterplot")

mse_list <- c(rep(NA, 20))
for (i in 1:20) {
mreg.predictive <- glm(imdb_score~poly(log(budget_in_millions),4) +bs(duration_in_hours, knots=c(2.2), degree=3) 
                       +poly(total_number_of_actors,3)
                       +poly(total_number_of_producers,3)
                       + genre_action + genre_animation + genre_biography + genre_comedy 
                       + genre_crime + genre_drama + genre_family + genre_fantasy 
                       + genre_filmnoir + genre_horror + genre_music + genre_musical 
                       + genre_romance + genre_scifi + genre_sport + genre_thriller 
                       + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                       + is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                       + is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                       + is_high_avg_score_main_producer_name
                       + is_high_avg_score_main_production_company
                       + budget_in_millions*year_of_release, data=movies2)
kfold <- mreg.predictive
mse <- cv.glm(movies2, kfold, K=10)$delta[1]
mse_list[i] <- mse
}
mse_list
mean(mse_list)

#Trying log transformations: second on the interaction term
mse_list <- c(rep(NA, 20))
for (i in 1:20) {
mreg.predictive <- glm(imdb_score~poly(log(budget_in_millions),4) +bs(duration_in_hours, knots=c(2.2), degree=5) 
                       +poly(log(total_number_of_actors),4)
                       +poly(total_number_of_producers,3) 
                       + genre_action + genre_animation + genre_biography + genre_comedy 
                       + genre_crime + genre_drama + genre_family + genre_fantasy 
                       + genre_filmnoir + genre_horror + genre_music + genre_musical 
                       + genre_romance + genre_scifi + genre_sport + genre_thriller 
                       + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                       + is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                       + is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                       + is_high_avg_score_main_producer_name
                       + is_high_avg_score_main_production_company
                       + budget_in_millions*year_of_release, data=movies2)
kfold <- mreg.predictive
mse <- cv.glm(movies2, kfold, K=10)$delta[1]
mse_list[i] <- mse
}
mse_list
mean(mse_list)

#Polynomial optimization on this more complex model
cv.error=array(0, dim=c(4,4,4,4))
for (a in 2:5) {
  for (b in 2:5) {
    for (c in 2:5) {
      for (d in 2:5) {
        mreg.kfold <- glm(imdb_score~poly(log(budget_in_millions,a)) +bs(duration_in_hours, knots=c(2.2), degree=b) 
                                  +poly(total_number_of_actors,c)
                                  +poly(total_number_of_producers,d)
                                  + genre_action + genre_animation + genre_biography + genre_comedy 
                                  + genre_crime + genre_drama + genre_family + genre_fantasy 
                                  + genre_filmnoir + genre_horror + genre_music + genre_musical 
                                  + genre_romance + genre_scifi + genre_sport + genre_thriller 
                                  + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                                  + is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                                  + is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                                  + is_high_avg_score_main_producer_name
                                  + is_high_avg_score_main_production_company
                                  + log(budget_in_millions*year_of_release), data=movies2)
       cv.error[a-1,b-1,c-1,d-1] <- cv.glm(movies2, mreg.kfold, K=10)$delta[1]
}}}}
arrayInd(which.min(cv.error), dim(cv.error))
mse <- cv.error[arrayInd(which.min(cv.error), dim(cv.error))]
mse


############ Final Model ###########

model.final <- glm(imdb_score~poly(log(budget_in_millions,3)) +bs(duration_in_hours, knots=c(2.2), degree=5) 
                   +poly(total_number_of_actors,3)
                   +poly(total_number_of_producers,4)
                   + genre_action + genre_animation + genre_biography + genre_comedy 
                   + genre_crime + genre_drama + genre_family + genre_fantasy 
                   + genre_filmnoir + genre_horror + genre_music + genre_musical 
                   + genre_romance + genre_scifi + genre_sport + genre_thriller 
                   + main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                   + is_high_avg_score_main_actor1_name + is_high_avg_score_main_actor2_name 
                   + is_high_avg_score_main_actor3_name + is_high_avg_score_main_director_name
                   + is_high_avg_score_main_producer_name
                   + is_high_avg_score_main_production_company
                   + log(budget_in_millions*year_of_release), data=movies2)
kfold.final <- model.final
mse.final <- cv.glm(movies2, kfold.final, K=10)$delta[1]
mse.final


######### Predictions #########

predictions <- predict(model.final, movies2, type="response")
View(predictions)

#Adding predictions to imdb_score
movies2$imdb_score <- predictions

View(movies2)
