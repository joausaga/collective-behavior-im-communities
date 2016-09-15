####
# Script that analyze the following 
# research question:
# Do moderation interventions influence
# user participation?
####

###
#
# HYPOTHESIS: Ideas that have more interventions from moderators 
# have more participation: TRUE
#
###

###
#
# METRICS
## Dependent variables
# Participation
# - number of distinct users that posted 
# comments, votes, or both to ideas
## Independent variables
# Moderator interventions:
# - number of comments and votes posted by moderators
#
###

###
# CLARIFICATION
# Comments of moderators and idea authors are explicitly
# flagged in IdeaScale however votes from moderators and 
# idea author are not highlighted in any particular way.
# We claim therefore that only comments of moderators
# and idea authors may have an effect in participation.
# Then, moderator and idea author votes are not considered in the analysis.
###

###
# CONCLUSIONS
## 1- Moderate ideas have significantly more distinct participants than immoderate ideas
## 2- Moderate ideas have significantly more distinct comment posters than immoderate ideas
## 3- Moderate ideas have significantly more comments from authors of ideas than immoderate ideas
## 4- Moderate ideas have significantly more comments from the participants (not moderators neither authors) 
## than immoderate ideas
## 5- Moderate ideas have significantly more votes than immoderate ideas
## 6- Ideas where their authors posted comments have significantly more distinct participants than 
## those in which the authors did not interact
## 7- Ideas where their authors posted comments have significantly more distinct comment posters than 
## those in which the authors did not interact
## 8- Ideas where their authors posted comments have significantly more comments from participants than 
## those in which the authors did not interact
## 9- Ideas where their authors posted comments have significantly more votes than those in which the 
## authors did not interact
## 10-Ideas where their authors posted comments have significantly more moderator comments than those 
## in which the authors did not interact
## 11-The effect of moderation in the number of participants and in the number of distinct comment posters
## holds independently of whether their authors interacted or not.
## 12- Linear regression analyses unveil that the factor strongest influence the number of participants in
## ideas is the voting activity then the commenting activity and finally the intervention of moderators and
## authors, which both account for the same magnitude (systemic patterns were found in the residuals)
## 13- Linear regression analyses unveil that the factor strongest influence the number of distinct 
## comment posters ideas is the commenting activity then the intervention of moderators and authors,
## finally, the voting activity (systemic pattern were found in the residuals)
## 14- When the idea authors comment the influence of moderators cannot be seen in terms of total votes 
## gathered by ideas
###


identify_outliers = function(dataset) {
  out = c()
  for (col in 2:ncol(dataset)) {
    data = dataset[,col]
    descriptives = summary(data)
    Q1 = descriptives[[2]]
    Q3 = descriptives[[5]]
    IQR = Q3 - Q1
    current_outliers = as.character(dataset[dataset[,col] < (Q1 - 1.5 * IQR) | dataset[,col] > (Q3 + 1.5 * IQR), 1])
    out = c(out, current_outliers)
  }
  return (unique(out))
}


log_transform_var = function(x) {
  return (log(x + 0.5))
}


# Transform count variable to factor for visualization
# purpose. Taken from 
# http://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
transform_count_var_to_factor = function(x, breaks=NULL) {
  if(is.null(breaks)) {
    breaks = unique(quantile(x, 0:10/10))
  } 
  x = cut(x, breaks, include.lowest = TRUE, 
             right = FALSE)
  levels(x) = paste(breaks[-length(breaks)], 
                    ifelse(diff(breaks) > 1,
                    c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
                    sep = "")
  return (x)
}


display_bivariate_dep_reg = function(data, dependent_var, regressors) {
  num_regressors = length(regressors)
  if (num_regressors > 1) {
    nrow=num_regressors/2
    if((num_regressors %% 2) != 0) nrow=nrow+1
    par(mfrow=c(nrow,2))
  }
  dep_log = log_transform_var(data[,dependent_var])
  for (regressor in regressors) {
    if (regressor == 'author_admin' || regressor == 'weekend') {
      data[,regressor] = as.factor(as.character(data[,regressor]))
    }
    class_reg = class(data[,regressor])
    if (class_reg == 'numeric' || class_reg == 'integer') {
      if (regressor == 'mod_comments' || regressor == 'comments' ||
          regressor == 'author_comments') {
        trans_reg = transform_count_var_to_factor(data[,regressor],c(0:2,4,6))  
      } else {
        trans_reg = transform_count_var_to_factor(data[,regressor])  
      }
      plot(dep_log ~ trans_reg, data=data, 
           ylab=paste('log(',dependent_var,')',sep=''), 
           xlab=regressor)
    } else {
      plot(dep_log ~ data[,regressor], data=data,
           ylab=paste('log(',dependent_var,')',sep=''), 
           xlab=regressor)
    }
  }
}

compute_regression = function(d_var, reg_family, data) {
  reg_obj = NULL
  
  if (reg_family == "lineal") {
    reg_model = lm(d_var ~ mod_comments+author_comments+
                   p_votes+p_comments+score, data = data)  
    y_hat = reg_model$fitted
    reg_obj$predicted = as.numeric(y_hat)
    reg_obj$coefficients = as.numeric(summary(reg_model)$coef[,1])
    reg_obj$se = as.numeric(summary(reg_model)$coef[,2])
    reg_obj$sig = as.numeric(summary(reg_model)$coef[,4])
  } else {
    if (reg_family == "poisson") {
      reg_model = glm(d_var ~ mod_comments+author_comments+
                      p_votes+p_comments+score, offset=log(users), 
                      data = data, family = poisson(link = "log"))
      reg_obj$coefficients = as.numeric(summary(reg_model)$coef[,1])
      reg_obj$se = as.numeric(summary(reg_model)$coef[,2])
      reg_obj$sig = as.numeric(summary(reg_model)$coef[,4])
    } else if (reg_family == "quasipoisson") {
      reg_model = glm(d_var ~ mod_comments+author_comments+
                      p_votes+p_comments+score, offset=log(users), 
                      data = data, family = quasipoisson)
      reg_obj$coefficients = as.numeric(summary(reg_model)$coef[,1])
      reg_obj$se = as.numeric(summary(reg_model)$coef[,2])
      reg_obj$sig = as.numeric(summary(reg_model)$coef[,4])
    } else if (reg_family == "negative") {
      reg_model = glm.nb(d_var ~ mod_comments+author_comments+
                         p_comments+p_votes+score+offset(log(users)), 
                         data = m_ideas, maxit=100)
      reg_obj$coefficients = as.numeric(summary(reg_model)$coef[,1])
      reg_obj$se = as.numeric(summary(reg_model)$coef[,2])
      reg_obj$sig = as.numeric(summary(reg_model)$coef[,4])
    } else if (reg_family == "hurdle") {
      d_var = m_ideas$participants
      data = m_ideas
      reg_model = hurdle(d_var ~ mod_comments+author_comments+
                         p_votes+p_comments+score, offset=log(users), 
                         data = data, dist = "negbin", 
                         zero.dist = "binomial")  
      reg_obj$coefficients = as.numeric(summary(reg_model)$coef$count[,1])
      reg_obj$se = as.numeric(summary(reg_model)$coef$count[,2])
      reg_obj$sig = as.numeric(summary(reg_model)$coef$count[,4])
    }
    # Compute logged participant proportions
    log_y = reg_obj$coefficients[1] + reg_obj$coefficients[2] * data$mod_comments + 
            reg_obj$coefficients[3] * data$author_comments +
            reg_obj$coefficients[4] * data$p_votes + 
            reg_obj$coefficients[5] * data$p_comments + 
            reg_obj$coefficients[6] * data$score
    y_hat = exp(log_y)
    reg_obj$predicted = y_hat
  }
  reg_obj$model = reg_model
  reg_obj$mean_res = mean(abs(data$p_participants-y_hat))
  
  return (reg_obj)
}


# Load library
source('utils.R')
library(dplyr)
library(pgirmess)
library(car)
library(pscl)

library(sandwich)
library(lmtest)
library(foreign)
library(MASS)
library(PMCMR)
library(ResourceSelection)
library(AER)

# Load data
ideas = read.csv('data/community_mod_intervention_details.csv')
idea_details = read.csv('data/idsc_ideas_fixed.csv', header=T, stringsAsFactors=F)
tagged_communities = read.csv('data/tagged_communities_and_archetypes.csv', header=T)

# Idea details
idea_details$new_id = paste(idea_details$id,"-",idea_details$community_id,sep="")
idea_details = filter(idea_details, new_id %in% ideas$idea_id)
idea_details = dplyr::select(idea_details, new_id, title, text, tags, creation_datetime)
idea_details = mutate(idea_details, title_length = nchar(title))
idea_details = mutate(idea_details, text_length = nchar(text))
idea_details$creation_datetime = as.POSIXlt(idea_details$creation_datetime)
idea_details$weekday = idea_details$creation_datetime$wday
idea_details$weekend = ifelse(idea_details$weekday %in% c(0,6), 1, 0)
idea_details = dplyr::select(idea_details, new_id, title_length, text_length, weekday, weekend)

# Select variables
ideas = dplyr::select(ideas, community_id, idea_id, users, comments, votes, mod_comments, 
                      distinct_comment_authors, author_admin, author_comments, 
                      participants, score)

# Cast variables
ideas$author_admin = as.logical(as.character(ideas$author_admin)) * 1
ideas$mod_comments = as.integer(ideas$mod_comments)
ideas$participants = as.integer(ideas$participants)
ideas$distinct_comment_authors = as.integer(ideas$distinct_comment_authors)
ideas$users = as.integer(ideas$users)
ideas$author_comments = as.integer(ideas$author_comments)
ideas$votes = as.integer(ideas$votes)
ideas$comments = as.integer(ideas$comments)
ideas$score = as.numeric(ideas$score)

# Create variables to hold proportions
ideas = mutate(ideas, p_comments=comments/users)
ideas = mutate(ideas, p_votes=votes/users)
ideas = mutate(ideas, p_distinct_comment_authors=distinct_comment_authors/users)
ideas = mutate(ideas, p_participants=participants/users)
ideas = mutate(ideas, p_mod_comments=mod_comments/users)
ideas = mutate(ideas, p_author_comments=author_comments/users)

# Create main dataset
ideas = merge(ideas, idea_details, by.x="idea_id",by.y="new_id")

# Filter out ideas belonging to communities that weren't tagged
ideas = filter(ideas, community_id %in% tagged_communities$community)

# Plot distribution of variables
plot(table(ideas$participants))
plot(table(ideas$mod_comments))
plot(table(ideas$author_comments))
plot(table(ideas$comments))
plot(table(ideas$votes))
plot(table(ideas$distinct_comment_authors))

# Draw scatter plots to see relationship between variables
# pairs(ideas)

# Identify extreme values
outliers = ideas %>% filter(comments>15)
outliers = rbind(outliers, ideas %>% filter(votes>60))
outliers = rbind(outliers, ideas %>% filter(mod_comments>2))
outliers = rbind(outliers, ideas %>% filter(distinct_comment_authors>10))
outliers = rbind(outliers, ideas %>% filter(author_comments>3))
outliers = rbind(outliers, ideas %>% filter(participants>70))
print(paste('There are',length(unique(outliers$idea_id)),'extreme values'))

# Get rid of extreme values
ideas = filter(ideas, !idea_id %in% unique(outliers$idea_id))

# Inspect correlations between independent variables
cor(dplyr::select(ideas, votes, comments, mod_comments, 
           author_comments, score))

# Create categorical variable to indicate intervention of 
# moderators and authors
ideas$cat = 
  ifelse(ideas$mod_comments>0&ideas$author_comments>0, "mod_and_author",
         ifelse(ideas$mod_comments==0&ideas$author_comments>0, "only_author",
                ifelse(ideas$mod_comments>0&ideas$author_comments==0, "only_mod",
                       "no_mod_no_author")))
ideas$cat = as.factor(ideas$cat)

# Create contingency table for
# proportion of participants
cell11 = mean(ideas[ideas$mod_comments>0&ideas$author_comments>0,'p_participants'])
cell12 = mean(ideas[ideas$mod_comments>0&ideas$author_comments==0,'p_participants'])
cell21 = mean(ideas[ideas$mod_comments==0&ideas$author_comments>0,'p_participants'])
cell22 = mean(ideas[ideas$mod_comments==0&ideas$author_comments==0,'p_participants'])
part_tab = matrix(c(cell11,cell21,cell12,cell22),nrow=2,ncol=2)
colnames(part_tab) = c('author','no_author')
rownames(part_tab) = c('moderator','no_moderator')

# Analyze differences in the proportion of participants
# by type of interventions
tapply(ideas$p_participants, ideas$cat, mean) 
tapply(ideas$p_participants, ideas$cat, sd)
plot(p_participants ~ cat, data=ideas, 
     ylab='Proportion Participants', 
     xlab='Intervention')
# Kruskal-Wallis comparison to analyze difference between
# interventions
k_test = kruskal.test(ideas$p_participants ~ ideas$cat)
# Post Hoc pairwise comparison analysis of kruskal test
kruskalmc(ideas$p_participants ~ ideas$cat, probs=.05, cont=NULL)

# Analyze differences in the proportion of distinct comment authors
# by type of interventions
tapply(ideas$p_distinct_comment_authors, ideas$cat, mean) 
tapply(ideas$p_distinct_comment_authors, ideas$cat, sd)
plot(p_distinct_comment_authors ~ cat, data=ideas, 
     ylab='Proportion Distinct Comment Authors', 
     xlab='Intervention')
# Kruskal-Wallis comparison to analyze difference between
# interventions
k_test = kruskal.test(ideas$p_distinct_comment_authors ~ ideas$cat)
# Post Hoc pairwise comparison analysis of kruskal test
kruskalmc(ideas$p_distinct_comment_authors ~ ideas$cat, probs=.05, cont=NULL)


###
# Study level of moderation against participation
##
ideas$level_mod = 
  ifelse(ideas$mod_comments>2, "high",
         ifelse(ideas$mod_comments==1, "low", 
                ifelse(ideas$mod_comments==2, "medium","no moderation")))
ideas$level_mod = as.factor(ideas$level_mod)
ideas$level_mod = factor(ideas$level_mod, levels = c("no moderation", "low", "medium", "high"))
# Participation
plot(p_participants ~ level_mod, data=ideas, 
     ylab='Proportion Participants', 
     xlab='Moderation Type')
k_test = kruskal.test(ideas$p_participants ~ ideas$level_mod)
kruskalmc(ideas$p_participants ~ ideas$level_mod, probs=.05, cont=NULL)


###
# Study author intervention against participation
##
ideas$level_author = 
  ifelse(ideas$author_comments==0, "no author comments",
         ifelse(ideas$author_comments==1, "low", 
                ifelse(ideas$author_comments==2, "medium","high")))
ideas$level_author = as.factor(ideas$level_author)
ideas$level_author = factor(ideas$level_author, 
                            levels = c("no author comments", "low", "medium", "high"))
# Participation
plot(p_participants ~ level_author, data=ideas, 
     ylab='Proportion Participants', 
     xlab='Idea Author Involvement')
k_test = kruskal.test(ideas$p_participants ~ ideas$level_author)
kruskalmc(ideas$p_participants ~ ideas$level_author, probs=.05, cont=NULL)


###
# Study influence of score in participation
###
ideas$score_cat = 
  ifelse(ideas$score>=-2&ideas$score<=2, "neutral",
         ifelse(ideas$score>2, "positive", "negative"))
ideas$score_cat = as.factor(ideas$score_cat)
# Participation
plot(p_participants ~ score_cat, data=ideas, 
     ylab='Proportion Participants', 
     xlab='Score Type')
k_test = kruskal.test(ideas$p_participants ~ ideas$score_cat)
kruskalmc(ideas$p_participants ~ ideas$score_cat, probs=.05, cont=NULL)
# Distinct Comment Authors
plot(distinct_comment_authors ~ score_cat, data=ideas, 
     ylab='Distinct Comment Authors', 
     xlab='Score Type')
k_test = kruskal.test(ideas$distinct_comment_authors ~ ideas$score_cat)
kruskalmc(ideas$distinct_comment_authors ~ ideas$score_cat, probs=.05, cont=NULL)


###
# Study the impact of different independent 
# variables in the number of participants and 
# in the number of distinct post commenters
#
# Generalized linear regression models, such as 
# Negative Binomial, Zero-inflated or Hurdle, 
# should be used because here we're dealing with 
# count data which, which on the one hand are skew 
# and on the other hand have excess of 0 values. 
###
n = nrow(ideas)
# Compute and display mean and variance of dependent variable
print(paste("Mean:", mean(ideas$p_participants)))
print(paste("Var:",var(ideas$p_participants)))
# Display pairwise bivariate of the dependent variable against each 
# of the regressors
display_bivariate_dep_reg(ideas,"participants", c("mod_comments","author_comments",
                                                  "comments", "votes",
                                                  "author_admin", "score",
                                                  "title_length", "text_length",
                                                  "weekend","weekday"))

# Create dataset for modeling analyses
ideas = mutate(ideas, has_mod_comments=ifelse(mod_comments>0,1,0))
ideas = mutate(ideas, has_author_comments=ifelse(author_comments>0,1,0))
ideas = mutate(ideas, has_comments=ifelse(comments>0,1,0))
ideas = mutate(ideas, has_participants=ifelse(participants>0,1,0))
m_ideas = dplyr::select(ideas, idea_id, participants, users, has_mod_comments, 
                        has_author_comments, p_votes, p_comments, score,
                        p_participants, mod_comments, author_comments, votes,
                        comments, has_comments, has_participants, p_mod_comments,
                        p_author_comments, distinct_comment_authors,
                        p_distinct_comment_authors)

# Calculate zero counts
sum(m_ideas$p_participants == 0)/n
sum(m_ideas$mod_comments == 0)/n
sum(m_ideas$author_comments == 0)/n
sum(m_ideas$comments == 0)/n
sum(m_ideas$votes == 0)/n

###
## Apply Regressions Models
###

### 
## Linear
###
linear_obj = compute_regression(m_ideas$p_participants, "lineal", m_ideas)
# Plot actual vs. predicted values
plot(linear_obj$actual,linear_obj$predicted,pch=19,
     col="grey",ylab="Predicted Rates",xlab="Actual Rates", 
     main="Lineal")
# Check colinearity
vif(linear_obj$model)


### 
## Poisson
###
pois_obj = compute_regression(m_ideas$participants, "poisson", m_ideas)
# Plot actual vs. predicted values
plot(pois_obj$actual,pois_obj$predicted,pch=19,
     col="grey",ylab="Predicted Rates",xlab="Actual Rates", 
     main="Poisson")
# Check colinearity
vif(pois_obj$model)


### 
## Quasi-poisson
###
qpois_obj = compute_regression(m_ideas$participants, "quasipoisson", m_ideas)
# Plot predicted vs. actual
plot(qpois_obj$actual, qpois_obj$predicted, pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Quasipoisson")
# Check colinearity
vif(qpois_obj$model)


### 
## Negative Binomial
###
neg_obj = compute_regression(m_ideas$participants, "negative", m_ideas)
# Plot predicted vs. actual
plot(neg_obj$actual, neg_obj$predicted, pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Negative Binomial")
# Check colinearity
vif(neg_obj$model)


###
# Hurdle
### 
hur_obj = compute_regression(m_ideas$participants, "hurdle", m_ideas)
# Plot predicted vs. actual
plot(hur_obj$actual, hur_obj$predicted, pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Hurdle")
# Check colinearity
vif(hur_obj$model)


# Create tables with results
if (exists("table_reg")) {
  remove(table_reg) 
}
for (i in 2:6) {
  row = c(
    paste(round(linear_obj$coefficients[i],5)," (",round(linear_obj$se[i],5),") ",ifelse(linear_obj$sig[i]<0.05,"*",""),sep=""),
    paste(round(exp(pois_obj$coefficients[i]),5)," (",round(pois_obj$se[i],5),") ",ifelse(pois_obj$sig[i]<0.05,"*",""),sep=""),
    paste(round(exp(qpois_obj$coefficients[i]),5)," (",round(qpois_obj$se[i],5),") ",ifelse(qpois_obj$sig[i]<0.05,"*",""),sep=""),
    paste(round(exp(neg_obj$coefficients[i]),5)," (",round(neg_obj$se[i],5),") ",ifelse(neg_obj$sig[i]<0.05,"*",""),sep=""),
    paste(round(exp(hur_obj$coefficients[i]),5)," (",round(hur_obj$se[i],5),") ",ifelse(hur_obj$sig[i]<0.05,"*",""),sep="")
    )
  if (exists("table_reg")) {
    table_reg = rbind(table_reg, row)    
  } else {
    table_reg = matrix(row, ncol=5)
  }
}
colnames(table_reg) = c('Lineal','Poisson','QuasiPoisson','Negative Binomial', 'Hurdle')
table_reg = rbind(table_reg, c(
  round(linear_obj$mean_res,5),
  round(pois_obj$mean_res,5),
  round(qpois_obj$mean_res,5),
  round(neg_obj$mean_res,5),
  round(hur_obj$mean_res,5)
  ))
table_reg = rbind(table_reg, c(
  paste(as.numeric(summary(linear_obj$actual)[4]),"/",as.numeric(summary(linear_obj$predicted)[4])),
  paste(as.numeric(summary(pois_obj$actual)[4]),"/",as.numeric(summary(pois_obj$predicted)[4])),
  paste(as.numeric(summary(qpois_obj$actual)[4]),"/",as.numeric(summary(qpois_obj$predicted)[4])),
  paste(as.numeric(summary(neg_obj$actual)[4]),"/",as.numeric(summary(neg_obj$predicted)[4])),
  paste(as.numeric(summary(hur_obj$actual)[4]),"/",as.numeric(summary(hur_obj$predicted)[4]))
))
table_reg = rbind(table_reg, c(
  paste(as.numeric(summary(linear_obj$actual)[3]),"/",as.numeric(summary(linear_obj$predicted)[3])),
  paste(as.numeric(summary(pois_obj$actual)[3]),"/",as.numeric(summary(pois_obj$predicted)[3])),
  paste(as.numeric(summary(qpois_obj$actual)[3]),"/",as.numeric(summary(qpois_obj$predicted)[3])),
  paste(as.numeric(summary(neg_obj$actual)[3]),"/",as.numeric(summary(neg_obj$predicted)[3])),
  paste(as.numeric(summary(hur_obj$actual)[3]),"/",as.numeric(summary(hur_obj$predicted)[3]))
))
table_reg = rbind(table_reg, c(
  paste(as.numeric(summary(linear_obj$actual)[1]),"/",as.numeric(summary(linear_obj$predicted)[1])),
  paste(as.numeric(summary(pois_obj$actual)[1]),"/",as.numeric(summary(pois_obj$predicted)[1])),
  paste(as.numeric(summary(qpois_obj$actual)[1]),"/",as.numeric(summary(qpois_obj$predicted)[1])),
  paste(as.numeric(summary(neg_obj$actual)[1]),"/",as.numeric(summary(neg_obj$predicted)[1])),
  paste(as.numeric(summary(hur_obj$actual)[1]),"/",as.numeric(summary(hur_obj$predicted)[1]))
))
table_reg = rbind(table_reg, c(
  paste(as.numeric(summary(linear_obj$actual)[6]),"/",as.numeric(summary(linear_obj$predicted)[6])),
  paste(as.numeric(summary(pois_obj$actual)[6]),"/",as.numeric(summary(pois_obj$predicted)[6])),
  paste(as.numeric(summary(qpois_obj$actual)[6]),"/",as.numeric(summary(qpois_obj$predicted)[6])),
  paste(as.numeric(summary(neg_obj$actual)[6]),"/",as.numeric(summary(neg_obj$predicted)[6])),
  paste(as.numeric(summary(hur_obj$actual)[6]),"/",as.numeric(summary(hur_obj$predicted)[6]))
))
rownames(table_reg) = c('mod_comments','author_comments','p_votes','p_comments','score', 'mean res', 
                        'mean actual vs. predicted', 'median actual vs. predicted', 'min actual vs. predicted',
                        'max actual vs. predicted')


### 
## Analisis of distinct comment authors
###

display_bivariate_dep_reg(ideas,"distinct_comment_authors", 
                          c("mod_comments","author_comments",
                            "comments","votes","author_admin",
                            "score", "title_length", "text_length",
                            "weekend","weekday"))

### 
## Linear
###
linear_obj = compute_regression(m_ideas$distinct_comment_authors/m_ideas$users, "lineal", m_ideas)
linear_obj$actual = m_ideas$distinct_comment_authors/m_ideas$users
linear_obj$mean_res = mean(abs(linear_obj$actual-linear_obj$predicted))
# Plot actual vs. predicted values
plot(linear_obj$predicted,linear_obj$actual,pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Lineal")
# Check colinearity
vif(linear_obj$model)

### 
## Poisson
###
pois_obj = compute_regression(m_ideas$distinct_comment_authors, "poisson", m_ideas)
pois_obj$actual = m_ideas$distinct_comment_authors/m_ideas$users
pois_obj$mean_res = mean(abs(pois_obj$actual-pois_obj$predicted))
# Plot actual vs. predicted values
plot(pois_obj$predicted, pois_obj$actual,pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Poisson")
# Check colinearity
vif(pois_obj$model)


### 
## Quasi-poisson
###
qpois_obj = compute_regression(m_ideas$distinct_comment_authors, "quasipoisson", m_ideas)
qpois_obj$actual = m_ideas$distinct_comment_authors/m_ideas$users
qpois_obj$mean_res = mean(abs(qpois_obj$actual-qpois_obj$predicted))
# Plot predicted vs. actual
plot(qpois_obj$predicted, qpois_obj$actual, pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Quasipoisson")
# Check colinearity
vif(qpois_obj$model)


### 
## Negative Binomial
###
neg_obj = compute_regression(m_ideas$distinct_comment_authors, "negative", m_ideas)
neg_obj$actual = m_ideas$distinct_comment_authors/m_ideas$users
neg_obj$mean_res = mean(abs(neg_obj$actual-neg_obj$predicted))
# Plot predicted vs. actual
plot(neg_obj$predicted, neg_obj$actual, pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Negative Binomial")
# Check colinearity
vif(neg_obj$model)


###
# Hurdle
### 
hur_obj = compute_regression(m_ideas$distinct_comment_authors, "hurdle", m_ideas)
hur_obj$actual = m_ideas$distinct_comment_authors/m_ideas$users
hur_obj$mean_res = mean(abs(hur_obj$actual-hur_obj$predicted))
# Plot predicted vs. actual
plot(hur_obj$actual, hur_obj$actual, pch=19,
     col="grey",ylab="Predicted Rates",xlab="Actual Rates", 
     main="Hurdle")
# Check colinearity
vif(hur_obj$model)


# Create tables with results
if (exists("table_reg")) {
  remove(table_reg) 
}
for (i in 2:6) {
  row = c(
    paste(round(linear_obj$coefficients[i],5)," (",round(linear_obj$se[i],5),") ",ifelse(linear_obj$sig[i]<0.05,"*",""),sep=""),
    paste(round(exp(pois_obj$coefficients[i]),5)," (",round(pois_obj$se[i],5),") ",ifelse(pois_obj$sig[i]<0.05,"*",""),sep=""),
    paste(round(exp(qpois_obj$coefficients[i]),5)," (",round(qpois_obj$se[i],5),") ",ifelse(qpois_obj$sig[i]<0.05,"*",""),sep=""),
    paste(round(exp(neg_obj$coefficients[i]),5)," (",round(neg_obj$se[i],5),") ",ifelse(neg_obj$sig[i]<0.05,"*",""),sep=""),
    paste(round(exp(hur_obj$coefficients[i]),5)," (",round(hur_obj$se[i],5),") ",ifelse(hur_obj$sig[i]<0.05,"*",""),sep="")
  )
  if (exists("table_reg")) {
    table_reg = rbind(table_reg, row)    
  } else {
    table_reg = matrix(row, ncol=5)
  }
}
colnames(table_reg) = c('Lineal','Poisson','QuasiPoisson','Negative Binomial', 'Hurdle')
table_reg = rbind(table_reg, c(
  round(linear_obj$mean_res,5),
  round(pois_obj$mean_res,5),
  round(qpois_obj$mean_res,5),
  round(neg_obj$mean_res,5),
  round(hur_obj$mean_res,5)
))
table_reg = rbind(table_reg, c(
  paste(as.numeric(summary(linear_obj$actual)[4]),"/",as.numeric(summary(linear_obj$predicted)[4])),
  paste(as.numeric(summary(pois_obj$actual)[4]),"/",as.numeric(summary(pois_obj$predicted)[4])),
  paste(as.numeric(summary(qpois_obj$actual)[4]),"/",as.numeric(summary(qpois_obj$predicted)[4])),
  paste(as.numeric(summary(neg_obj$actual)[4]),"/",as.numeric(summary(neg_obj$predicted)[4])),
  paste(as.numeric(summary(hur_obj$actual)[4]),"/",as.numeric(summary(hur_obj$predicted)[4]))
))
table_reg = rbind(table_reg, c(
  paste(as.numeric(summary(linear_obj$actual)[3]),"/",as.numeric(summary(linear_obj$predicted)[3])),
  paste(as.numeric(summary(pois_obj$actual)[3]),"/",as.numeric(summary(pois_obj$predicted)[3])),
  paste(as.numeric(summary(qpois_obj$actual)[3]),"/",as.numeric(summary(qpois_obj$predicted)[3])),
  paste(as.numeric(summary(neg_obj$actual)[3]),"/",as.numeric(summary(neg_obj$predicted)[3])),
  paste(as.numeric(summary(hur_obj$actual)[3]),"/",as.numeric(summary(hur_obj$predicted)[3]))
))
table_reg = rbind(table_reg, c(
  paste(as.numeric(summary(linear_obj$actual)[1]),"/",as.numeric(summary(linear_obj$predicted)[1])),
  paste(as.numeric(summary(pois_obj$actual)[1]),"/",as.numeric(summary(pois_obj$predicted)[1])),
  paste(as.numeric(summary(qpois_obj$actual)[1]),"/",as.numeric(summary(qpois_obj$predicted)[1])),
  paste(as.numeric(summary(neg_obj$actual)[1]),"/",as.numeric(summary(neg_obj$predicted)[1])),
  paste(as.numeric(summary(hur_obj$actual)[1]),"/",as.numeric(summary(hur_obj$predicted)[1]))
))
table_reg = rbind(table_reg, c(
  paste(as.numeric(summary(linear_obj$actual)[6]),"/",as.numeric(summary(linear_obj$predicted)[6])),
  paste(as.numeric(summary(pois_obj$actual)[6]),"/",as.numeric(summary(pois_obj$predicted)[6])),
  paste(as.numeric(summary(qpois_obj$actual)[6]),"/",as.numeric(summary(qpois_obj$predicted)[6])),
  paste(as.numeric(summary(neg_obj$actual)[6]),"/",as.numeric(summary(neg_obj$predicted)[6])),
  paste(as.numeric(summary(hur_obj$actual)[6]),"/",as.numeric(summary(hur_obj$predicted)[6]))
))
rownames(table_reg) = c('mod_comments','author_comments','p_votes','p_comments','score', 'mean res', 
                        'mean actual vs. predicted', 'median actual vs. predicted', 'min actual vs. predicted',
                        'max actual vs. predicted')

###
# After an inspection of differente regression models
# the best option appears to be simple linear regression
# Below the linear models are computed again for
# proportion of participants and for proportion
# of distinct comment authors
###

# Proportion of participants
linear_model = lm(m_ideas$p_participants ~ m_ideas$mod_comments+m_ideas$author_comments+
                  m_ideas$p_votes+m_ideas$p_comments+m_ideas$score)
summary(linear_model)
round(coef(linear_model), 5)
# Plot actual vs. predicted values
plot(linear_model$fitted,m_ideas$p_participants,pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Linear Regression")
# Plot residual
plot(linear_model)

# Transform to log
aux_m_ideas = dplyr::select(m_ideas, p_participants, mod_comments, author_comments,
                            p_votes, p_comments, score)
log_m_ideas = data.frame(sapply(aux_m_ideas, function(x) log(x+0.5)))
# Fit linear model with transformed data
log_linear_model = lm(p_participants ~ mod_comments+author_comments+
                      p_votes+p_comments+score, data=log_m_ideas)
summary(log_linear_model)
# Plot actual vs. predicted values
plot(log_linear_model$fitted,log_m_ideas$p_participants,pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Linear Regression")
# Plot residual
plot(log_linear_model)


# Exclusing votes and comments because
# author and mod comments are correlated with
# comments and score is correlated with votes
cor(m_ideas$comments,m_ideas$mod_comments)
cor(m_ideas$comments,m_ideas$author_comments)
cor(m_ideas$votes, m_ideas$score)
linear_model = lm(m_ideas$p_participants ~ m_ideas$mod_comments+m_ideas$author_comments+
                  m_ideas$score)
summary(linear_model)
# Plot actual vs. predicted values
plot(linear_model$fitted,m_ideas$p_participants,pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Linear Regression")
# Plot residual
plot(linear_model)

# Excluding votes and comment but log
#log_p_participants = sign(m_ideas$p_participants) * log(abs(m_ideas$p_participants) + 1)
log_p_participants = log(m_ideas$p_participants+0.5)
log_linear_model = lm(log_p_participants ~ m_ideas$mod_comments+
                       m_ideas$author_comments+m_ideas$score)
summary(log_linear_model)
# Plot actual vs. predicted values
plot(log_linear_model$fitted,log_p_participants,pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Linear Regression")
# Plot residual
plot(log_linear_model)

# Draw variables
hist(m_ideas$p_participants,
main='DV: Participants on ideas', 
xlab='Participants by members',
col='green')
hist(m_ideas$mod_comments,
     main='IV: Moderator comments on ideas', 
     xlab='Moderator comments',
     col='blue')
hist(m_ideas$author_comments,
     main='IV: Author comments on ideas', 
     xlab='Author comments',
     col='blue')
hist(m_ideas$score,
     main='IV: Score of ideas', 
     xlab='Score',
     col='blue')

# Proportion of distinct comment authors
linear_model = lm(m_ideas$p_distinct_comment_authors ~ m_ideas$mod_comments+
                  m_ideas$author_comments+m_ideas$p_votes+m_ideas$p_comments+m_ideas$score)
summary(linear_model)
round(coef(linear_model), 5)
# Plot actual vs. predicted values
plot(linear_model$fitted,m_ideas$p_distinct_comment_authors,pch=19,
     col="grey",ylab="Actual Rates",xlab="Predicted Rates", 
     main="Linear Regression")
# Plot residual
plot(linear_model)
####################################################################################

# Two-part models
# First: Logistic Reg.
# logRegPart = glm(has_participants ~ mod_comments+author_comments+p_comments+
#                    p_votes+score, 
#                  data=m_ideas, family="binomial",
#                  maxit = 100)
# # Test how good the model fits the data (>1)
# hos_t = hoslem.test(logRegPart$y, fitted(logRegPart), g=10)
# cbind(hos_t$observed,hos_t$expected)
# # Summary of coeficients
# summary(logRegPart)
# # Draw diagnosis plots
# res = residuals(logRegPart)
# plot(predict(logRegPart), res)
# qqnorm(res)
# qqline(res)
# # Second: QuasiPoisson
# part_ideas = m_ideas[m_ideas$participants>0,]
# pois = glm(participants ~ mod_comments+author_comments+p_comments+
#              p_votes+score+offset(log(users)), data=part_ideas,
#            family="poisson")
# # Summary of coeficients
# summary(pois)
# exp(coef(pois))[1:6]
# # Draw plots
# plot(predict(pois), residuals(pois))
# hist(residuals(pois))
# qqnorm(residuals(pois))
# qqline(residuals(pois))


# Apply Hurdle Model
# Justification.
# Poisson: var==mean
# Negative Binomial: var>mean
# None of these is the case.
# Moreover, the data have many zeros
# (about 50%). The glm that best fits should be
# one developed to cope high occurrence of zeros
# in the outcome.
# Zero-inflation assumes that zeros have two
# different origins structural and samplings,
# which is not the case. Also, zero-inflation
# doesn't consider the existance of over-dispersion.
# Hurdle models are prepared to treat high occurence
# of zeros and it assumes that zeros come from the same
# source, which is our case.
# fm_hurdle0 = hurdle(participants ~ mod_comments+author_comments+
#                       p_votes+p_comments+score+offset(log(users)), 
#                     data = m_ideas, dist = "negbin", 
#                     zero.dist = "binomial")
# # Show coeficients
# summary(fm_hurdle0)
# est = cbind(Estimate = coef(fm_hurdle0), confint(fm_hurdle0))
# exp(est)
# exp(coef(fm_hurdle0))[1:6]
# # Get model coefficients
# b0 = as.numeric(coef(fm_hurdle0)[1])
# b1 = as.numeric(coef(fm_hurdle0)[2])
# b2 = as.numeric(coef(fm_hurdle0)[3])
# b3 = as.numeric(coef(fm_hurdle0)[4])
# b4 = as.numeric(coef(fm_hurdle0)[5])
# b5 = as.numeric(coef(fm_hurdle0)[6])
# # Compute logged participant proportions
# log_part_users = b0 + b1 * m_ideas$mod_comments + b2 * m_ideas$author_comments +
#   b3 * m_ideas$p_votes + b4 * m_ideas$p_comments + b5 * m_ideas$score
# # Create dataset with actual and predicted values
# aux_t = data.frame(idea_id=m_ideas$idea_id,
#                    users=m_ideas$users,
#                    participants=m_ideas$participants,
#                    p_participants=round(m_ideas$participants/m_ideas$users,4),
#                    p_participants_hat=round(exp(log_part_users),4))
# # Plot predicted vs. actual
# plot(aux_t$p_participants, aux_t$p_participants_hat, pch=19,
#      col="grey",ylab="Predicted Rates",xlab="Actual Rates", 
#      main="Quasipoisson")
# #Plot residuals
# res = residuals(fm_hurdle0, type="pearson")
# plot(predict(fm_hurdle0), res, pch=19,
#      col="grey",ylab="Residuals",xlab="Fitted")
# abline(h=0, lty=2)
# qqnorm(res)
# qqline(res)
# # Draw histogram of residuals
# hist(abs(res))
# # Draw fitted vs. residuals
# aux_fitted = data.frame(fitted=predict(fm_hurdle0, type="response"),
#                         residuals=fm_hurdle0$residuals,
#                         y=m_ideas$participants, off=m_ideas$users)
# aux_fitted = aux_fitted[aux_fitted$fitted<=50,]
# plot(aux_fitted$fitted/aux_fitted$off,aux_fitted$y/aux_fitted$users,
#      pch=19, col="grey",ylab="Actual",xlab="Predicted")
# 
# aux_t = cbind(round(m_ideas$p_participants,4),round(fm_hurdle0$fitted,4))
# colnames(aux_t) = c('actual','predicted')
# plot(aux_fitted$fitted,log(aux_fitted$y),pch=19,
#      col="grey",ylab="Actual",xlab="Predicted")
# cbind(summary(log(m_ideas$p_participants)),summary(fm_hurdle0$fitted))

# Check overdispersion in data (alpha > 0)
# pois = glm(participants ~ mod_comments+author_comments+p_comments+
#              p_votes+score+offset(log(users)), data=m_ideas,
#            family="poisson")
# dispersiontest(pois,trafo=1)

# Checking model assumptions (chisquare < 0.05)
# X2 = 2 * (logLik(fm_hurdle0) - logLik(pois))
# pchisq(X2, df = 1, lower.tail=FALSE)

# Bp test indicates the presence of heteroscedasticity in the model
# Cut-off point with large values of Cook's D.
# d1 = cooks.distance(fit)
# r = stdres(fit)
# a = cbind(ideas, d1, r)
# problematic_rows = rownames(a[d1 > 4/n, ])
# new_ideas = ideas[-as.numeric(problematic_rows),]
# new_fit = lm(p_participants~mod_comments+author_comments+p_comments+p_votes+author_admin+score,
#              data=new_ideas)
# summary(new_fit)
# bptest(new_fit)
# plot(new_fit)

# Conduct robust linear regression
# r_fit = rlm(p_participants~mod_comments+author_comments+p_comments+p_votes,data=ideas)
# summary(r_fit)
# plot(r_fit)
# bptest(r_fit) # even robust regression has heteroscedasticity 
# 
# fit = lm(p_distinct_comment_authors~mod_comments+author_comments+p_comments+p_votes+author_admin+score,
#          data=ideas)
# summary(fit)
# plot(fit)
# bptest(fit)
# 
# # Model creation
# m_basic = lm(p_participants~p_votes+score,data=ideas)
# summary(m_basic)
# plot(m_basic)
# bptest(m_basic)
# 
# # Model creation
# cor(ideas$mod_comments,ideas$author_comments)
# m_author_comments_mod_comments = lm(author_comments~mod_comments,data=ideas)
# summary(m_author_comments_mod_comments)
# bptest(m_author_comments_mod_comments)
# plot(m_author_comments_mod_comments)
# 
# # Model diagnosis
# plot(residuals(fit))
# title("Residual Plot")
# acf(residuals(fit), main = ""); 
# title("Residual Autocorrelation Plot")
# plot(fitted(fit),resid(fit))
# title("Residual vs Fit. value")
# plot(y, residuals(fit) ); 
# title( "Residual vs Obs. value")
# bptest(fit) # low p-value indicates the presence of heteroscedasticity
# 
# # Residuals show systematic patterns (poor model fit), convert to logged and try again
# log_ideas = data.frame(sapply(select(ideas, -author_admin, -idea_id), 
#                               function(x) ifelse(x>0,log2(x),0)))
# fit = lm(participants~mod_comments+author_comments+comments+votes,data=log_ideas)
# summary(fit)
# plot(fit) # <- Residuals show strong systemic patterns
# 
# # Analyze the impact of different independent variables in the number of distinct comment posters
# fit = lm(distinct_comment_authors~mod_comments+author_comments+comments+votes,data=ideas)
# summary(fit)
# plot(fit)  # <- Residuals show strong systemic patterns
# # Residuals show systemic patterns, try again with logged data
# fit = lm(distinct_comment_authors~mod_comments+author_comments+comments+votes,data=log_ideas)
# summary(fit)
# plot(fit) # <- Residuals show strong systemic patterns
# 
# 
# # Only on ideas with activities (votes and comments greater than zero)
# active_ideas = filter(ideas, votes>0|comments>0)
# y = active_ideas$participants
# fit = lm(y~mod_comments+author_comments+comments+votes,data=active_ideas)
# summary(fit)
# plot(fit) # <- Residuals show strong systemic patterns
# log_ideas = data.frame(sapply(select(active_ideas, -author_admin, -idea_id), 
#                               function(x) ifelse(x>0,log2(x),0)))
# fit = lm(participants~mod_comments+author_comments+comments+votes,data=log_ideas)
# summary(fit)
# plot(fit) # <- Residuals show strong systemic patterns
# 
# y=active_ideas$distinct_comment_authors
# fit = lm(y~mod_comments+author_comments+comments+votes,data=active_ideas)
# summary(fit) #<- Residuals show strong systemic patterns
# fit = lm(distinct_comment_authors~mod_comments+author_comments+comments+votes,data=log_ideas)
# summary(fit)
# plot(fit) # <- Residuals show strong systemic patterns