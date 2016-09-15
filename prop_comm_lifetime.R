####
# Script that analyze the following 
# research question
# Do exists properties of communities that influence 
# their lifetime?
####

###
#
# HYPOTHESES
# H7: Community responsiveness influence possitively the lifetime of communities (i.e., last more) -- NO
# H8: Quality of content influence possitively the lifetime of communities -- NO
# H9: Moderator intervention influence possitively the lifetime of communities -- NO
#
###

###
#
# METRICS
## Dependent variables
# Lifetime
# - time window between the date time of the first ideas and the date time of the last idea/comment/vote
## Independent variables
# Community responsivess:
# - ratio of ideas attended by total ideas
# - ratio of comments attended by total comments
## Quality of content:
# - ratio of irrelevant ideas by total ideas
# - ratio of total tags used to organize ideas by total ideas
## Moderator intervention:
# - ratio of moderators by total members (quantity of moderators)
# - ratio of content created by moderators by total content (ideas+comments+votes) (size of interv.)
# - ratio of total moderators' ideas by total interventions (type of intervention)
# - ratio of total moderators' votes by total interventions (type of intervention)
# - ratio of total moderators' comments by total interventions (type of intervention)
#
###

# Load library
library(dplyr)
library(car)
source('utils.R')

# Load data
communities = read.csv('data/new_metrics.csv')
lifetimes = read.csv("./data/idsc_communities_lifetime.csv", sep=",", header=T)
mod_inter = read.csv("./data/total_mod_interventions.csv", sep=",", header=T)
gral_comm = read.csv("./data/final_communities.csv", sep=",", header=T, skip=1)


### =============================================
# ---------------  STUDY: H7 --------------------
# Does community responsiveness influence possitively the 
# lifetime of communities (last more)?
## Dependent variable
# Lifetime
# - time window between the date time of the first ideas and 
# the date time of the last idea/comment/vote
## Independent variable
# Community responsivess:
# - ratio of ideas attended by total ideas
# - ratio of comments attended by total comments

# Select variables
h7_df = communities %>% dplyr::select(community_id, attended_ideas_by_ideas,
                                      attended_comments_by_comments)

# Create dataset
h7_df = merge(h7_df,lifetimes,by.x="community_id",by.y="id")

# Identify outliers
outliers_ids = h7_df[h7_df$attended_ideas_by_ideas %in% 
                       tukey_outlier(h7_df$attended_ideas_by_ideas)$outliers, 
                     'community_id']
outliers_ids = c(outliers_ids, h7_df[h7_df$attended_comments_by_comments %in% 
                                       tukey_outlier(h7_df$attended_comments_by_comments)$outliers, 
                                     'community_id'])
outliers_ids = c(outliers_ids, h7_df[h7_df$lifetime_days %in% 
                                       tukey_outlier(h7_df$lifetime_days)$outliers, 
                                     'community_id'])

# Remove outliers and index column
h7_df = h7_df %>% filter(! community_id %in% outliers_ids) %>% select(-community_id)

# Compute descriptive statistics
summary(h7_df)

# Draw histograms and boxplots
hist(h7_df$attended_ideas_by_ideas)
boxplot(h7_df$attended_ideas_by_ideas)
hist(h7_df$attended_comments_by_comments)
boxplot(h7_df$attended_comments_by_comments)
hist(h7_df$lifetime_days)
boxplot(h7_df$lifetime_days)

# Draw scatter plots
scatterplotMatrix(~lifetime_days+attended_ideas_by_ideas+attended_comments_by_comments, data=h7_df)

# Compute correlation matrix
cor(h7_df)

# Draw dependent var vs independent vars
par(mfrow=c(1,2))
dep_log = log(h7_df[,'lifetime_days']+0.5)
trans_iv1 = transform_count_var_to_factor(h7_df[,'attended_ideas_by_ideas'])  
plot(dep_log ~ trans_iv1, data=h7_df, 
     ylab='log(lifetime_days)', 
     xlab='attended_ideas_by_ideas')
trans_iv2 = transform_count_var_to_factor(h7_df[,'attended_comments_by_comments'])  
plot(dep_log ~ trans_iv2, data=h7_df, 
     ylab='log(ideas_by_members)', 
     xlab='attended_comments_by_comments')

# Fit poisson model
fm_pois = glm(round(lifetime_days,0) ~ attended_ideas_by_ideas+attended_comments_by_comments,
              data = h7_df, family = poisson)
summary(fm_pois)
plot(fm_pois$fitted,fm_pois$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Fit quasipoisson model
m_poisson = glm(round(lifetime_days,0) ~ attended_ideas_by_ideas+attended_comments_by_comments,
                data = h7_df, family=quasipoisson)
summary(m_poisson)
plot(m_poisson$fitted,m_poisson$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Fit negative binomial model
fm_nbin = glm.nb(round(lifetime_days,0) ~ attended_ideas_by_ideas+attended_comments_by_comments, 
                 data=h7_df)
summary(fm_nbin)
plot(fm_nbin$fitted,fm_nbin$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Comparison
fm = list("ML-Pois" = fm_pois, "Quasi-Pois" = m_poisson, "NB" = fm_nbin)
sapply(fm, function(x) coef(x)[1:3])
# Fitted likelihood
rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),
      Df = sapply(fm, function(x) attr(logLik(x), "df")))

# Fit model with transformed variables
fit = lm(lifetime_days ~ ., h7_df)
summary(fit)

# Draw Residuals
plot(fit)
### =============================================


### =============================================
# ---------------  STUDY: H8 --------------------
# Does the quality of content influence possitively the 
# lifetime of communities?
## Dependent variable
# Lifetime
# - time window between the date time of the first ideas and 
# the date time of the last idea/comment/vote
## Independent variable
## Quality of content:
# - ratio of irrelevant ideas by total ideas
# - ratio of total tags used to organize ideas by total ideas

# Select variables
h8_df = communities %>% dplyr::select(community_id, irrelevant_ideas_by_ideas, tags_by_ideas)

# Create dataset
h8_df = merge(h8_df,lifetimes,by.x="community_id",by.y="id")

# Identify outliers
outliers_ids = h8_df[h8_df$irrelevant_ideas_by_ideas %in% 
                     tukey_outlier(h8_df$irrelevant_ideas_by_ideas)$outliers, 
                     'community_id']
outliers_ids = c(outliers_ids, h8_df[h8_df$tags_by_ideas %in% 
                                     tukey_outlier(h8_df$tags_by_ideas)$outliers, 
                                     'community_id'])
outliers_ids = c(outliers_ids, h8_df[h8_df$lifetime_days %in% 
                                     tukey_outlier(h8_df$lifetime_days)$outliers, 
                                     'community_id'])

# Remove outliers and index column
h8_df = h8_df %>% filter(! community_id %in% unique(outliers_ids)) %>% select(-community_id)

# Compute descriptive statistics
summary(h8_df)

# Draw histograms and boxplots
hist(h8_df$irrelevant_ideas_by_ideas)
hist(h8_df$tags_by_ideas)
hist(h8_df$lifetime_days)

# Draw scatter plots
scatterplotMatrix(~lifetime_days+irrelevant_ideas_by_ideas+tags_by_ideas, data=h8_df)

# Compute correlation matrix
cor(h8_df)

# Draw dependent var vs independent vars
par(mfrow=c(1,2))
dep_log = log(h8_df[,'lifetime_days']+0.5)
trans_iv1 = transform_count_var_to_factor(h8_df[,'irrelevant_ideas_by_ideas'])  
plot(dep_log ~ trans_iv1, data=h8_df, 
     ylab='log(lifetime_days)', 
     xlab='irrelevant_ideas_by_ideas')
trans_iv2 = transform_count_var_to_factor(h8_df[,'tags_by_ideas'])  
plot(dep_log ~ trans_iv2, data=h8_df, 
     ylab='log(ideas_by_members)', 
     xlab='tags_by_ideas')

# Fit quasipoisson model
m_poisson = glm(round(lifetime_days,0) ~ irrelevant_ideas_by_ideas+tags_by_ideas,
                data = h8_df, family=quasipoisson)
summary(m_poisson)
plot(m_poisson$fitted,m_poisson$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Fit negative binomial model
fm_nbin = glm.nb(round(lifetime_days,0) ~ irrelevant_ideas_by_ideas+tags_by_ideas, 
                 data=h8_df)
summary(fm_nbin)
plot(fm_nbin$fitted,fm_nbin$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Fit Hurdle Model
fm_hurdle0 = hurdle(round(lifetime_days,0) ~ irrelevant_ideas_by_ideas+tags_by_ideas, 
                    data = h8_df, dist = "negbin")
summary(fm_hurdle0)
plot(fm_hurdle0$fitted,fm_hurdle0$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Comparison
fm = list("Quasi-Pois" = m_poisson, "NB" = fm_nbin, "Hurdel NB" = fm_hurdle0)
sapply(fm, function(x) coef(x)[1:3])
# Fitted likelihood
rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),
      Df = sapply(fm, function(x) attr(logLik(x), "df")))

# Fit model with transformed variables
fit = lm(lifetime_days ~ ., h8_df)
summary(fit)

# Draw Residuals
plot(fit)
### =============================================


### =============================================
# ---------------  STUDY: H9 --------------------
# Do moderator intervention influence possitively 
# the lifetime of communities?
## Dependent variable
# Lifetime
# - time window between the date time of the first ideas and 
# the date time of the last idea/comment/vote
## Independent variable
# Moderator Intervention
# - ratio of moderators by total members (quantity of moderators)
# - ratio of content created by moderators by total content (ideas+comments+votes) (size of interv.)
# - ratio of total moderators' ideas by total interventions (type of intervention)
# - ratio of total moderators' votes by total interventions (type of intervention)
# - ratio of total moderators' comments by total interventions (type of intervention)

# Select variables
h9_df = communities %>% dplyr::select(community_id, moderators_by_members)

# Create dataset
gral_comm = dplyr::select(gral_comm, id, ideas, comments, votes)
mod_inter_df = merge(gral_comm, mod_inter,by.x="id",by.y="community_id")
mod_inter_df = mutate(mod_inter_df, total_content=ideas+votes+comments)
mod_inter_df = mutate(mod_inter_df, size_inter=tot_inter/total_content)
h9_df = merge(h9_df,dplyr::select(mod_inter_df, id, size_inter),by.x="community_id",by.y="id")
h9_df = merge(h9_df,lifetimes,by.x="community_id",by.y="id")

# Identify outliers
outlier_ids = h9_df[h9_df$moderators_by_members %in% tukey_outlier(h9_df$moderators_by_members)$outliers, 
                    'community_id']
outlier_ids = c(outlier_ids, h9_df[h9_df$size_inter %in% tukey_outlier(h9_df$size_inter)$outliers, 
                                  'community_id'])
outlier_ids = c(outlier_ids, h9_df[h9_df$lifetime_days %in% 
                                   tukey_outlier(h9_df$lifetime_days)$outliers, 
                                   'community_id'])

# Remove outliers and index column
h9_df = h9_df %>% filter(! community_id %in% unique(outlier_ids)) %>% select(-community_id)

# Compute descriptive statistics
summary(h9_df)

# Draw histograms and boxplots
hist(h9_df$moderators_by_members)
hist(h9_df$size_inter)
hist(h9_df$lifetime_days)

# Draw scatter plots
scatterplotMatrix(~lifetime_days+moderators_by_members+size_inter, data=h9_df)

# Compute correlation matrix
cor(h9_df)

# Draw dependent var vs independent vars
par(mfrow=c(1,2))
dep_log = log(h9_df[,'lifetime_days']+0.5)
trans_iv1 = transform_count_var_to_factor(h9_df[,'moderators_by_members'])  
plot(dep_log ~ trans_iv1, data=h9_df, 
     ylab='log(lifetime_days)', 
     xlab='moderators_by_members')
trans_iv2 = transform_count_var_to_factor(h9_df[,'size_inter'])  
plot(dep_log ~ trans_iv2, data=h9_df, 
     ylab='log(ideas_by_members)', 
     xlab='size_inter')

# Fit negative binomial model
fm_nbin = glm.nb(round(lifetime_days,0) ~ moderators_by_members+size_inter, 
                 data=h9_df)
summary(fm_nbin)
plot(h9_df$lifetime_days,fm_nbin$fitted,pch=19,col="grey",ylab="Fitted",xlab="Lifetime (days)")
res = residuals(fm_nbin, type="deviance")
plot(predict(fm_nbin), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

# Fit model with transformed variables
fit = lm(lifetime_days ~ ., h9_df)
summary(fit)

# Draw Residuals
plot(fit)