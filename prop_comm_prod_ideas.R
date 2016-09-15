####
# Script that analyze the
# following research question:
# Do exists properties of communities 
# that influence their productivity of ideas?
####

# We are going to employ multi-variable
# linear regression to conduct the analyzes
# Assumptions:
# 1- Predictors shouldn't be correlated
# 2- Predictors should be linearly related with
# dependent variable
#
####

####
# STEPS TO PERFORM THE  ANALYSIS
# 1) Select independent and dependent variables
# 2) Compute descriptive statistics of vars (mean, median, std, variance, min, max)
# 3) Computer correlation matrix and see whether independent and dependent vars are correlated
# 4) Analyze linear relationship between dependent and independent vars
# (deviations from regression line should be across the whole range, in other
# words the vertial deviations from the regression line should be identically 
# distributed)
# 5) Draw residual vs predicted picture and look for systemic patterns
# 6) If they aren't lineraly related apply natural log transformation to variables
# (dependent and independent)
###

###
#
# HYPOTHESES
# H1: Community responsiveness influence possitively the productivity of ideas -- NO
# H2: Quality of content influence possitively the productivity of ideas -- NO
# H3: Moderator intervention influence possitively the productivity of ideas -- partially
#
###

###
#
# METRICS
## Dependent variables
# Productivity of ideas
# - ratio of total ideas by total members
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
# - compare level of participation in ideas the have intervention vs ideas that don't have it
#
###


# Load library
library(dplyr)
library(car)
source('utils.R')

# Load data
communities = read.csv('data/new_metrics.csv')
mod_inter = read.csv("./data/total_mod_interventions.csv", sep=",", header=T)
gral_comm = read.csv("./data/final_communities.csv", sep=",", header=T, skip=1)
tagged_communities = read.csv("./data/tagged_communities_and_archetypes.csv", sep=",", header=T)

# Select only tagged communities
communities = filter(communities, community_id %in% tagged_communities$community_id)
gral_comm = filter(gral_comm, id %in% tagged_communities$community_id)

# Draw distribution of variables
hist(communities$ideas_by_members,
     main='DV: Productivity', 
     xlab='Ideas by members',
     col='green')

### =============================================
# ---------------  STUDY: H1 --------------------
# Does community responsiveness influence possitively the 
# productivity of ideas?
## Dependent variable
# Productivity of ideas: 
# - ratio of total ideas by total members
## Independent variable
# Community responsivess:
# - ratio of ideas attended by total ideas
# - ratio of comments attended by total comments

# Select variables
h1_df = communities %>% dplyr::select(community_id,
                                      ideas_by_members,
                                      attended_ideas_by_ideas,
                                      attended_comments_by_comments)

# Identify outliers
outlier_ids = h1_df[h1_df$ideas_by_members %in% tukey_outlier(h1_df$ideas_by_members)$outliers, 
                    'community_id']
outlier_ids = c(outlier_ids, h1_df[h1_df$attended_ideas_by_ideas %in% tukey_outlier(h1_df$attended_ideas_by_ideas)$outliers, 
                                   'community_id'])
outlier_ids = c(outlier_ids, h1_df[h1_df$attended_comments_by_comments %in% 
                                     tukey_outlier(h1_df$attended_comments_by_comments)$outliers, 
                                   'community_id'])

# Remove outliers and index column
h1_df = h1_df %>% filter(! community_id %in% unique(outlier_ids)) %>% select(-community_id)

# Compute descriptive statistics
summary(h1_df[,2:ncol(h1_df)])

# Draw scatter plots
scatterplotMatrix(~ideas_by_members+attended_ideas_by_ideas+attended_comments_by_comments, data=h1_df)

# Get correlation matrix
cor(h1_df)
cor.test(h1_df$attended_ideas_by_ideas,h1_df$ideas_by_members)
cor.test(h1_df$attended_comments_by_comments,h1_df$ideas_by_members)
cor.test(h1_df$attended_comments_by_comments,h1_df$attended_ideas_by_ideas)

# Draw dependet var vs independent vars
par(mfrow=c(1,2))
dep_log = log(h1_df[,'ideas_by_members']+0.5)
trans_attended_ideas = transform_count_var_to_factor(h1_df[,'attended_ideas_by_ideas'])  
plot(dep_log ~ trans_attended_ideas, data=h1_df, 
     ylab='ideas_by_members', 
     xlab='attended_ideas_by_ideas')
trans_attended_comments = transform_count_var_to_factor(h1_df[,'attended_comments_by_comments'])  
plot(dep_log ~ trans_attended_comments, data=h1_df, 
     ylab='ideas_by_members', 
     xlab='attended_comments_by_comments')


# Fit model
fit = lm(ideas_by_members ~ ., h1_df)
summary(fit)

# Residuals
plot(fit)

# Apply natural logarithmic normalization (base 2)
log_h1_df = data.frame(sapply(h1_df, function(x) ifelse(x>0,log2(x),0)))

# Fit model with transformed vars
fit = lm(idea_by_members ~ ., log_h1_df)
summary(fit)

# Residuals
plot(fit)

#- Conclusion: 
#- It coudn't be checked that the choosen 
#- community responsinevess metrics influence possitively
#- the productivity of ideas
### =============================================


### =============================================
# ---------------  STUDY: H2 --------------------
# Does the quality of content influence possitively 
# productivity of ideas?
## Dependent variable
# Productivity of ideas: 
# - ratio of total ideas by total members
## Independent variable
# Quality of content:
# - ratio of irrelevant ideas by total ideas
# - ratio of total tags used to organize ideas by total ideas

# Select variables
h2_df = communities %>% dplyr::select(community_id, ideas_by_members, irrelevant_ideas_by_ideas, tags_by_ideas)

# Identify outliers
outlier_ids = h2_df[h2_df$ideas_by_members %in% tukey_outlier(h2_df$ideas_by_members)$outliers, 
                    'community_id']
outlier_ids = c(outlier_ids, h2_df[h2_df$irrelevant_ideas_by_ideas %in% tukey_outlier(h2_df$irrelevant_ideas_by_ideas)$outliers, 
                                   'community_id'])
outlier_ids = c(outlier_ids, h2_df[h2_df$tags_by_ideas %in% 
                                     tukey_outlier(h2_df$tags_by_ideas)$outliers, 
                                   'community_id'])

# Remove outliers and index column
h2_df = h2_df %>% filter(! community_id %in% unique(outlier_ids)) %>% select(-community_id)

# Compute descriptive statistics
summary(h2_df)

# Draw histograms to inspect outliers
hist(h2_df$tags_by_ideas)

# Draw scatter plots
scatterplotMatrix(~ideas_by_members+irrelevant_ideas_by_ideas+tags_by_ideas, data=h2_df)

# Compute correlation matrix
cor(h2_df)

# Draw dependent var vs independent vars
par(mfrow=c(1,2))
dep_log = log(h2_df[,'ideas_by_members']+0.5)
trans_irrelevant_ideas = transform_count_var_to_factor(h2_df[,'irrelevant_ideas_by_ideas'])  
plot(dep_log ~ trans_attended_ideas, data=h1_df, 
     ylab='log(ideas_by_members)', 
     xlab='irrelevant_ideas_by_ideas')
trans_tags_by_ideas = transform_count_var_to_factor(h2_df[,'tags_by_ideas'])  
plot(dep_log ~ trans_tags_by_ideas, data=h2_df, 
     ylab='ideas_by_members', 
     xlab='tags_by_ideas')

# Apply natural logarithmic normalization (base 2)
log_h2_df = data.frame(sapply(h2_df, function(x) ifelse(x>0,log(x,2),0)))

# Draw (again) scatter plots
scatterplotMatrix(~ideas_by_members+irrelevant_ideas_by_ideas+tags_by_ideas, data=log_h2_df)

# Compute (again) correlation matrix
cor(log_h2_df)

# Fit model
fit = lm(ideas_by_members ~ ., h2_df)
summary(fit)

# Residuals
plot(fit)

# Fit model with transformed vars
fit = lm(ideas_by_members ~ ., log_h2_df)
summary(fit)

# Residuals
plot(fit)
### =============================================


### =============================================
# ---------------  STUDY: H3 --------------------
# Do moderator intervention influence possitively the 
# productivity of ideas?
## Dependent variable
# Productivity of ideas: 
# - ratio of total ideas by total members
## Independent variable
# - ratio of moderators by total members (quantity of moderators)
# - ratio of content created by moderators by total content (ideas+comments+votes) (size of interv.)
# - ratio of total moderators' ideas by total interventions (type of intervention)
# - ratio of total moderators' votes by total interventions (type of intervention)
# - ratio of total moderators' comments by total interventions (type of intervention)

# Select variables
h3_df = communities %>% dplyr::select(community_id, ideas_by_members, moderators_by_members)

# Create dataset
gral_comm2 = dplyr::select(gral_comm, id, ideas, comments, votes, users)
mod_inter_df = merge(gral_comm2, mod_inter,by.x="id",by.y="community_id")
mod_inter_df = mutate(mod_inter_df, total_content=ideas+votes+comments)
mod_inter_df = mutate(mod_inter_df, size_inter=tot_inter/total_content)
h3_df = merge(h3_df,dplyr::select(mod_inter_df, id, size_inter, users, ideas),
              by.x="community_id",by.y="id")

# Identify outliers
outlier_ids = h3_df[h3_df$ideas_by_members %in% tukey_outlier(h3_df$ideas_by_members)$outliers, 
                    'community_id']
outlier_ids = c(outlier_ids, h3_df[h3_df$size_inter %in% tukey_outlier(h3_df$size_inter)$outliers, 
                                   'community_id'])
outlier_ids = c(outlier_ids, h3_df[h3_df$moderators_by_members %in% 
                                     tukey_outlier(h3_df$moderators_by_members)$outliers, 
                                   'community_id'])

# Remove outliers and index column
# h3_df = h3_df %>% filter(! community_id %in% unique(outlier_ids)) %>% select(-community_id)

# Compute descriptive statistics
summary(h3_df)

# Draw histograms to inspect outliers
hist(h3_df$moderators_by_members)
hist(h3_df$size_inter)

# Draw scatter plots
scatterplotMatrix(~ideas_by_members+moderators_by_members+size_inter, data=h3_df)

# Compute correlation matrix
cor(h3_df)

# Draw dependent var vs independent vars
par(mfrow=c(1,2))
dep_log = log(h3_df[,'ideas_by_members']+0.5)
trans_mod_by_members = transform_count_var_to_factor(h3_df[,'moderators_by_members'])  
plot(dep_log ~ trans_mod_by_members, data=h3_df, 
     ylab='log(ideas_by_members)', 
     xlab='moderators_by_members')
trans_size_inter = transform_count_var_to_factor(h3_df[,'size_inter'])  
plot(dep_log ~ trans_size_inter, data=h3_df, 
     ylab='log(ideas_by_members)', 
     xlab='size_inter')

# Plot dependent variables
par(mfrow=c(1,1))
plot(table(h3_df$ideas_by_members))

# Fit model with poisson
fm_pois = glm(ideas ~ moderators_by_members+size_inter, offset=log(users),
              data = h3_df, family = poisson)
summary(fm_pois)

# Compute predicted values
glm_coefficients = as.numeric(summary(fm_pois)$coef[,1])
log_y = glm_coefficients[1] + glm_coefficients[2] * h3_df$moderators_by_members + 
        glm_coefficients[3] * h3_df$size_inter
y_hat = exp(log_y)

# Save predicted and actual in df
pred_act = data.frame(predicted=y_hat,actual=h3_df$ideas/h3_df$users)

# Plot predited vs. actual
pred_act = filter(pred_act, predicted<=5, actual<=5)
plot(pred_act$predicted,pred_act$actual,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="H9 - Poisson Regression (Predicted vs. Actual)")

# Fit linear model
lm = lm(ideas_by_members ~ moderators_by_members + size_inter, h3_df)
summary(lm)

# Draw residuals
plot(lm)

# Save predicted and actual
pred_act = data.frame(predicted=lm$fitted,actual=h3_df$ideas_by_members)

# Draw predicted vs. actual
plot(pred_act$predicted,pred_act$actual,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="H9 - Linear Regression (Predicted vs. Actual)")

# Apply natural logarithmic normalization (base 2)
log_h3_df = data.frame(sapply(h3_df, function(x) log(x+0.5)))

# Draw (again) scatter plots
scatterplotMatrix(~ideas_by_members+moderators_by_members+size_inter, data=log_h3_df)

# Compute (again) correlation matrix
cor(log_h3_df)

# Fit model
log_fit = lm(h3_df$ideas_by_members ~ log_h3_df$moderators_by_members + log_h3_df$size_inter)
summary(log_fit)

# Draw residuals
plot(log_fit)

# Save predicted and actual
pred_act = data.frame(predicted=log_fit$fitted,actual=h3_df$ideas_by_members)

# Draw predicted vs. actual
plot(pred_act$predicted,pred_act$actual,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="H9 - Linear Regression (Predicted vs. Actual)")

##
# Conclusion:
# Only the ratio between the number of moderators 
# by the total number of members shows to influence
# signficantly and possitively the productivity
# of ideas. Therefore, hypothesis 3 can be supported
# partially.
### =============================================

###
## Analysis that includes all independent variables together
###
h_df = dplyr::select(communities, community_id, ideas_by_members,
                     attended_ideas_by_ideas, attended_comments_by_comments,
                     irrelevant_ideas_by_ideas, tags_by_ideas,
                     moderators_by_members)

# Create dataset
gral_comm2 = dplyr::select(gral_comm, id, ideas, comments, votes, contributors, users)
mod_inter_df = merge(gral_comm2, mod_inter,by.x="id",by.y="community_id")
mod_inter_df = mutate(mod_inter_df, total_content=ideas+votes+comments)
mod_inter_df = mutate(mod_inter_df, size_inter=tot_inter/total_content)
h_df = merge(h_df, dplyr::select(mod_inter_df, id, size_inter, users, contributors), 
             by.x="community_id",by.y="id")

# Fit model
rq2_lm = lm(ideas_by_members ~ attended_ideas_by_ideas+attended_comments_by_comments+
            irrelevant_ideas_by_ideas+tags_by_ideas+moderators_by_members+size_inter, 
            h_df)
summary(rq2_lm)

# Residual
plot(rq2_lm)

# Plot predited vs. actual
plot(rq2_lm$fitted,h_df$ideas_by_members,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="RQ2 - Linear Regression (Predicted vs. Actual)")

# Transform to log
log_h_df = data.frame(sapply(h_df, function(x) log(x+0.5)))

# Fit model with fitted data
rq2_log_lm = lm(ideas_by_members ~ attended_ideas_by_ideas+attended_comments_by_comments+
                irrelevant_ideas_by_ideas+tags_by_ideas+moderators_by_members+size_inter, 
                log_h_df)
summary(rq2_log_lm)

# Residual
plot(rq2_log_lm)

# Plot predited vs. actual
plot(rq2_log_lm$fitted,h_df$ideas_by_members,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="RQ2 - Linear Regression (Predicted vs. Actual)")