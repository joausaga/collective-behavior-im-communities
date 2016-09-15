####
# Script that analyze the following 
# research question:
# Do exists properties of communities that influence 
# participation?
####

###
#
# HYPOTHESES
# H4: Community responsiveness influence positively the level of participation -- YES
# H5: Quality of content influence positively the level of participation -- NO
# H6: Moderator intervention influence positively the level of participation -- (maybe) partially
#

###
#
# METRICS
## Dependent variables
# Level of participation
# - ratio of contributors (i.e., created at least 1 ideas, vote or comment) by total members
## Independent variables
# Community responsiveness:
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
library(ggplot2)
source('utils.R')

# Load data
communities = read.csv('data/derived/new_metrics.csv')
mod_inter = read.csv("./data/total_mod_interventions.csv", sep=",", header=T)
gral_comm = read.csv("./data/final_communities.csv", sep=",", header=T)
tagged_communities = read.csv('data/tagged_communities_and_archetypes.csv', header=T)

# Filter out communities that weren't tagged
communities = filter(communities, community_id %in% tagged_communities$community_id)
gral_comm = filter(gral_comm, id %in% tagged_communities$community_id)

# Draw distribution of vars
hist(communities$contributors_by_members, 
     main='DV: Contributors', xlab='Contributors by members',
     col='green')
hist(communities$attended_ideas_by_ideas,
     main='IV: Replied Ideas', 
     xlab='Replied ideas by ideas',
     col='blue')
hist(communities$attended_comments_by_comments,
     main='IV: Replied Comments', 
     xlab='Replied comments by comments',
     col='blue')
hist(communities$irrelevant_ideas_by_ideas,
     main='IV: Irrelevant Ideas', 
     xlab='Irrelevant ideas by ideas',
     col='blue')
hist(communities$tags_by_ideas,
     main='IV: Tags', 
     xlab='Tags by ideas',
     col='blue')
hist(communities$moderators_by_members,
     main='IV: Moderators by members', 
     xlab='Moderators by members',
     col='blue')

### =============================================
# ---------------  STUDY: H4 --------------------
# Does community responsiveness influence possitively 
# the level of participation?
## Dependent variable
# Level of participation
# - ratio of contributors (i.e., created at least 1 ideas, vote or comment) by total members
## Independent variable
# Community responsivess:
# - ratio of ideas attended by total ideas
# - ratio of comments attended by total comments

# Select variables
h4_df = communities %>% dplyr::select(community_id, contributors_by_members,
                                      attended_ideas_by_ideas,
                                      attended_comments_by_comments)

# Identify outliers
outlier_ids = h4_df[h4_df$contributors_by_members %in% tukey_outlier(h4_df$contributors_by_members)$outliers, 
                    'community_id']
outlier_ids = c(outlier_ids, h4_df[h4_df$attended_ideas_by_ideas %in% tukey_outlier(h4_df$attended_ideas_by_ideas)$outliers, 
                                   'community_id'])
outlier_ids = c(outlier_ids, h4_df[h4_df$attended_comments_by_comments %in% 
                                   tukey_outlier(h4_df$attended_comments_by_comments)$outliers, 
                                   'community_id'])

# Remove outliers and index column
# h4_df = h4_df %>% filter(! community_id %in% unique(outlier_ids)) %>% select(-community_id)
h4_df = h4_df %>% dplyr::select(-community_id)

# Compute descriptive statistics
summary(h4_df)

# Draw histograms
hist(h4_df$contributors_by_members)
hist(h4_df$attended_ideas_by_ideas)
hist(h4_df$attended_comments_by_comments)

# Draw scatter plots
scatterplotMatrix(~contributors_by_members+attended_ideas_by_ideas+attended_comments_by_comments, data=h4_df)

# Get correlation matrix
cor(h4_df)

# Apply natural logarithmic normalization (base 2)
log_h4_df = data.frame(sapply(h4_df[,c(2,3)], function(x) log(x+0.5)))

# Draw (again) scatter plots
# scatterplotMatrix(~contributors_by_members+attended_ideas_by_ideas+attended_comments_by_comments, data=log_h4_df)

# Compute (again) correlation matrix
# cor(log_h4_df)

# Fit model
fit = lm(contributors_by_members ~ ., h4_df)
summary(fit)

# Plot predited vs. actual
plot(fit$fitted,h4_df$contributors_by_members,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="H1 - Linear Regression (Predicted vs. Actual)")

# Residuals
plot(fit)

# Fit model with transformed vars
fit = lm(h4_df$contributors_by_members ~ log_h4_df$attended_ideas_by_ideas+log_h4_df$attended_comments_by_comments)
summary(fit)

# Residuals
plot(fit)

# Plot predited vs. actual
plot(fit$fitted,h4_df$contributors_by_members,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="H1 - Linear Regression (Predicted vs. Actual)")

# Conclusion:
# Apparently community responsiveness influence
# possitively and significantly in participation
#
### =============================================


### =============================================
# ---------------  STUDY: H5 --------------------
# Does the quality of content influence possitively 
# the level of participation?
## Dependent variable
# Level of participation
# - ratio of contributors (i.e., created at least 1 ideas, vote or comment) by total members
## Independent variable
# Quality of content:
# - ratio of irrelevant ideas by total ideas
# - ratio of total tags used to organize ideas by total ideas

# Select variables
h5_df = communities %>% select(community_id, contributors_by_members, 
                               irrelevant_ideas_by_ideas, tags_by_ideas)

# Identify outliers
outlier_ids = h5_df[h5_df$contributors_by_members %in% tukey_outlier(h5_df$contributors_by_members)$outliers, 
                    'community_id']
outlier_ids = c(outlier_ids, h5_df[h5_df$tags_by_ideas %in% tukey_outlier(h5_df$tags_by_ideas)$outliers, 
                                   'community_id'])

# Remove outliers and index column
h5_df = h5_df %>% filter(! community_id %in% unique(outlier_ids)) %>% select(-community_id)

# Compute descriptive statistics
summary(h5_df)

# Draw histograms
hist(h5_df$contributors_by_members)
hist(h5_df$irrelevant_ideas_by_ideas)
hist(h5_df$tags_by_ideas)

# Draw scatter plots
scatterplotMatrix(~contributors_by_members+irrelevant_ideas_by_ideas+tags_by_ideas, data=h5_df)

# Get correlation matrix
cor(h5_df)

# Apply natural logarithmic normalization (base 2)
log_h5_df = data.frame(sapply(h5_df, function(x) ifelse(x>0,log2(x),0)))

# Draw (again) scatter plots
scatterplotMatrix(~contributors_by_members+irrelevant_ideas_by_ideas+tags_by_ideas, data=log_h5_df)

# Compute (again) correlation matrix
cor(log_h5_df)

# Fit model
fit = lm(contributors_by_members ~ ., h5_df)
summary(fit)

# Residuals
plot(fit)

# Fit model with transformed data
fit = lm(contributors_by_members ~ ., log_h5_df)
summary(fit)

# Residuals
plot(fit)

# Conclusion
# Aparently there is positive and significant
# influence of tags in the participation however
# systemic patterns can be seen in the residuals.
# There, it would say no.
### =============================================


### =============================================
# ---------------  STUDY: H6 --------------------
# Do moderator intervention influence possitively 
# the level of participation?
## Dependent variable
# Level of participation
# - ratio of contributors (i.e., created at least 1 ideas, vote or comment) by total members
## Independent variable
# - ratio of moderators by total members (quantity of moderators)
# - ratio of content created by moderators by total content (ideas+comments+votes) (size of interv.)
# - ratio of total moderators' ideas by total interventions (type of intervention)
# - ratio of total moderators' votes by total interventions (type of intervention)
# - ratio of total moderators' comments by total interventions (type of intervention)

# Select variables
h6_df = communities %>% dplyr::select(community_id, contributors_by_members, 
                                      moderators_by_members)

# Create dataset
gral_comm2 = dplyr::select(gral_comm, id, ideas, comments, votes, contributors, users)
mod_inter_df = merge(gral_comm2, mod_inter,by.x="id",by.y="community_id")
mod_inter_df = mutate(mod_inter_df, total_content=ideas+votes+comments)
mod_inter_df = mutate(mod_inter_df, size_inter=tot_inter/total_content)
h6_df = merge(h6_df, dplyr::select(mod_inter_df, id, size_inter, users, contributors), 
              by.x="community_id",by.y="id")

# Draw distribution of size of interventions
hist(h6_df$size_inter,
     main='IV: Size mod. interventions', 
     xlab='Size intervention by total content',
     col='blue')

# Identify outliers
outlier_ids = h6_df[h6_df$contributors_by_members %in% tukey_outlier(h6_df$contributors_by_members)$outliers, 
                    'community_id']
outlier_ids = c(outlier_ids, h6_df[h6_df$size_inter %in% tukey_outlier(h6_df$size_inter)$outliers, 
                                   'community_id'])
outlier_ids = c(outlier_ids, h6_df[h6_df$moderators_by_members %in% 
                                   tukey_outlier(h6_df$moderators_by_members)$outliers, 
                                   'community_id'])

# Remove outliers and index column
# h6_df = h6_df %>% filter(! community_id %in% unique(outlier_ids)) %>% select(-community_id)
h6_df = h6_df %>% dplyr::select(-community_id)

# Compute descriptive statistics
summary(h6_df)

# Draw histograms to inspect outliers
hist(h6_df$contributors_by_members)
hist(h6_df$moderators_by_members)
hist(h6_df$size_inter)

# Draw scatter plots
scatterplotMatrix(~contributors_by_members+moderators_by_members+size_inter, data=h6_df)

# Compute correlation matrix
cor(h6_df)

# Apply natural logarithmic normalization (base 2)
log_h6_df = data.frame(sapply(h6_df, function(x) log(x+0.5)))

# Draw scatter plots
scatterplotMatrix(~contributors_by_members+moderators_by_members+size_inter, data=log_h6_df)

# Compute (again) correlation matrix
cor(log_h6_df)

# Fit model
fit = lm(contributors_by_members ~ moderators_by_members+size_inter, log_h6_df)
summary(fit)

# Residual
plot(fit)

# Fit model (disconcerting results)
fit = lm(contributors_by_members ~ moderators_by_members+size_inter, h6_df)
summary(fit)

# Residual
plot(fit)

# Plot predited vs. actual
plot(fit$fitted,h6_df$contributors_by_members,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="H3 - Linear Regression (Predicted vs. Actual)")

# Fit model with transformed variables
# fit = lm(contributors_by_members ~ ., log_h6_df)
# summary(fit)

# Residual
# plot(fit)

# Fit poisson model
pois_lm = glm(contributors ~ moderators_by_members + size_inter, offset=log(users), 
              data = h6_df, family = poisson(link = "log"))
summary(pois_lm)

# Compute predicted values
pois_coefficients = as.numeric(summary(pois_lm)$coef[,1])
log_y = pois_coefficients[1] + pois_coefficients[2] * h6_df$moderators_by_members + 
        pois_coefficients[3] * h6_df$size_inter
y_hat = exp(log_y)

# Save predicted and actual in df
pred_act = data.frame(predicted=y_hat,actual=h6_df$contributors/h6_df$users)

# Plot predited vs. actual
pred_act = filter(pred_act, predicted <= 1)
plot(pred_act$predicted,pred_act$actual,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="H3 - Poisson Regression (Predicted vs. Actual)")

# Fit negative binomial model
glm = glm.nb(contributors ~ moderators_by_members + size_inter+offset(log(users)), 
             data = h6_df)
summary(glm)

# Compute predicted values
glm_coefficients = as.numeric(summary(glm)$coef[,1])
log_y = glm_coefficients[1] + glm_coefficients[2] * h6_df$moderators_by_members + 
        glm_coefficients[3] * h6_df$size_inter
y_hat = exp(log_y)

# Save predicted and actual in df
pred_act = data.frame(predicted=y_hat,actual=h6_df$contributors/h6_df$users)

# Plot predited vs. actual
pred_act = filter(pred_act, predicted <= 1)
plot(pred_act$predicted,pred_act$actual,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="H3 - Negative Binomial Regression (Predicted vs. Actual)")


# Conclusion:
# The result is confusing. The number of moderators seems 
# to impact positively and significantly in participation 
# but as much the moderators intervene in the community as
# less participation exists.
# Systematic patterns can be seen in residuals also.
### =============================================


###
## Analysis that includes all independent variables together
###
cor(select(h_df, attended_ideas_by_ideas, attended_comments_by_comments,
           irrelevant_ideas_by_ideas, tags_by_ideas, moderators_by_members,
           size_inter, contributors_by_members))

h_df = dplyr::select(communities, community_id, contributors_by_members,
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
fit = lm(contributors_by_members ~ attended_ideas_by_ideas+attended_comments_by_comments+
         irrelevant_ideas_by_ideas+tags_by_ideas+moderators_by_members, 
         h_df)
summary(fit)

summary(h_df$users)
aux = filter(h_df, users>=120)
fit = lm(contributors_by_members ~ attended_ideas_by_ideas+attended_comments_by_comments+
         irrelevant_ideas_by_ideas+tags_by_ideas+moderators_by_members, 
         aux)
summary(fit)

# Residual
plot(fit)

# Plot predited vs. actual
plot(fit$fitted,h_df$contributors_by_members,pch=19,
     col="grey",xlab="Predicted",ylab="Actual", 
     main="RQ1 - Linear Regression (Predicted vs. Actual)")
