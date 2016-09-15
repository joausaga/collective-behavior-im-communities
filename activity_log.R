# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(fpc)
library(cluster)
library(reshape)
library(GGally)
library(knitr)


main = function() {
  source('utils.R')
  
  ##
  ### Define constants
  ##
  TESTING_COMMUNITIES = c(13542, 24523, 2538, 2137, 22174, 25813, 10495, 34206, 8188, 6408, 8538,
                          18666, 28229, 21679, 10806)
  INCOMPLETE_COMMUNITIES = c(20036, 2780, 15287, 23001, 31589, 13493, 18116)
  UNAVAILABLE_COMMUNITIES = c(27159, 33161, 24472, 34451)
  SPAM_COMMUNITIES = c(27157, 24385)
  SPECIAL_OUTLIER_COMMUNITY = c(7100)
  
  ##
  ### Load datasets
  ##
  communities_event_log = read.csv("./data/communities_event_log_no_dup.csv", sep=",", header=T)
  communities_df = read.csv("./data/final_communities.csv", sep=",", header=T)
  communities_lt = read.csv("./data/idsc_communities_lifetime.csv", sep=",", header=T)
  tagged_communities = read.csv('data/tagged_communities_and_archetypes.csv', header=T)
  
  # Print out some histograms
  ggplot(tagged_communities, aes(ideas)) +  
    geom_histogram(col="black", fill="grey") +
    theme(text = element_text(size=20), 
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey"),
          panel.grid.minor.y = element_line(size = 0),
          panel.grid.major.x = element_line(size = 0),
          panel.grid.minor.x = element_line(size = 0))
  ggplot(tagged_communities, aes(users)) +  
    geom_histogram(col="black", fill="grey") +
    theme(text = element_text(size=20), 
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey"),
          panel.grid.minor.y = element_line(size = 0),
          panel.grid.major.x = element_line(size = 0),
          panel.grid.minor.x = element_line(size = 0))
  ggplot(tagged_communities, aes(comments)) +  
    geom_histogram(col="black", fill="grey") +
    theme(text = element_text(size=20), 
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey"),
          panel.grid.minor.y = element_line(size = 0),
          panel.grid.major.x = element_line(size = 0),
          panel.grid.minor.x = element_line(size = 0))
  ggplot(tagged_communities, aes(votes)) +  
    geom_histogram(col="black", fill="grey") +
    theme(text = element_text(size=20), 
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey"),
          panel.grid.minor.y = element_line(size = 0),
          panel.grid.major.x = element_line(size = 0),
          panel.grid.minor.x = element_line(size = 0))
  
  ##
  ### Polish and Prepare data
  ##
  # 0- Get rid of testing, incomplete, unavailable and spam communities
  communities_event_log = filter(communities_event_log, !community_id %in% TESTING_COMMUNITIES)
  communities_event_log = filter(communities_event_log, !community_id %in% INCOMPLETE_COMMUNITIES)
  communities_event_log = filter(communities_event_log, !community_id %in% UNAVAILABLE_COMMUNITIES)
  communities_event_log = filter(communities_event_log, !community_id %in% SPAM_COMMUNITIES)
  communities_event_log = filter(communities_event_log, !community_id %in% SPECIAL_OUTLIER_COMMUNITY)
  communities_df = filter(communities_df, !id %in% TESTING_COMMUNITIES)
  communities_df = filter(communities_df, !id %in% INCOMPLETE_COMMUNITIES)
  communities_df = filter(communities_df, !id %in% UNAVAILABLE_COMMUNITIES)
  communities_df = filter(communities_df, !id %in% SPAM_COMMUNITIES)
  communities_df = filter(communities_df, !id %in% SPECIAL_OUTLIER_COMMUNITY)
  communities_lt = filter(communities_lt, !id %in% TESTING_COMMUNITIES)
  communities_lt = filter(communities_lt, !id %in% INCOMPLETE_COMMUNITIES)
  communities_lt = filter(communities_lt, !id %in% UNAVAILABLE_COMMUNITIES)
  communities_lt = filter(communities_lt, !id %in% SPAM_COMMUNITIES)
  communities_lt = filter(communities_lt, !id %in% SPECIAL_OUTLIER_COMMUNITY)
  
  # 1- Select communities that lasted at least 1 year (365 days)
  selected_communities = filter(communities_lt, lifetime_days > 365)
  
  # 2- Filter out communities less than 1 year old
  communities_df = filter(communities_df, id %in% selected_communities$id)
  communities_event_log = filter(communities_event_log, community_id %in% selected_communities$id)
  
  # 3- Select only tagged communities
  communities_df = filter(communities_df, id %in% tagged_communities$id)
  communities_event_log = filter(communities_event_log, community_id %in% tagged_communities$id)
  
  # X- Double-check communities' lifetime
  #result = recalculate_communities_lifetime(communities_event_log)
  #new_communities_lt = result[[1]]
  
  # 4- Identify outliers (extreme number of user registered or production of 
  # ideas/votes/comments/moderator interventions)
  ggpairs(select(communities_df, ideas, users, contributors, votes, comments, 
                 inter_ideas, inter_comments, inter_votes, inter_total))
  # Set thresholds
  IDEA_THRESHOLD = 3000; USER_THRESHOLD = 4000; CONT_THRESHOLD = 3000; VOTE_THRESHOLD = 25000
  COMM_THRESHOLD = 5000; INTI_THRESHOLD = 250; INTV_THRESHOLD = 250; INTC_THRESHOLD = 300
  INTT_THRESHOLD = 500
  outliers = identify_outliers(communities_df, IDEA_THRESHOLD, USER_THRESHOLD, CONT_THRESHOLD,
                               VOTE_THRESHOLD, COMM_THRESHOLD, INTI_THRESHOLD, INTV_THRESHOLD,
                               INTC_THRESHOLD, INTT_THRESHOLD)
  
  # 4- Get rid of outliers
  #communities_el_no_out = filter(communities_event_log,!community_id %in% outliers$outliers)
  #communities_el_no_out = filter(communities_el_no_out,!community_id %in% outliers$int_outliers)
  #communities_df = filter(communities_df, !id %in% outliers$outliers)
  #communities_df = filter(communities_df, !id %in% outliers$int_outliers)
  # At the end decided not to exclude outliers
  communities_el_no_out = communities_event_log
  
  
  ##
  ### Extract the events of the communities that occurred during the first year of
  ### their life
  ##
  first_year_log = slice_communities_log_by_weeks(communities_el_no_out, 1, 52)

  
  ##
  ### Compute basic statistics of communities
  ##
  print(paste("Total communities:",nrow(communities_df)))
  print("General Summary Table (first year)")
  kable(compute_statistics(first_year_log))
  

  ##
  ### Analysis
  ##
  # 1- Compute user registration clusters
  users_reg_communities = compute_proportions_per_quantiles(communities_el_no_out, 'user')
  # K-Means Cluster Analysis
  fit = kmeans(users_reg_communities, 5)
  # Get Cluster Means
  aggregate(users_reg_communities, by=list(fit$cluster), FUN=mean) 
  # Append Cluster Assignment
  users_clus = data.frame(community_id=rownames(users_reg_communities), 
                          q1=users_reg_communities$q1, q2=users_reg_communities$q2, 
                          q3=users_reg_communities$q3, q4=users_reg_communities$q4, 
                          cluster=fit$cluster)
  rownames(users_clus) = NULL
  # Create an aggregated dataset
  aggregate_clus = select(users_clus, community_id,cluster)
  colnames(aggregate_clus) = c('community_id','user_reg_cluster')
  # Draw cluster plot
  clusplot(users_clus, fit$cluster, color=TRUE, shade=TRUE, lines=0)
  # Draw clusters
  users_clus$q0 = 0
  plot_clusters(users_clus, 'users')
  # Rename clusters (manually)
  #users_clus$new_cluster = -1
  #users_clus[users_clus$cluster==1,'new_cluster'] = 1
  #users_clus[users_clus$cluster==2,'new_cluster'] = 5
  #users_clus[users_clus$cluster==3,'new_cluster'] = 3
  #users_clus[users_clus$cluster==4,'new_cluster'] = 2
  #users_clus[users_clus$cluster==5,'new_cluster'] = 4
  #users_clus$cluster = users_clus$new_cluster
  #plot_clusters(users_clus, 'users')
  # get clusters' stats
  get_cluster_stats(communities_df, users_clus, communities_lt)
  # get stats of moderators' interventions
  get_mod_interventions(users_clus)
  # get stats by clusters
  table_stats = get_clusters_stats_per_quantiles(communities_el_no_out, users_clus)
  
  
  # 2- Compute idea generation clusters
  ideas_communities = compute_proportions_per_quantiles(communities_el_no_out, 'idea')
  # Filter out community 8686 because all its ideas were submitted by moderators/administrators
  ideas_communities = ideas_communities[!is.na(ideas_communities$q1),]
  # K-Means Cluster Analysis
  fit = kmeans(ideas_communities, 5)
  # Get cluster means 
  aggregate(ideas_communities, by=list(fit$cluster), FUN=mean)
  # Append cluster assignment
  ideas_clus = data.frame(community_id=rownames(ideas_communities), 
                          q1=ideas_communities$q1, q2=ideas_communities$q2, 
                          q3=ideas_communities$q3, q4=ideas_communities$q4, 
                          cluster=fit$cluster)
  rownames(ideas_clus) = NULL
  # Draw cluster plot
  clusplot(ideas_clus, fit$cluster, color=TRUE, shade=TRUE, lines=0)
  # Draw clusters
  ideas_clus$q0 = 0
  plot_clusters(ideas_clus, 'ideas')  
  # Rename clusters (manually)
  #ideas_clus$new_cluster = -1
  #ideas_clus[ideas_clus$cluster==1,'new_cluster'] = 1
  #ideas_clus[ideas_clus$cluster==2,'new_cluster'] = 4
  #ideas_clus[ideas_clus$cluster==3,'new_cluster'] = 3
  #ideas_clus[ideas_clus$cluster==4,'new_cluster'] = 2
  #ideas_clus[ideas_clus$cluster==5,'new_cluster'] = 5
  #ideas_clus$cluster = ideas_clus$new_cluster
  #plot_clusters(ideas_clus, 'ideas')
  # Aggregate clusters to the aggregated dataset
  aggregate_clus = merge(aggregate_clus, select(ideas_clus, community_id, cluster), 
                         by.x='community_id', by.y='community_id')
  colnames(aggregate_clus) = c('community_id','user_reg_cluster','idea_gen_cluster')
  
  # 3- Compute comment generation clusters
  comments_communities = compute_proportions_per_quantiles(communities_el_no_out, 'comment')  
  # Filter out communities that don't have comments submitted by participants
  comments_communities = comments_communities[!is.na(comments_communities$q1),]
  # K-Means Cluster Analysis
  fit = kmeans(comments_communities, 5)
  # Get cluster means 
  aggregate(comments_communities, by=list(fit$cluster), FUN=mean)
  # append cluster assignment
  comments_clus = data.frame(community_id=rownames(comments_communities), 
                             q1=comments_communities$q1, q2=comments_communities$q2, 
                             q3=comments_communities$q3, q4=comments_communities$q4, 
                             cluster=fit$cluster)
  rownames(comments_clus) = NULL
  # Draw cluster plot
  clusplot(comments_clus, fit$cluster, color=TRUE, shade=TRUE, lines=0)
  # Draw clusters
  comments_clus$q0 = 0
  plot_clusters(comments_clus, 'comments')
  # Rename clusters (manually)
  comments_clus$new_cluster = -1
  comments_clus[comments_clus$cluster==1,'new_cluster'] = 1
  comments_clus[comments_clus$cluster==2,'new_cluster'] = 4
  comments_clus[comments_clus$cluster==3,'new_cluster'] = 5
  comments_clus[comments_clus$cluster==4,'new_cluster'] = 2
  comments_clus[comments_clus$cluster==5,'new_cluster'] = 3
  comments_clus$cluster = comments_clus$new_cluster
  plot_clusters(comments_clus, 'comments')
  # Aggregate clusters to the aggregated dataset
  aggregate_clus = merge(aggregate_clus, select(comments_clus, community_id, cluster), 
                         by.x='community_id', by.y='community_id')
  colnames(aggregate_clus) = c('community_id','user_reg_cluster','idea_gen_cluster', 'comment_gen_cluster')
  
  # 4- Compute vote generation clusters
  votes_communities = compute_proportions_per_quantiles(communities_el_no_out, 'vote')
  # Filter out communities that don't have votes submitted by participants
  votes_communities = votes_communities[!is.na(votes_communities$q1),]
  # K-Means Cluster Analysis
  fit = kmeans(votes_communities, 5)
  # Get cluster means 
  aggregate(votes_communities, by=list(fit$cluster), FUN=mean)
  # append cluster assignment
  votes_clus = data.frame(community_id=rownames(votes_communities), 
                          q1=votes_communities$q1, q2=votes_communities$q2, 
                          q3=votes_communities$q3, q4=votes_communities$q4, 
                          cluster=fit$cluster)
  rownames(votes_clus) = NULL
  # Draw cluster plot
  clusplot(votes_clus, fit$cluster, color=TRUE, shade=TRUE, lines=0)
  # Draw clusters
  votes_clus$q0 = 0
  plot_clusters(votes_clus, 'votes')
  # Rename clusters (manually)
  votes_clus$new_cluster = -1
  votes_clus[votes_clus$cluster==1,'new_cluster'] = 4
  votes_clus[votes_clus$cluster==2,'new_cluster'] = 1
  votes_clus[votes_clus$cluster==3,'new_cluster'] = 3
  votes_clus[votes_clus$cluster==4,'new_cluster'] = 2
  votes_clus[votes_clus$cluster==5,'new_cluster'] = 5
  votes_clus$cluster = votes_clus$new_cluster
  plot_clusters(votes_clus, 'votes')
  # Aggregate clusters to the aggregated dataset
  aggregate_clus = merge(aggregate_clus, select(votes_clus, community_id, cluster), 
                         by.x='community_id', by.y='community_id')
  colnames(aggregate_clus) = c('community_id','user_cluster','idea_cluster', 'comment_cluster', 'vote_cluster')
  
  ##
  ### Save community clusters
  ##
  write.csv(aggregate_clus, "./data/derived/community_activity_pattern_clusters.csv", row.names=F)
  
  
  ##
  ### Analyze community behavior
  ##
  analytic_cluster = aggregate_clus
  analytic_cluster$type_beh = ''
  for (c_id in analytic_cluster$community_id) {
    community = analytic_cluster[analytic_cluster$community_id==c_id,]
    c_user_clus = community$user_cluster
    c_idea_clus = community$idea_cluster
    c_vote_clus = community$vote_cluster
    c_comm_clus = community$comment_cluster
    if (c_user_clus==c_idea_clus & c_idea_clus==c_comm_clus &
      c_comm_clus==c_vote_clus) {
      analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'uicv'
    } else {
      if (c_user_clus==c_idea_clus & c_idea_clus==c_comm_clus) {
        analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'uic'
      } else {
        if (c_user_clus==c_idea_clus & c_idea_clus==c_vote_clus) {
          analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'uiv'  
        } else {
          if (c_user_clus==c_comm_clus & c_comm_clus==c_vote_clus) {
            analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'ucv'  
          } else {
            if (c_idea_clus==c_comm_clus & c_comm_clus==c_vote_clus) {
              analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'icv'  
            } else {
              if (c_user_clus==c_idea_clus) {
                analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'ui'  
              } else {
                if (c_user_clus==c_comm_clus) {
                  analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'uc'  
                } else {
                  if (c_user_clus==c_vote_clus) {
                    analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'uv'  
                  } else {
                    if (c_idea_clus==c_comm_clus) {
                      analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'ic'  
                    } else {
                      if (c_idea_clus==c_vote_clus) {
                        analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'iv'  
                      } else {
                        if (c_comm_clus==c_vote_clus) {
                          analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'cv'  
                        } else {
                          analytic_cluster[analytic_cluster$community_id==c_id,'type_beh'] = 'alldiff'  
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  remove('m_type_beh')
  for (tb in unique(analytic_cluster$type_beh)) {
    tot = nrow(filter(analytic_cluster,type_beh==tb))
    per = round(tot/(nrow(analytic_cluster)) * 100,1)
    row = c(tb, tot, per)
    if (!exists('m_type_beh')) {
      m_type_beh = matrix(row,ncol=3)
    } else {
      m_type_beh = rbind(m_type_beh,row)  
    }
  }
  rownames(m_type_beh) = NULL
  colnames(m_type_beh) = c('type','tot','per')
  dt_type_beh = data.frame(m_type_beh)
  dt_type_beh$tot = as.numeric(as.character(dt_type_beh$tot))
  dt_type_beh$per = as.numeric(as.character(dt_type_beh$per))
  dt_type_beh = arrange(dt_type_beh, desc(tot))
  write.csv(dt_type_beh, "./data/derived/community_cluster_behaviors.csv", row.names=F)
}

main()

#Leasons form the manual inspections:
#Communities revived after the following events occured:
#a) A burst of new ideas were posted by the moderators and administrators
#b) New users showed off
#Exemplary communities
#GeoVation Challenge: http://challenge.geovation.org.uk
#52 communities were manually analyzed