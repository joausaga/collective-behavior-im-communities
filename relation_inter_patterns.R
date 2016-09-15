
compute_correlation = function(stats,clus_info){
  mean_clus = summarise(group_by(clus_info, cluster), 
                        mean_q1=mean(q1, na.rm=T),
                        mean_q2=mean(q2, na.rm=T),
                        mean_q3=mean(q3, na.rm=T),
                        mean_q4=mean(q4, na.rm=T))
  for (i in (1:5)) {
    for (j in (1:4)) {
      stats[stats$cluster==i&stats$quartile==j,'prop'] = mean_clus[i,j+1]
    }
  }
  colnames(stats) = c("cluster","quartile","inter","prop")
  cor.test(stats$inter, stats$prop)
}

# Load data
communities_event_log = read.csv("./data/communities_event_log_no_dup.csv", sep=",", header=T)
communities_df = read.csv("./data/final_communities.csv", sep=",", header=T)
tagged_communities = read.csv('data/tagged_communities_and_archetypes.csv', header=T)

# Select only tagged communities
communities_df = filter(communities_df, id %in% tagged_communities$id)
communities_event_log = filter(communities_event_log, community_id %in% tagged_communities$id)

# Analyze whether the proportion of ideation activity per quartile is related with
# intervention in the quartile
ideas_communities = compute_proportions_per_quantiles(communities_event_log, 'idea')
ideas_clus = data.frame(community_id=rownames(ideas_communities), 
                        q1=ideas_communities$q1, q2=ideas_communities$q2, 
                        q3=ideas_communities$q3, q4=ideas_communities$q4)
ideas_clus = merge(ideas_clus, tagged_communities[,c('id','idea_cluster')], 
                   by.x='community_id',by.y='id')
colnames(ideas_clus) = c('community_id','q1','q2','q3','q4','cluster')
table_stats = get_clusters_stats_per_quantiles(communities_event_log, ideas_clus)
table_stats = table_stats[, c(1, 2, 12)]
compute_correlation(table_stats, ideas_clus)

# Analyze whether the proportion of commenting activity per quartile is related with
# intervention in the quartile
comments_communities = compute_proportions_per_quantiles(communities_event_log, 'comment')  
comments_clus = data.frame(community_id=rownames(comments_communities), 
                           q1=comments_communities$q1, q2=comments_communities$q2, 
                           q3=comments_communities$q3, q4=comments_communities$q4)
comments_clus = merge(comments_clus, tagged_communities[,c('id','comment_cluster')], 
                      by.x='community_id',by.y='id')
colnames(comments_clus) = c('community_id','q1','q2','q3','q4','cluster')
table_stats = get_clusters_stats_per_quantiles(communities_event_log, comments_clus)
table_stats = table_stats[, c(1, 2, 12)]
compute_correlation(table_stats, comments_clus)

# Analyze whether the proportion of voting activity per quartile is related with
# intervention in the quartile
votes_communities = compute_proportions_per_quantiles(communities_event_log, 'vote')
votes_clus = data.frame(community_id=rownames(votes_communities), 
                        q1=votes_communities$q1, q2=votes_communities$q2, 
                        q3=votes_communities$q3, q4=votes_communities$q4)
votes_clus = merge(votes_clus, tagged_communities[,c('id','vote_cluster')], 
                   by.x='community_id',by.y='id')
colnames(votes_clus) = c('community_id','q1','q2','q3','q4','cluster')
table_stats = get_clusters_stats_per_quantiles(communities_event_log, votes_clus)
table_stats = table_stats[, c(1, 2, 12)]
compute_correlation(table_stats, votes_clus)
