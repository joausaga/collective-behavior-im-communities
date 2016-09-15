library(dplyr)

# Load dataset
communities = read.csv('data/tagged_communities_and_archetypes.csv', header=T)
mod_interventions = read.csv('data/communities_mod_interventions.csv', header=T)

# Selected columns for the analysis
communities = dplyr::select(communities, id, user_cluster, idea_cluster, vote_cluster, 
                            comment_cluster)

# Merge clusters with interventions
main_df = merge(communities, mod_interventions, by.x="id", by.y="id", all.x=T)

# Add column for total interventions
main_df = mutate(main_df, tot_inter=inter_ideas+inter_votes+inter_comments)

# Idea gen clusters group by inter
idea_clu_inter = summarise(group_by(main_df, idea_cluster), 
                           mean_inter=mean(tot_inter, na.rm=T),
                           mean_inter_ideas=mean(inter_ideas, na.rm=T),
                           mean_inter_comments=mean(inter_comments, na.rm=T),
                           mean_inter_votes=mean(inter_votes, na.rm=T))
idea_clu_inter %>% arrange(desc(mean_inter)) %>% dplyr::select(idea_cluster, mean_inter)
mean(filter(idea_clu_inter,idea_cluster==1|idea_cluster==3)$mean_inter)
mean(filter(idea_clu_inter,idea_cluster!=1&idea_cluster!=3)$mean_inter)
mean(filter(idea_clu_inter,idea_cluster==1|idea_cluster==3)$mean_inter)/mean(filter(idea_clu_inter,idea_cluster!=1&idea_cluster!=3)$mean_inter)

# Comment gen clusters group by inter
comment_clu_inter = summarise(group_by(main_df, comment_cluster), 
                              mean_inter=mean(tot_inter, na.rm=T),
                              mean_inter_ideas=mean(inter_ideas, na.rm=T),
                              mean_inter_comments=mean(inter_comments, na.rm=T),
                              mean_inter_votes=mean(inter_votes, na.rm=T))
comment_clu_inter %>% arrange(desc(mean_inter)) %>% dplyr::select(comment_cluster, mean_inter)

# Voten gen clusters group by inter
vote_clu_inter = summarise(group_by(main_df, vote_cluster), 
                           mean_inter=mean(tot_inter, na.rm=T),
                           mean_inter_ideas=mean(inter_ideas, na.rm=T),
                           mean_inter_comments=mean(inter_comments, na.rm=T),
                           mean_inter_votes=mean(inter_votes, na.rm=T))
vote_clu_inter %>% arrange(desc(mean_inter)) %>% dplyr::select(vote_cluster, mean_inter)

# Considering only communities that keep a constant activity along the time
active_communities = filter(main_df, idea_cluster==1&vote_cluster==1&
                            comment_cluster==1&!is.na(tot_inter))
mean(active_communities$tot_inter, na.rm=T)

rest_communities = filter(main_df, !id%in%active_communities$id&!is.na(tot_inter))
mean(rest_communities$tot_inter, na.rm=T)

t.test(active_communities$tot_inter,rest_communities$tot_inter)
