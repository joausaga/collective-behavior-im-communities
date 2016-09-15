# Load libraries
library(dplyr)


get_real_community_lifetime = function(c_ideas, c_comments, c_votes) {  
  c_ideas = c_ideas %>% select(creation_datetime)
  c_comments = c_comments %>% select(creation_datetime)
  c_votes = c_votes %>% select(creation_datetime)
  
  c_content = rbind(c_ideas, c_comments)
  c_content = rbind(c_content, c_votes)
  c_content$creation_datetime = as.POSIXlt(c_content$creation_datetime)
  c_content$creation_datetime = sort(c_content$creation_datetime, decreasing=TRUE)
  lifetime = as.numeric(difftime(c_content[1,'creation_datetime'], 
                                 c_content[nrow(c_content),'creation_datetime'], units="days"))
  
  return (lifetime)
}


calculate_avg_community_lifetime = function(communities, ideas, comments, votes) {
  communities = all_communities
  ideas = all_ideas
  comments = all_comments
  votes = all_votes
  
  community_lts = c()
  for (c_id in communities$id) {
    community_ideas = filter(ideas, community_id==c_id)
    community_comments = filter(comments, community_id==c_id)
    community_votes = filter(votes, community_id==c_id)
    lt = get_real_community_lifetime(community_ideas, community_comments, community_votes)
    community_lts = rbind(community_lts, c(c_id,lt))  
  }
  
  colnames(community_lts) = c('id','lifetime_days')
  rownames(community_lts) = NULL
  write.csv(community_lts, "./data/idsc_communities_lifetime.csv", row.names=F)
}


create_moderator_intervention_ds = function(id_communities, users, ideas, comments, votes) {
  id_communities = ids_valid_communities
  users = authors
  ideas = all_ideas
  votes = all_votes
  comments = all_comments
  
  # Convert to boolean
  users$admin = as.logical(users$admin)
  users$moderator = as.logical(users$moderator)
  
  remove(mat_mod_inter)
  
  # Save idea submitted by moderators
  sub_ideas = filter(ideas, community_id %in% id_communities)
  for (i in 1:nrow(sub_ideas)) {
    idea = sub_ideas[i,]
    author_id = idea$author_id
    if (author_id > 0) {
      author = filter(users, id == author_id)
      if (author$admin || author$moderator) {
        if (exists('mat_mod_inter')) {
          mat_mod_inter = rbind(mat_mod_inter,list(idea$id,'idea',author_id,idea$creation_datetime,idea$community_id))
        } else {
          mat_mod_inter = data.frame(id=idea$id,type='idea',author_id=author_id,creation_datetime=idea$creation_datetime,community_id=idea$community_id)
        }
      }
    }  
  }
  write.csv(mat_mod_inter, "./data/moderator_interventions.csv", row.names=F)
  # Save comments submitted by moderators
  sub_comments = filter(comments, community_id %in% id_communities)
  for (i in 1:nrow(sub_comments)) {
    comment = sub_comments[i,]
    author_id = comment$author_id
    if (author_id > 0) {
      author = filter(users, id == author_id)
      if (author$admin || author$moderator) {
        if (exists('mat_mod_inter')) {
          mat_mod_inter = rbind(mat_mod_inter,list(comment$id,'comment',author_id,comment$creation_datetime,comment$community_id))
        } else {
          mat_mod_inter = data.frame(id=comment$id,type='comment',author_id=author_id,creation_datetime=comment$creation_datetime,community_id=comment$community_id)
        }
      }
    }  
  }
  if (!file.exists("./data/moderator_interventions.csv")) {
    write.csv(mat_mod_inter, "./data/moderator_interventions.csv", row.names=F)
  } else {
    write.table(mat_mod_inter, "./data/moderator_interventions.csv", sep=",", append=T, row.names=F, col.names=F)
  }
  # Save votes submitted by moderators
  sub_votes = filter(votes, community_id %in% id_communities)
  for (i in 1:nrow(sub_votes)) {
    vote = sub_votes[i,]
    author_id = vote$author
    if (author_id > 0) {
      author = filter(users, id == author_id)
      if (author$admin || author$moderator) {
        if (exists('mat_mod_inter')) {
          mat_mod_inter = rbind(mat_mod_inter,list(vote$id,'vote',author_id,vote$creation_datetime,vote$community_id))
        } else {
          mat_mod_inter = data.frame(id=vote$id,type='vote',author_id=author_id,creation_datetime=vote$creation_datetime,community_id=vote$community_id)
        }
      }
    }  
  }
  if (!file.exists("./data/moderator_interventions.csv")) {
    write.csv(mat_mod_inter, "./data/moderator_interventions.csv", row.names=F)
  } else {
    write.table(mat_mod_inter, "./data/moderator_interventions.csv", sep=",", append=T, row.names=F, col.names=F)
  }
}


create_main_data_frame = function(all_communities, all_ideas, all_votes, all_comments, all_inter,
                                  all_users, min_weeks) {
  community_ids = all_communities$id
  short_communities = 0
  
  for (c_id in community_ids) {
    users_community = filter(all_users, community_id==c_id)
    ideas_community = filter(all_ideas, community_id==c_id)
    votes_community = filter(all_votes, community_id==c_id)
    comments_community = filter(all_comments, community_id==c_id)
    inter_community = filter(all_inter, community_id==c_id)
    community_event_log = create_community_event_log(users_community, ideas_community, 
                                                     comments_community, votes_community,
                                                     inter_community)
    event_per_weeks = calculate_event_per_week_community_lf(community_event_log, 
                                                            'idea', FALSE)
    # Only consider communities that last at least min_weeks
    if (nrow(event_per_weeks) < min_weeks) {
      short_communities = short_communities + 1
    } else {
      if (exists('communities_event_log')) {
        communities_event_log = rbind(communities_event_log, community_event_log)
      } else {
        communities_event_log = community_event_log
      }
    }
  }
  
  print(paste(short_communities, ' communities were not included because they last less than ', min_weeks, ' weeks',sep=''))
  return(communities_event_log)
}


summarize_community_moderator_interventions = function(mod_interventions) {
  sum_mod = summarise(group_by(mod_interventions, community_id, type), count=n())
  communities_inter = c()
  
  for (c_id in unique(sum_mod$community_id)) {
    inter_community = filter(sum_mod, community_id==c_id)
    inter_idea = ifelse('idea' %in% inter_community$type,
                        inter_community[inter_community$type=='idea',][[3]],
                        0)
    inter_vote = ifelse('vote' %in% inter_community$type,
                        inter_community[inter_community$type=='vote',][[3]],
                        0)
    inter_comment = ifelse('comment' %in% inter_community$type,
                           inter_community[inter_community$type=='comment',][[3]],
                           0)
    communities_inter = rbind(communities_inter, c(c_id,inter_idea,inter_vote,inter_comment))
  }
  colnames(communities_inter) = c('id','inter_ideas','inter_votes','inter_comments')
  
  return (data.frame(communities_inter))
}


# Load datasets
all_communities = read.csv("./data/idsc_communities.csv", sep=",", header=T, skip=1) # the first row contains the observation date
all_ideas = read.csv("./data/idsc_ideas_no_text_fixed.csv", sep=",", header=T)
all_votes = read.csv("./data/idsc_votes.csv", sep=",", header=T, skip=1)
all_comments = read.csv("./data/idsc_comments_no_text_fixed.csv", sep=",", header=T)
communities_lf = read.csv("./data/idsc_communities_lifetime.csv", sep=",", header=T)
authors = read.csv("./data/idsc_authors_reloaded.csv", sep=",", header=T, skip=1)
colnames(authors)[which(names(authors) == "registered")] = "creation_datetime"
mod_interventions = read.csv("./data/moderator_interventions.csv", sep=",", header=T)
MIN_WEEKS = 4


# 1- Create data frame with the event logs of the communities that last more than MIN_WEEKS
communities_event_log = create_main_data_frame(all_communities, all_ideas, all_votes, 
                                               all_comments, mod_interventions,
                                               authors, MIN_WEEKS)
write.csv(communities_event_log, "./data/communities_event_log.csv", row.names=F)

# 2- Create moderator intervention data frame
communities_mod_interventions = summarize_community_moderator_interventions(mod_interventions)
write.csv(communities_mod_interventions, "./data/communities_mod_interventions.csv", row.names=F)

# 3- Merge moderator intervention data frame with the main data frame of communities 
communities_df = merge(all_communities, communities_mod_interventions, all=T)
communities_df[is.na(communities_df$inter_ideas),'inter_ideas'] = 0
communities_df[is.na(communities_df$inter_votes),'inter_votes'] = 0
communities_df[is.na(communities_df$inter_comments),'inter_comments'] = 0
communities_df = mutate(communities_df, inter_total=inter_ideas+inter_votes+inter_comments)
write.csv(communities_df, "./data/communities_dataset.csv", row.names=F)

# 4- Remove duplicates
communities_event_log = read.csv("./data/communities_event_log.csv", sep=",", header=T)
communities_event_log = communities_event_log[!duplicated(communities_event_log),]
write.csv(communities_event_log, "./data/communities_event_log_no_dup.csv", row.names=F)