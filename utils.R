# Requirements: 
# Libraries: dplyr and ggplot2
# Dataset: must have columns called id and name
describe_var = function(dataset, var_name) {
  # Calculate 90% quantile
  quantile_90 = as.numeric(quantile(dataset[,var_name], na.rm=T, probs=c(0.95)))
  
  # Calculate inner and outer fence
  summary_var = summary(dataset[,var_name])
  first_q = summary_var[2]
  third_q = summary_var[5]
  iqr = third_q-first_q
  inner_fence_up = third_q + (iqr*1.5)
  inner_fence_down = first_q - (iqr*1.5)
  outer_fence_up = third_q + (iqr*3)
  outer_fence_down = first_q - (iqr*3)
  fences = c(inner_fence_up,outer_fence_up)
  
  # Get outliers (Thouse out of the 90% quantile)
  outliers = filter(dataset, dataset[,var_name] > outer_fence_up)
  colNums = match(c("id", "name", "url", var_name),names(outliers))
  outliers = outliers %>% select(colNums)
  outliers = outliers %>% arrange(outliers[,2])
  
  # Create dataset without outliers
  data_no_outliers = filter(dataset, dataset[,var_name] <= outer_fence_up)
  
  # Compute summary table
  summary = summary(data_no_outliers[,var_name])
  mean_i=summary[4]
  median_i=summary[3]
  max=summary[6]
  min=summary[1]
  sd=round(sd(data_no_outliers[,var_name]),3)
  first_q = summary[2]
  third_q = summary[5]
  summary_table = matrix(c(min,first_q,median_i,mean_i,third_q,max,sd),ncol=7,byrow=TRUE)
  summary_table = rbind(summary_table,c('','','','','','','')) #Add an extra empty row since 'kable' doesn't work with 1 row tables
  colnames(summary_table) = c('min','1st quartile','median','mean','3rd quartile','max','SD')
  
  ret = list(fences,outliers,summary_table,data_no_outliers)

  return (ret)
}

describe_var_theshold = function(dataset, var_name, outlier_threshold) {
  # Calculate inner and outer fence
  summary_var = summary(dataset[,var_name])
  first_q = summary_var[2]
  third_q = summary_var[5]
  iqr = third_q-first_q
  inner_fence_up = third_q + (iqr*1.5)
  inner_fence_down = first_q - (iqr*1.5)
  outer_fence_up = third_q + (iqr*3)
  outer_fence_down = first_q - (iqr*3)
  fences = c(inner_fence_up,outer_fence_up)
  
  # Get outliers (Thouse beyond the outlier_threshold)
  outliers = filter(dataset, dataset[,var_name] > outlier_threshold)
  colNums = match(c("id", "name", "url", var_name),names(outliers))
  outliers = outliers %>% select(colNums)
  outliers = outliers %>% arrange(outliers[,2])
  
  # Create dataset without outliers
  data_no_outliers = filter(dataset, dataset[,var_name] <= outlier_threshold)
  
  # Compute summary table
  summary = summary(data_no_outliers[,var_name])
  mean_i=summary[4]
  median_i=summary[3]
  max=summary[6]
  min=summary[1]
  sd=round(sd(data_no_outliers[,var_name]),3)
  first_q = summary[2]
  third_q = summary[5]
  summary_table = matrix(c(min,first_q,median_i,mean_i,third_q,max,sd),ncol=7,byrow=TRUE)
  summary_table = rbind(summary_table,c('','','','','','','')) #Add an extra empty row since 'kable' doesn't work with 1 row tables
  colnames(summary_table) = c('min','1st quartile','median','mean','3rd quartile','max','SD')
  
  ret = list(fences,outliers,summary_table,data_no_outliers,outlier_threshold)
  
  return (ret)
}

format_outliers = function(outliers, pos, mat_outliers) {
  for (i in 1:nrow(outliers)) {
    name = as.character(outliers[i,"name"])
    row = c(name,'','','','','','','','','','','')
    row[pos] = 'x'
    
    if (ncol(mat_outliers) == 1) { 
      mat_outliers = matrix(row, ncol=12, byrow=TRUE)
    } else {
      if (name%in%mat_outliers[,1]) {
        idx = which((name==mat_outliers[,1])==TRUE)
        mat_outliers[idx, pos] = 'x'
      } else {
        mat_outliers = rbind(mat_outliers, row)
      }
    }
  }
  
  return (mat_outliers)
}

get_admin_mod_interventions = function(community_id, ideas, votes, comments, authors) {
  authors_ideas_community = ideas[ideas$community_id==community_id,c("id","author_id","status")]
  authors_votes_community = votes[votes$community_id==community_id,"author"]
  authors_comments_community = comments[comments$community_id==community_id,"author_id"]
  authors_community = c(authors_ideas_community$author_id, authors_votes_community, authors_comments_community)
  authors_community = unique(authors_community)
  counter_mod_ideators = 0
  counter_mod_voters = 0
  counter_mod_commenters = 0
  counter_admin_ideators = 0
  counter_admin_voters = 0
  counter_admin_commenters = 0
  counter_mod_ideas_complete = 0
  counter_mod_ideas_progress = 0
  counter_mod_ideas_review = 0
  counter_admin_ideas_complete = 0
  counter_admin_ideas_progress = 0
  counter_admin_ideas_review = 0
  for (author_id in authors_community) {
    is_mod = authors[authors$id==author_id,"moderator"]
    is_admin = authors[authors$id==author_id,"admin"]
    if (length(is_admin) || length(is_mod)) {
      idxs_ideas = which((author_id==authors_ideas_community$author_id)==TRUE)
      idxs_votes = which((author_id==authors_votes_community)==TRUE)
      idxs_comments = which((author_id==authors_comments_community)==TRUE)
      if (is_admin == 1) {
        counter_admin_ideators = counter_mod_ideators + length(idxs_ideas)
        counter_admin_voters = counter_mod_voters + length(idxs_votes)
        counter_admin_commenters = counter_mod_commenters + length(idxs_comments) 
        if (length(idxs_ideas) > 0) {
          idxs_ideas_complete = which(("complete"==authors_ideas_community[idxs_ideas,"status"])==TRUE)
          idxs_ideas_in_progress = which(("progress"==authors_ideas_community[idxs_ideas,"status"])==TRUE)
          idxs_ideas_in_review = which(("review"==authors_ideas_community[idxs_ideas,"status"])==TRUE)
          counter_admin_ideas_complete = counter_admin_ideas_complete + length(idxs_ideas_complete)
          counter_admin_ideas_progress = counter_admin_ideas_progress + length(idxs_ideas_in_progress)
          counter_admin_ideas_review = counter_admin_ideas_review + length(idxs_ideas_in_review)
        }
      } else {
        if (is_mod == 1) {
          counter_mod_ideators = counter_mod_ideators + length(idxs_ideas)
          counter_mod_voters = counter_mod_voters + length(idxs_votes)
          counter_mod_commenters = counter_mod_commenters + length(idxs_comments)    
          if (length(idxs_ideas) > 0) {
            idxs_ideas_complete = which(("complete"==authors_ideas_community[idxs_ideas,"status"])==TRUE)
            idxs_ideas_in_progress = which(("progress"==authors_ideas_community[idxs_ideas,"status"])==TRUE)
            idxs_ideas_in_review = which(("review"==authors_ideas_community[idxs_ideas,"status"])==TRUE)
            counter_mod_ideas_complete = counter_mod_ideas_complete + length(idxs_ideas_complete)
            counter_mod_ideas_progress = counter_mod_ideas_progress + length(idxs_ideas_in_progress)
            counter_mod_ideas_review = counter_mod_ideas_review + length(idxs_ideas_in_review)
          }
        }
      }
    }
  }
  
  ret = list(counter_mod_ideators,counter_mod_voters,counter_mod_commenters,
             counter_admin_ideators,counter_admin_voters,counter_admin_commenters,
             counter_admin_ideas_complete, counter_admin_ideas_progress, counter_admin_ideas_review,
             counter_mod_ideas_complete, counter_mod_ideas_progress, counter_mod_ideas_review)
  
  return (ret)
}

calculate_derived_metrics = function(communities, ideas, votes, comments, authors) {
  # Participation
  communities = mutate(communities, participation=contributors/users)
  # Productivity
  communities = mutate(communities, productivity=ideas/users)
  community_ids = unique(communities$id)
  # Moderator and Admin Interventations
  for (id in as.numeric(community_ids)) {
    interventions = get_admin_mod_interventions(id,ideas,votes,comments,authors)
    communities$ideas_by_moderators[communities$id==id] = interventions[[1]]
    communities$votes_by_moderators[communities$id==id] = interventions[[2]]
    communities$comments_by_moderators[communities$id==id] = interventions[[3]]
    communities$ideas_by_admins[communities$id==id] = interventions[[4]]
    communities$votes_by_admins[communities$id==id] = interventions[[5]]
    communities$comments_by_admins[communities$id==id] = interventions[[6]]
    communities$ideas_in_review_by_admins[communities$id==id] = interventions[[9]]
    communities$ideas_in_progress_by_admins[communities$id==id] = interventions[[8]]
    communities$ideas_complete_by_admins[communities$id==id] = interventions[[7]]
    communities$ideas_in_review_by_moderators[communities$id==id] = interventions[[12]]
    communities$ideas_in_progress_by_moderators[communities$id==id] = interventions[[11]]
    communities$ideas_complete_by_moderators[communities$id==id] = interventions[[10]]
  }
  communities$ideas_by_moderators = as.numeric(communities$ideas_by_moderators)
  communities$votes_by_moderators = as.numeric(communities$votes_by_moderators)
  communities$comments_by_moderators = as.numeric(communities$comments_by_moderators)
  communities$ideas_by_admins = as.numeric(communities$ideas_by_admins)
  communities$votes_by_admins = as.numeric(communities$votes_by_admins)
  communities$comments_by_admins = as.numeric(communities$comments_by_admins)
  communities$ideas_in_review_by_admins = as.numeric(communities$ideas_in_review_by_admins)
  communities$ideas_in_progress_by_admins = as.numeric(communities$ideas_in_progress_by_admins)
  communities$ideas_complete_by_admins = as.numeric(communities$ideas_complete_by_admins)
  communities$ideas_in_review_by_moderators = as.numeric(communities$ideas_in_review_by_moderators)
  communities$ideas_in_progress_by_moderators = as.numeric(communities$ideas_in_progress_by_moderators)
  communities$ideas_complete_by_moderators = as.numeric(communities$ideas_complete_by_moderators)
  write.csv(communities, "./data/communities_preprocessed.csv", row.names=F)
  
  return (communities)
}

get_content_authors_distribution = function(data, var1, var2, var3) {
  for (i in 1:nrow(data)) {
    participant_content = data[i,var1]-(data[i,var2]+data[i,var3])
    if (participant_content < 0) {
      participant_content = data[i,var2]+data[i,var3]
    }
    if (exists("content_by_authors")) {
      content_by_authors = rbind(content_by_authors,c(as.character(data[i,"name"]),"participants",participant_content))
    } else {    
      content_by_authors = matrix(c(as.character(data[i,"name"]),"participants",participant_content), ncol=3, byrow=T)
    } 
    content_by_authors = rbind(content_by_authors,c(as.character(data[i,"name"]),"moderators",data[i,var2]))
    content_by_authors = rbind(content_by_authors,c(as.character(data[i,"name"]),"admins",data[i,var3]))
  }
  data_f = data.frame(name=content_by_authors[,1],option=content_by_authors[,2],quantity=as.numeric(content_by_authors[,3]))
  return (data_f)
}


check_normality = function(vec) {
  res = sf.test(vec)
  return (res$p.value > 0.05)
}

remove_few_representative_groups = function(dataset, groups){
  groups_to_remove = c()
  for (group in groups) {
    if (nrow(dataset[dataset$group==group,]) <= 2) {
      groups_to_remove = c(groups_to_remove,as.character(group))
    }  
  }
  if (!is.null(groups_to_remove)) {
    new_ds = filter(dataset,!group%in%groups_to_remove)
    groups = groups[-which(groups%in%groups_to_remove)] 
  } else {
    new_ds = dataset
  }
  ret = list(new_ds,groups)
  
  return (ret)
}

analyze_content_production = function(dataset, type_content, groups) {
  content_no_outliers = dataset
  max = -1
  means = c()
  medians = c()
  ns = c()
  for (idx in 1:length(groups)) {
    ns = c(ns, length(content_no_outliers[content_no_outliers$group==groups[idx],type_content]))
    median = median(content_no_outliers[content_no_outliers$group==groups[idx],type_content])
    mean = mean(content_no_outliers[content_no_outliers$group==groups[idx],type_content])
    if (!is.na(mean) & !is.na(median)) {
      if (mean > max) { max=mean}  
      medians = c(medians, round(as.numeric(median),3))
      means = c(means, round(as.numeric(mean),3))
    }
    else {
      if (is.na(mean)) { means = c(means, 0) }
      if (is.na(median)) {medians = c(medians, 0)}
    }
  }
  
  max = max(medians)
  if (max == 0) {
    max = max(means)
    idx_max = match(max,means)
  }
  else {
    idx_max = match(max,medians)
  }
  max_group = groups[idx_max]
  
  
  ret = remove_few_representative_groups(content_no_outliers,groups)
  content_no_outliers = ret[[1]]
  groups = ret[[2]]
  data_normal = check_normality(content_no_outliers[,type_content])
  
  if (data_normal) {
    res = oneway.test(content ~ group, data=content_no_outliers)  
    type_test = "oneway"
  }
  else {
    data = list()
    for (group in groups) {
      data[[group]] = content_no_outliers[content_no_outliers$group==group,type_content]
    }
    res = kruskal.test(data)
    type_test = "kruskal"
  }
  
  ret = list(max=max_group,p_value=res$p.value,test=type_test,means=means,medians=medians,ns=ns,
             test_result=res)
  
  return (ret)
}

# Compute the proportion of the activity for each quantile
get_proportions_per_quantiles = function(activity_per_week, type) {
  # Split community's lifecycle in FOUR quartiles
  first_q = round(as.numeric(quantile(activity_per_week$week)[2]),0)
  second_q = round(as.numeric(quantile(activity_per_week$week)[3]),0)
  third_q = round(as.numeric(quantile(activity_per_week$week)[4]),0)
  fourth_q = round(as.numeric(quantile(activity_per_week$week)[5]),0)
  tot = sum(activity_per_week[,type])
  
  first_q_avg = round(sum(activity_per_week[activity_per_week$week<=first_q,type])/tot,2)
  second_q_avg = round(sum(activity_per_week[activity_per_week$week>first_q&
                                               activity_per_week$week<=second_q,type])/tot,2)
  third_q_avg = round(sum(activity_per_week[activity_per_week$week>second_q&
                                              activity_per_week$week<=third_q,type])/tot,2)
  fourth_q_avg = round(sum(activity_per_week[activity_per_week$week>third_q&
                                               activity_per_week$week<=fourth_q,type])/tot,2)
  
  return (c(first_q_avg, second_q_avg, third_q_avg, fourth_q_avg))
}


get_productivity_per_quantiles = function(activity_per_week, users_per_week, type) {  
  # Split community lifecycle in FOUR quartiles
  first_q = round(as.numeric(quantile(activity_per_week$week)[2]),0)
  second_q = round(as.numeric(quantile(activity_per_week$week)[3]),0)
  third_q = round(as.numeric(quantile(activity_per_week$week)[4]),0)
  fourth_q = round(as.numeric(quantile(activity_per_week$week)[5]),0)
  
  # Compute the proportion of the activity for each quantile
  if (sum(users_per_week[users_per_week$week<=first_q,'user']) > 0 ) {
    first_q_norm_tot = round(sum(activity_per_week[activity_per_week$week<=first_q,type])/sum(users_per_week[users_per_week$week<=first_q,'user']),2)  
  } else {
    first_q_norm_tot = 0
  }
  if (sum(users_per_week[users_per_week$week>first_q&users_per_week$week<=second_q,'user']) > 0) {
    second_q_norm_tot = round(sum(activity_per_week[activity_per_week$week>first_q&activity_per_week$week<=second_q,type])/sum(users_per_week[users_per_week$week>first_q&users_per_week$week<=second_q,'user']),2)  
  } else {
    second_q_norm_tot = 0
  }
  if (sum(users_per_week[users_per_week$week>second_q&users_per_week$week<=third_q,'user']) > 0) {
    third_q_norm_tot = round(sum(activity_per_week[activity_per_week$week>second_q&activity_per_week$week<=third_q,type])/sum(users_per_week[users_per_week$week>second_q&users_per_week$week<=third_q,'user']),2)  
  } else {
    third_q_norm_tot = 0
  }
  if (sum(users_per_week[users_per_week$week>third_q&users_per_week$week<=fourth_q,'user']) > 0) {
    fourth_q_norm_tot = round(sum(activity_per_week[activity_per_week$week>third_q&activity_per_week$week<=fourth_q,type])/sum(users_per_week[users_per_week$week>third_q&users_per_week$week<=fourth_q,'user']),2)  
  } else {
    fourth_q_norm_tot = 0
  }
  
  return (c(first_q_norm_tot, second_q_norm_tot, third_q_norm_tot, fourth_q_norm_tot))
}


get_total_quantiles = function(activity_per_week, type) {
  # Split community's lifecycle in FOUR quartiles
  first_q = round(as.numeric(quantile(activity_per_week$week)[2]),0)
  second_q = round(as.numeric(quantile(activity_per_week$week)[3]),0)
  third_q = round(as.numeric(quantile(activity_per_week$week)[4]),0)
  fourth_q = round(as.numeric(quantile(activity_per_week$week)[5]),0)
  
  # Compute the proportion of the activity for each quantile
  first_q_users = round(sum(activity_per_week[activity_per_week$week<=first_q,type]),2)
  second_q_users = round(sum(activity_per_week[activity_per_week$week>first_q&
                                                 activity_per_week$week<=second_q,type]),2)
  third_q_users = round(sum(activity_per_week[activity_per_week$week>second_q&
                                                activity_per_week$week<=third_q,type]),2)
  fourth_q_users = round(sum(activity_per_week[activity_per_week$week>third_q&
                                                 activity_per_week$week<=fourth_q,type]),2)
  
  return (c(first_q_users, second_q_users, third_q_users, fourth_q_users))
}


calculate_activity_per_week_vector = function(activity, type) {
  activity = filter(activity, creation_datetime!='')
  activity = arrange(activity, creation_datetime)
  activity$week=as.numeric(round(difftime(activity[,'creation_datetime'], 
                                          activity[1,'creation_datetime'], 
                                          units = "weeks"),0)) + 1
  
  activity_by_w = group_by(activity, week)
  if (type == "idea") {
    sum_activity = summarise(activity_by_w, idea=n())  
  }
  if (type == "comment") {
    sum_activity = summarise(activity_by_w, comment=n())  
  }
  if (type == "vote") {
    sum_activity = summarise(activity_by_w, vote=n())  
  }
  if (type == "user") {
    sum_activity = summarise(activity_by_w, user=n())  
  }
  if (type == "inter") {
    sum_activity = summarise(activity_by_w, inter=n())  
  }
  if (type == "cont") {
    sum_activity = summarise(activity_by_w, cont=n())  
  }
  
  activity_per_week = cbind(as.numeric(sum_activity[1,'week']),as.numeric(sum_activity[1,type]))
  week_counter = 2
  if (nrow(sum_activity) >= 2) {
    for (i in 2:nrow(sum_activity)) {
      if (as.numeric(sum_activity[i,'week']) == week_counter) {
        activity_per_week = rbind(activity_per_week, c(week_counter, as.numeric(sum_activity[i,type])))      
        week_counter = week_counter + 1
      } else {
        for (j in week_counter:(as.numeric(sum_activity[i,'week'])-1)) {
          activity_per_week = rbind(activity_per_week, c(j,0))      
        }
        activity_per_week = rbind(activity_per_week, c(as.numeric(sum_activity[i,'week']),as.numeric(sum_activity[i,type])))      
        week_counter = as.numeric(sum_activity[i,'week']) + 1
      }
    } 
  }
  rownames(activity_per_week) = NULL
  colnames(activity_per_week) = c('week',type)
  
  return (data.frame(activity_per_week))
}

# Calculate the number of event occurances on each week of the community's lifetime.
# The parameter 'inc' determines whether the calculation should be incremental or 
# specific for the week.
calculate_event_per_week_community_lf = function(community_events, type, inc) {
  community_events = community_events[!duplicated(community_events),]  # Just in case, remove duplicates
  community_events = filter(community_events, creation_datetime!='')
  community_events = arrange(community_events, creation_datetime)
  community_events$creation_datetime = as.Date(as.character(community_events$creation_datetime))
  community_events$week=as.numeric(round(difftime(community_events[,'creation_datetime'], 
                                                  community_events[1,'creation_datetime'], 
                                                  units = "weeks"),0)) + 1  
  event_by_w = group_by(community_events, week, type_event)
  sum_event = summarise(event_by_w, total=n())
  total = 0
  if (sum_event[1,'type_event']==type) {
    total = total +  as.numeric(sum_event[1,'total'])
    event_per_week = cbind(1, total)    
  } else {
    if (type=='inter') {
      # Check if the type of the event contains the word 'inter'
      if (grepl(type,sum_event[1,'type_event'])) {
        total = total +  as.numeric(sum_event[1,'total'])
        event_per_week = cbind(1, total)
      } else {
        event_per_week = cbind(1,0)   
      }
    } else {
      event_per_week = cbind(1,0)   
    }
  }
  for (i in 2:as.numeric(sum_event[nrow(sum_event),'week'])) {
    if (i %in% sum_event[[1]]) {
      if (TRUE %in% grepl(type,filter(sum_event,week==i)[[2]])) {
        if (type!='inter') {
          row = filter(sum_event, week==i&type_event==type)  
        } else {
          # Check if the type of the event contains the word 'inter'
          row = filter(sum_event, week==i&grepl(type,type_event))  
        }
        if (inc == TRUE) {
          total = total + sum(row[[3]])
          event_per_week = rbind(event_per_week, c(i,total))  
        } else {
          event_per_week = rbind(event_per_week, c(i,sum(row[[3]])))
        }
      } else {
        event_per_week = rbind(event_per_week, c(i,0))
      }
    } else {
      event_per_week = rbind(event_per_week, c(i,0))          
    }
  }
  rownames(event_per_week) = NULL
  colnames(event_per_week) = c('week',type)
  
  return (data.frame(event_per_week))
}

calculate_contributors_per_week_community_lf = function(community_events) {
  community_events = community_events[!duplicated(community_events),]  # Just in case, remove duplicates
  community_events = filter(community_events, creation_datetime!='')
  community_events = arrange(community_events, creation_datetime)
  community_events$creation_datetime = as.Date(as.character(community_events$creation_datetime))
  community_events$week=as.numeric(round(difftime(community_events[,'creation_datetime'], 
                                                  community_events[1,'creation_datetime'], 
                                                  units = "weeks"),0)) + 1
  # The registration of users are not counted as contributions
  community_events = filter(community_events, type_event!='user')
  # The intervention of moderators and adminitrators are not counted as contributions
  community_events = filter(community_events, !grepl('inter',type_event))
  event_by_w = group_by(community_events, week, type_event, author_id)
  sum_event = summarise(event_by_w, total=n())
  # Remove duplicates
  aux_sum_event = NULL
  for (c_week in unique(sum_event$week)) {
    subset_event = filter(sum_event, c_week==week)
    aux_sum_event = rbind(aux_sum_event, subset_event[!duplicated(subset_event[,'author_id']),])   
  }
  sum_event = aux_sum_event
  for (i in 1:as.numeric(sum_event[nrow(sum_event),'week'])) {
    if (i %in% sum_event[[1]]) {
      if (exists('event_per_week')) {
        event_per_week = rbind(event_per_week, cbind(i,nrow(sum_event[sum_event$week==i,])))  
      } else {
        event_per_week = cbind(i,1)  
      }
    } else {
      if (exists('event_per_week')) {
        event_per_week = rbind(event_per_week, c(i, 0))          
      } else {
        event_per_week = cbind(i,0)
      }
    }
  }
  
  rownames(event_per_week) = NULL
  colnames(event_per_week) = c('week','contri')
  
  return(data.frame(event_per_week))
}


# Compute per each community their productivity (i.e., ideas/reg_users, comments/reg_users, 
# votes/reg_users) along their lifetime (divided in quantiles for the analysis)
compute_productivity_per_quantiles = function(communities_event_log, type) {
  vecs = c()
  ok_ids = c()
  
  for (c_id in unique(communities_event_log$community_id)) {
    community_event_log = filter(communities_event_log, community_id==c_id)
    activity_per_weeks = calculate_event_per_week_community_lf(community_event_log, type, FALSE)
    users_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'user', FALSE)
    vec_activity_community = get_productivity_per_quantiles(activity_per_weeks, type, 
                                                            users_per_weeks)
    vecs = rbind(vecs, vec_activity_community)
    ok_ids = rbind(ok_ids,c_id)
  }
  
  all_vectors = data.frame(q1=vecs[,1],q2=vecs[,2],q3=vecs[,3],q4=vecs[,4],row.names=ok_ids)
  return (all_vectors)
}


# Compute per each community the number of events ocurrance in ther quantiles
compute_proportions_per_quantiles = function(communities_event_log, type) {
  vecs = c()
  ok_ids = c()
  
  for (c_id in unique(communities_event_log$community_id)) {
    community_event_log = filter(communities_event_log, community_id==c_id)
    activity_per_weeks = calculate_event_per_week_community_lf(community_event_log, type, FALSE)
    vec_activity_community = get_proportions_per_quantiles(activity_per_weeks, type)
    vecs = rbind(vecs, vec_activity_community)
    ok_ids = rbind(ok_ids,c_id)
  }
  
  all_vectors = data.frame(q1=vecs[,1],q2=vecs[,2],q3=vecs[,3],q4=vecs[,4],row.names=ok_ids)
  return (all_vectors)
}


create_community_event_log = function(users_community, ideas_community, comments_community, 
                                      votes_community, inter_community) {
  users_community$creation_datetime = as.character(as.POSIXlt(users_community$creation_datetime, 
                                                              format="%Y-%m-%dT%H:%M:%S"))
  colnames(votes_community)[which(names(votes_community) == "author")] = "author_id"
  community_event_log = users_community %>% 
    select(id, creation_datetime, community_id) %>% 
    filter(creation_datetime!='') %>%
    mutate(type_event='user', author_id=id)
  community_event_log = rbind(community_event_log, 
                              ideas_community %>% 
                                mutate(type_event='idea') %>%
                                select(id, creation_datetime, community_id, type_event, author_id))
  community_event_log = rbind(community_event_log, 
                              votes_community %>% 
                                mutate(type_event='vote') %>%
                                select(id, creation_datetime, community_id, type_event, author_id))
  community_event_log = rbind(community_event_log, 
                              comments_community %>% 
                                mutate(type_event='comment') %>%
                                select(id, creation_datetime, community_id, type_event, author_id))
  if (nrow(inter_community) > 0) {
    for (row in 1:nrow(inter_community)) {
      community_event_log[community_event_log$id==inter_community[row,'id'],'type_event'] = 
        paste('inter_',inter_community[row,'type'],sep='')  
    }  
  }
  
  return (community_event_log)
}


compute_activity_vectors_quantile = function(communities_event_log, clusters) {
  comm_qua_act = data.frame(community_id=clusters$community_id,
                            cluster=0, users_q1=0,
                            users_q2=0, users_q3=0,
                            users_q4=0, ideas_q1=0, 
                            ideas_q2=0, ideas_q3=0,
                            ideas_q4=0, comments_q1=0,
                            comments_q2=0, comments_q3=0,
                            comments_q4=0, votes_q1=0,
                            votes_q2=0, votes_q3=0,
                            votes_q4=0, inter_q1=0,
                            inter_q2=0, inter_q3=0,
                            inter_q4=0, inter_ideas_q1=0,
                            inter_ideas_q2=0, inter_ideas_q3=0,
                            inter_ideas_q4=0, inter_comments_q1=0,
                            inter_comments_q2=0, inter_comments_q3=0,
                            inter_comments_q4=0, inter_votes_q1=0,
                            inter_votes_q2=0, inter_votes_q3=0,
                            inter_votes_q4=0, contri_q1=0,
                            contri_q2=0, contri_q3=0,
                            contri_q4=0, prod_ideas_q1=0,
                            prod_ideas_q2=0, prod_ideas_q3=0,
                            prod_ideas_q4=0, prod_comments_q1=0, 
                            prod_comments_q2=0, prod_comments_q3=0, 
                            prod_comments_q4=0, prod_votes_q1=0, 
                            prod_votes_q2=0, prod_votes_q3=0, 
                            prod_votes_q4=0, parti_q1=0, 
                            parti_q2=0, parti_q3=0, parti_q4=0)
  for (clus_id in unique(clusters$cluster)) {
    print(paste('Cluster:',clus_id))
    ids = clusters[clusters$cluster==clus_id, 'community_id']
    for (c_id in ids) {
      print(paste('Community Id:',c_id))
      comm_qua_act[comm_qua_act$community_id==c_id,'cluster'] = clus_id
      community_event_log = filter(communities_event_log, community_id==c_id)
      # Users registration
      users_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'user', FALSE)
      users_qs = get_total_quantiles(users_per_weeks, 'user')
      comm_qua_act[comm_qua_act$community_id==c_id,'users_q1'] = users_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'users_q2'] = users_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'users_q3'] = users_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'users_q4'] = users_qs[4]
      # Ideas
      ideas_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'idea', FALSE)
      ideas_qs = get_total_quantiles(ideas_per_weeks, 'idea')
      comm_qua_act[comm_qua_act$community_id==c_id,'ideas_q1'] = ideas_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'ideas_q2'] = ideas_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'ideas_q3'] = ideas_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'ideas_q4'] = ideas_qs[4]
      # Votes
      votes_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'vote', FALSE)
      votes_qs = get_total_quantiles(votes_per_weeks, 'vote')
      comm_qua_act[comm_qua_act$community_id==c_id,'votes_q1'] = votes_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'votes_q2'] = votes_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'votes_q3'] = votes_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'votes_q4'] = votes_qs[4]
      # Comments
      comments_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'comment', FALSE)
      comments_qs = get_total_quantiles(comments_per_weeks, 'comment')
      comm_qua_act[comm_qua_act$community_id==c_id,'comments_q1'] = comments_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'comments_q2'] = comments_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'comments_q3'] = comments_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'comments_q4'] = comments_qs[4]
      # Comments posted by moderators and administrators
      if (nrow(filter(community_event_log,type_event=='inter_comment'))>0) {
        inter_comments_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'inter_comment', FALSE)
        comments_inter_qs = get_total_quantiles(inter_comments_per_weeks, 'inter_comment')
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_comments_q1'] = comments_inter_qs[1]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_comments_q2'] = comments_inter_qs[2]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_comments_q3'] = comments_inter_qs[3]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_comments_q4'] = comments_inter_qs[4]
      }
      # Votes posted by moderators and administrators
      if (nrow(filter(community_event_log, type_event=='inter_vote'))>0) {
        inter_votes_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'inter_vote', FALSE)
        votes_inter_qs = get_total_quantiles(inter_votes_per_weeks, 'inter_vote')
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_votes_q1'] = votes_inter_qs[1]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_votes_q2'] = votes_inter_qs[2]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_votes_q3'] = votes_inter_qs[3]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_votes_q4'] = votes_inter_qs[4]  
      }
      # Ideas posted by moderators and administrators
      if (nrow(filter(community_event_log, type_event=='inter_idea'))>0) {
        inter_ideas_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'inter_idea', FALSE)
        ideas_inter_qs = get_total_quantiles(inter_ideas_per_weeks, 'inter_idea')
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_ideas_q1'] = ideas_inter_qs[1]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_ideas_q2'] = ideas_inter_qs[2]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_ideas_q3'] = ideas_inter_qs[3]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_ideas_q4'] = ideas_inter_qs[4]
      }
      # Total moderators and administrators' interventions
      if (nrow(filter(community_event_log, grepl('inter',type_event)))>0) {
        tot_inter_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'inter', FALSE)
        tot_inter_qs = get_total_quantiles(tot_inter_per_weeks, 'inter')
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_q1'] = tot_inter_qs[1]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_q2'] = tot_inter_qs[2]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_q3'] = tot_inter_qs[3]
        comm_qua_act[comm_qua_act$community_id==c_id,'inter_q4'] = tot_inter_qs[4]
      }
      # Community contributors
      cont_per_weeks = calculate_contributors_per_week_community_lf(community_event_log)
      cont_qs = get_total_quantiles(cont_per_weeks, 'contri')
      comm_qua_act[comm_qua_act$community_id==c_id,'contri_q1'] = cont_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'contri_q2'] = cont_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'contri_q3'] = cont_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'contri_q4'] = cont_qs[4]
      # Productivity of ideas
      inc_users_per_weeks = calculate_event_per_week_community_lf(community_event_log, 'user', TRUE)
      prod_ideas_qs = get_productivity_per_quantiles(ideas_per_weeks, inc_users_per_weeks, 'idea')
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_ideas_q1'] = prod_ideas_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_ideas_q2'] = prod_ideas_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_ideas_q3'] = prod_ideas_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_ideas_q4'] = prod_ideas_qs[4]
      # Productivity of comments
      prod_comments_qs = get_productivity_per_quantiles(comments_per_weeks, inc_users_per_weeks, 'comment') 
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_comments_q1'] = prod_comments_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_comments_q2'] = prod_comments_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_comments_q3'] = prod_comments_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_comments_q4'] = prod_comments_qs[4]
      # Productivity of votes
      prod_votes_qs = get_productivity_per_quantiles(votes_per_weeks, inc_users_per_weeks, 'vote')
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_votes_q1'] = prod_votes_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_votes_q2'] = prod_votes_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_votes_q3'] = prod_votes_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'prod_votes_q4'] = prod_votes_qs[4]
      # Participation
      part_qs = get_productivity_per_quantiles(cont_per_weeks, inc_users_per_weeks, 'contri')
      comm_qua_act[comm_qua_act$community_id==c_id,'parti_q1'] = part_qs[1]
      comm_qua_act[comm_qua_act$community_id==c_id,'parti_q2'] = part_qs[2]
      comm_qua_act[comm_qua_act$community_id==c_id,'parti_q3'] = part_qs[3]
      comm_qua_act[comm_qua_act$community_id==c_id,'parti_q4'] = part_qs[4]
    }
  }
  return (comm_qua_act)
}


plot_activity_per_week = function(community, activity_ds, activity_type) {  
  # Plot community idea activity
  activity_p_w = calculate_activity_per_week_vector(filter(activity_ds, community_id==community$id), activity_type)
  
  if (activity_type == "idea") {
    base = ggplot(activity_p_w, aes(x=as.numeric(week), y=idea))
    y_lab = labs(y="ideas")
  }
  if (activity_type == "comment") {
    base = ggplot(activity_p_w, aes(x=as.numeric(week), y=comment))
    y_lab = labs(y="comments")
  }
  if (activity_type == "vote") {
    base = ggplot(activity_p_w, aes(x=as.numeric(week), y=vote))
    y_lab = labs(y="votes")
  }
  if (activity_type == "user") {
    base = ggplot(activity_p_w, aes(x=as.numeric(week), y=user))
    y_lab = labs(y="users")
  }
  if (activity_type == "inter") {
    base = ggplot(activity_p_w, aes(x=as.numeric(week), y=inter))
    y_lab = labs(y="interactions")
  }
  if (activity_type == "cont") {
    base = ggplot(activity_p_w, aes(x=as.numeric(week), y=cont))
    y_lab = labs(y="contributors")
  }
  base + geom_line(size=1) + 
    labs(x="week") +
    y_lab + 
    ggtitle(paste('Community: ',community$name,sep=''))
}


plot_clusters = function(clusters, activity_type) {
  for (clus_id in unique(clusters$cluster)) {
    num_regs = nrow(filter(clusters,cluster==clus_id))
    clusters[clusters$cluster==clus_id,'cluster_id'] = paste('Cluster ',clus_id,' (N=',num_regs,')',sep='')  
  }
  if (activity_type == 'users') {
    verb = 'registered'
  } else {
    verb = 'produced'
  }
  colnames(clusters)[which(names(clusters) == "q0")] = "0"
  colnames(clusters)[which(names(clusters) == "q1")] = "3"
  colnames(clusters)[which(names(clusters) == "q2")] = "6"
  colnames(clusters)[which(names(clusters) == "q3")] = "9"
  colnames(clusters)[which(names(clusters) == "q4")] = "12"
  cluster_m =  melt(clusters, id.vars = c("community_id", "cluster", "cluster_id"), 
                    measure.vars = c("0","3", "6", "9", "12"))
  ggplot(cluster_m, aes(x=variable,y=value,group=community_id)) + 
    geom_line() +
    labs(x="Month",y=paste("Proportion of ", activity_type, " ", verb, sep="")) +
    facet_grid(cluster_id ~ .) +
    theme(axis.text=element_text(size=22),
          axis.title=element_text(size=22),
          strip.text.y = element_text(size = 20))
}


get_cluster_stats = function(communities, clusters, communities_lf) {
  
  for (clus_id in sort(unique(clusters$cluster))) {
    ids = clusters[clusters$cluster==clus_id, 'community_id']
    sub_communities = communities %>% filter(id %in% ids) %>% select(id, ideas, comments, votes, 
                                                                     users, contributors)
    print(paste('Cluster:',clus_id))
    print(paste('Avg. Ideas:',mean(sub_communities$ideas)))
    print(paste('Avg. Comments:',mean(as.numeric(sub_communities$comments))))
    print(paste('Avg. Votes:',mean(sub_communities$votes)))
    print(paste('Avg. Users:',mean(filter(sub_communities, users!=-1)$users, na.rm=T)))
    print(paste('Avg. Contributors:',mean(sub_communities$contributors)))
    part = round(filter(sub_communities, users!=-1)$contributors/filter(sub_communities, users!=-1)$users,3)
    print(paste('Avg. Participation',mean(part)))
    prod_ideas = round(filter(sub_communities, users!=-1)$ideas/filter(sub_communities, users!=-1)$users,3)
    print(paste('Avg. Productivity (ideas)',mean(prod_ideas)))
    prod_comments = round(filter(sub_communities, users!=-1)$comments/filter(sub_communities, users!=-1)$users,3)
    print(paste('Avg. Productivity (comments)',mean(prod_comments)))
    prod_votes = round(filter(sub_communities, users!=-1)$votes/filter(sub_communities, users!=-1)$users,3)
    print(paste('Avg. Productivity (votes)',mean(prod_votes)))
    sub_communities_lf = communities_lf %>% filter(id %in% ids)
    print(paste('Avg. Lifetime:',median(sub_communities_lf$lifetime_days)))
  }
}


get_clusters_stats_per_quantiles = function(communities_event_log, clusters) {
  print(paste('Computing community event log per quartile...'))
  communities_quantiles_activities = compute_activity_vectors_quantile(communities_event_log, 
                                                                       clusters)
  quantiles = c(1,2,3,4)
  for (clus_id in sort(unique(communities_quantiles_activities$cluster))) {
    cluster_communities = filter(communities_quantiles_activities,cluster==clus_id)
    for (quantile in quantiles) {
      print(paste('Quantile: ', quantile, sep=''))
      avg_ideas = round(mean(cluster_communities[,paste('ideas_q',quantile,sep='')]),2)
      avg_comments = round(mean(cluster_communities[,paste('comments_q',quantile,sep='')]),2)
      avg_votes = round(mean(cluster_communities[,paste('votes_q',quantile,sep='')]),2)
      avg_users = round(mean(cluster_communities[,paste('users_q',quantile,sep='')]),2)
      avg_contr = round(mean(cluster_communities[,paste('contri_q',quantile,sep='')]),2)
      avg_parti = round(mean(cluster_communities[,paste('parti_q',quantile,sep='')]),2)
      avg_prod_i = round(mean(cluster_communities[,paste('prod_ideas_q',quantile,sep='')]),2)
      avg_prod_c = round(mean(cluster_communities[,paste('prod_comments_q',quantile,sep='')]),2)
      avg_prod_v = round(mean(cluster_communities[,paste('prod_votes_q',quantile,sep='')]),2)
      avg_mod = round(mean(cluster_communities[,paste('inter_q',quantile,sep='')]),2)
      avg_mod_i = round(mean(cluster_communities[,paste('inter_ideas_q',quantile,sep='')]),2)
      avg_mod_c = round(mean(cluster_communities[,paste('inter_comments_q',quantile,sep='')]),2)
      avg_mod_v = round(mean(cluster_communities[,paste('inter_votes_q',quantile,sep='')]),2)
      row = c(clus_id,quantile,avg_ideas,avg_comments,avg_votes,avg_users,avg_contr,
              avg_parti,avg_prod_i,avg_prod_c,avg_prod_v,avg_mod,avg_mod_i,
              avg_mod_c,avg_mod_v)
      if (exists('res')) {
        res = rbind(res, row)  
      } else {
        res = matrix(row, nrow=1, ncol=15)
      }
    }
  }
  rownames(res) = NULL
  colnames(res) = c('cluster','quartile','ideas','comments','votes','users','contributors',
                    'participantion','productivity (ideas)','prod. (comments)','prod. (votes)',
                    'mod. interventions', 'mod. inter. (ideas)', 'mod. inter. (comments)',
                    'mod. inter. (votes)')
  return(data.frame(res))
}


get_clusters_correlation_per_quantiles = function(communities_event_log, clusters) {
  print(paste('Computing community event log per quartile...'))
  communities_quantiles_activities = compute_activity_vectors_quantile(communities_event_log, 
                                                                       clusters)
  remove(res)
  quantiles = c(1,2,3,4)
  for (clus_id in sort(unique(communities_quantiles_activities$cluster))) {
    cluster_communities = filter(communities_quantiles_activities,cluster==clus_id)
    for (quantile in quantiles) {
      print(paste('Quantile: ', quantile, sep=''))
      cor_t_idea_mod = cor.test(cluster_communities[,paste('ideas_q',quantile,sep='')], 
                                cluster_communities[,paste('inter_q',quantile,sep='')])
      cor_t_comm_mod = cor.test(cluster_communities[,paste('comments_q',quantile,sep='')], 
                                cluster_communities[,paste('inter_q',quantile,sep='')])
      cor_t_vote_mod = cor.test(cluster_communities[,paste('votes_q',quantile,sep='')], 
                                cluster_communities[,paste('inter_q',quantile,sep='')])
      if (cor_t_idea_mod$estimate > 0.50) {
        cor_idea_mod = paste(round(cor_t_idea_mod$estimate,2),' (',
                             round(cor_t_idea_mod$p.value,2),')',sep='')
      } else {
        cor_idea_mod = round(cor_t_idea_mod$estimate,2)
      }
      if (cor_t_comm_mod$estimate > 0.50) {
        cor_comment_mod = paste(round(cor_t_comm_mod$estimate,2),' (',
                                round(cor_t_comm_mod$p.value,2),')',sep='')
      } else {
        cor_comment_mod = round(cor_t_comm_mod$estimate,2)
      }
      if (cor_t_vote_mod$estimate > 0.50) {
        cor_vote_mod = paste(round(cor_t_vote_mod$estimate,2),' (',
                             round(cor_t_vote_mod$p.value,2),')',sep='')
      } else {
        cor_vote_mod = round(cor_t_vote_mod$estimate,2)
      }
      row = c(clus_id,quantile,cor_idea_mod,cor_comment_mod,cor_vote_mod)
      if (exists('res')) {
        res = rbind(res, row)  
      } else {
        res = matrix(row, nrow=1, ncol=5)
      }
    }
  }
  rownames(res) = NULL
  colnames(res) = c('cluster','quartile','ideas~inter','comments~inter','votes~inter')
  return(data.frame(res))
}


calculate_mod_interventions = function(users, ideas, comments, votes, clusters) {
  mod_inter = data.frame(community_id=clusters$community_id,ideas_inter=0, votes_inter=0, comments_inter=0, tot_inter=0)
  
  for (clus_id in unique(clusters$cluster)) {
    print(paste('Cluster:',clus_id))
    ids = clusters[clusters$cluster==clus_id, 'community_id']
    for (c_id in ids) {
      tot_intervention = 0
      mod_intervention = 0
      sub_ideas = ideas %>% filter(community_id==c_id)
      for (author_id in sub_ideas$author_id) {
        if (author_id > 0) {
          author = filter(users, id == author_id)
          if (author$admin || author$moderator) mod_intervention = mod_intervention + 1  
        }  
      }
      mod_inter[mod_inter$community_id==c_id,'ideas_inter'] = mod_intervention
      tot_intervention = tot_intervention + mod_intervention
      mod_intervention = 0
      sub_comments = comments %>% filter(community_id==c_id)
      for (author_id in sub_comments$author_id) {
        if (author_id > 0) {
          author = filter(users, id == author_id)
          if (author$admin || author$moderator) mod_intervention = mod_intervention + 1
        }
      }
      mod_inter[mod_inter$community_id==c_id,'comments_inter'] = mod_intervention
      tot_intervention = tot_intervention + mod_intervention
      mod_intervention = 0
      sub_votes = votes %>% filter(community_id==c_id)
      for (author_id in sub_votes$author) {
        if (author_id > 0) {
          author = filter(users, id == author_id)
          if (author$admin || author$moderator) {
            mod_intervention = mod_intervention + 1
          }
        }
      }
      mod_inter[mod_inter$community_id==c_id,'votes_inter'] = mod_intervention
      tot_intervention = tot_intervention + mod_intervention
      mod_inter[mod_inter$community_id==c_id,'tot_inter'] = tot_intervention
    }
    print(paste('Avg. mod intervention in the cluster: ', median(mod_inter[mod_inter$community_id %in% ids, 'tot_inter'])))
    print(paste('Avg. mod ideas in the cluster: ', median(mod_inter[mod_inter$community_id %in% ids, 'ideas_inter'])))
    print(paste('Avg. mod comments in the cluster: ', median(mod_inter[mod_inter$community_id %in% ids, 'comments_inter'])))
    print(paste('Avg. mod votes in the cluster: ', median(mod_inter[mod_inter$community_id %in% ids, 'votes_inter'])))
  }
}


get_mod_interventions = function(clusters) {
  tot_interventions = read.csv("./data/total_mod_interventions.csv", sep=",", header=T)
  
  for (clus_id in sort(unique(clusters$cluster))) {
    print(paste('Cluster:',clus_id))
    ids = clusters[clusters$cluster==clus_id, 'community_id']
    sub_inter = filter(tot_interventions, community_id %in% ids)
    print(paste('Avg. mod intervention in the cluster: ', mean(sub_inter$tot_inter)))
    print(paste('Avg. mod ideas in the cluster: ', mean(sub_inter$ideas_inter)))
    print(paste('Avg. mod comments in the cluster: ', mean(sub_inter$comments_inter)))
    print(paste('Avg. mod votes in the cluster: ', mean(sub_inter$votes_inter)))  
  }
}


identify_outliers = function(communities_reg, IDEA_THRESHOLD, USER_THRESHOLD, CONT_THRESHOLD,
                             VOTE_THRESHOLD, COMM_THRESHOLD, INTI_THRESHOLD, INTV_THRESHOLD,
                             INTC_THRESHOLD, INTT_THRESHOLD) {
  filtered_reg = select(communities_reg, id, ideas, users, contributors, votes, comments, 
                        inter_ideas, inter_comments, inter_votes, inter_total)  
  idea_outliers = filtered_reg[filtered_reg$ideas>IDEA_THRESHOLD,'id']
  user_outliers = filtered_reg[filtered_reg$users>USER_THRESHOLD&!is.na(filtered_reg$users),'id']
  cont_outliers = filtered_reg[filtered_reg$contributors>CONT_THRESHOLD,'id']
  vote_outliers = filtered_reg[filtered_reg$votes>VOTE_THRESHOLD,'id']
  comm_outliers = filtered_reg[filtered_reg$comments>COMM_THRESHOLD,'id']
  outliers = unique(c(idea_outliers, user_outliers, cont_outliers, vote_outliers, comm_outliers))
  inti_outliers = filtered_reg[filtered_reg$inter_ideas>INTI_THRESHOLD,'id']
  intv_outliers = filtered_reg[filtered_reg$inter_votes>INTV_THRESHOLD,'id']
  intc_outliers = filtered_reg[filtered_reg$inter_comments>INTC_THRESHOLD,'id']
  intt_outliers = filtered_reg[filtered_reg$inter_total>INTT_THRESHOLD,'id']
  int_outliers = unique(c(inti_outliers,intv_outliers,intc_outliers,intt_outliers))
  return (list(outliers=outliers,int_outliers=int_outliers))
}


##
# It seems that calculating the communities' lifetime by considering the time span between the
# first and last event is not totally precise. To remedy this, gaps of six months or more
# of inactivity are searched and if found, they are analyzed to see whether they are real
# event within the lifetime and not just occasional events that mislead the calculation of the 
# communities' age.
##
recalculate_communities_lifetime = function(communities_event_log) {
  ok_communities_lt = c()
  prob_communities = c()
  
  for (c_id in unique(communities_event_log$community_id)) {
    community_event_log = arrange(filter(communities_event_log, community_id==c_id),
                                  creation_datetime)
    community_event_log = filter(community_event_log, creation_datetime!='')
    community_event_log$creation_datetime = as.Date(
      as.character(community_event_log$creation_datetime))
    community_event_log$week=as.numeric(round(difftime(community_event_log[,'creation_datetime'], 
                                                       community_event_log[1,'creation_datetime'], 
                                                       units = "weeks"),0)) + 1
    current_week = community_event_log[1,'week']
    gap = FALSE
    gap_info = c()
    for (row in 2:nrow(community_event_log)) {
      diff_weeks = community_event_log[row,'week'] - current_week
      if (diff_weeks > 26) {  # Six months
        gap = TRUE
        print(paste("There is a gap of",diff_weeks,"weeks in the community",c_id,
                    "between the week", community_event_log[row,'week'],
                    "and the week",current_week))
        veredict = analyze_events_around_gap(community_event_log, row)
        if (veredict=='CUT_BEFORE' && 'CUT_BEFORE' %in% gap_info[,2]) {
          before_row = gap_info[gap_info[,2]=='CUT_BEFORE',]
          if (row > before_row[1]) {
            before_row[1] = row
          }
        } else if (veredict=='CUT_AFTER' && 'CUT_AFTER' %in% gap_info[,2]) {
          after_row = gap_info[gap_info[,2]=='CUT_AFTER',]
          if (row < after_row[1]) {
            after_row[1] = row
          }
        } else {
          gap_info = rbind(gap_info, c(row,veredict))
        }
      } 
      current_week = community_event_log[row,'week'] 
    }
    
    if (gap) {
      if ('CANNOT_DECIDE' %in% gap_info[,2]) {
        prob_communities = cbind(prob_communities, c_id)
        next
      }
      else {
        curated_event_log = c()
        for (i in 1:nrow(gap_info)) {
          if (gap_info[i,2]=='CUT_BEFORE') {
            curated_event_log = rbind(curated_event_log,
                                      community_event_log[gap_info[i,1]:nrow(community_event_log),])  
          }
          if (gap_info[i,2]=='CUT_AFTER') {
            curated_event_log = rbind(curated_event_log, 
                                      community_event_log[1:as.numeric(gap_info[i,1])-1,])
          }
        }
      }
      lf = as.numeric(difftime(curated_event_log[nrow(curated_event_log),'creation_datetime'], 
                               curated_event_log[1,'creation_datetime'], units = "days"))
    } else {
      lf = as.numeric(difftime(community_event_log[nrow(community_event_log),'creation_datetime'], 
                               community_event_log[1,'creation_datetime'], units = "days"))
    }
    ok_communities_lt = rbind(ok_communities_lt,c(c_id,lf))
  }  
  colnames(ok_communities_lt) = c('id','lifetime_days')
  
  return (list(data.frame(ok_communities_lt),prob_communities))
}


##
# Analyze whether the events that surround the gap represent occasional events
# in the lifetime of the community. A set of heuristics are applied to 
# determine it.
##
analyze_events_around_gap = function(community_event_log, cutting_row) {  
  events_before_gap = community_event_log[1:cutting_row-1,]
  events_after_gap = community_event_log[cutting_row:nrow(community_event_log),]
  tot_events_before_gap = nrow(events_before_gap)
  tot_events_after_gap = nrow(events_after_gap)
  tot_events_community = nrow(community_event_log)
  
  if (tot_events_before_gap == 1) {
    return('CUT_BEFORE')  
  }
  
  if (tot_events_after_gap == 1) {
    return('CUT_AFTER')
  }
  
  if (tot_events_before_gap/tot_events_community < 0.05) {
    # only vote events that can be discarded
    if ('vote' %in% events_before_gap$type_event && 
          length(unique(events_before_gap$type_event))==1) {
      return('CUT_BEFORE')
    }
    # checks the type of events, if there are new users registered 
    # very likely the segment contains spam
    if ('user' %in% events_before_gap$type_event) {
      return('CUT_BEFORE')
    }
  }
  
  if (tot_events_after_gap/tot_events_community < 0.05) {
    if ('vote' %in% events_after_gap$type_event && 
          length(unique(events_after_gap$type_event))==1) {
      return('CUT_AFTER')
    }
    if ('user' %in% events_after_gap$type_event) {
      return('CUT_AFTER')
    }
  }
  
  # if we reach this point, we cannot decide automatically 
  # nothing regarding the gap
  return('CANNOT_DECIDE')
}


##
# Extract the portion of the communities' events between min_week and max_week
##
slice_communities_log_by_weeks = function(communities_events, min_week, max_week) {
  communities_events_sublog = c()
  
  for (c_id in unique(communities_events$community_id)) {
    community_event_log = arrange(filter(communities_events, community_id==c_id),
                                  creation_datetime)
    community_event_log = filter(community_event_log, creation_datetime!='')
    community_event_log$creation_datetime = as.Date(
      as.character(community_event_log$creation_datetime))
    community_event_log$week=as.numeric(round(difftime(community_event_log[,'creation_datetime'], 
                                                       community_event_log[1,'creation_datetime'], 
                                                       units = "weeks"),0)) + 1
    communities_events_sublog = rbind(communities_events_sublog, 
                                      community_event_log %>% filter(week>=min_week) %>% 
                                        filter(week<=max_week))
  }
  
  return (communities_events_sublog)
}


##
# Check whether there are bursts in the creation of ideas or comments.
# A burst is identified if two ideas or comments where created by the same author in 
# less than 30 seconds. We assume that the creation of ideas and comments should take more time.
# This situation may indicate the presence of repeated or sporious content generated by automatic 
# posting actions or by system failures. If sporious content represents a significant part of the
# it should be removed.
##
search_content_creation_bursts = function(communities_events) {
  communities_with_burst = c()
  
  for (c_id in unique(communities_events$community_id)) {
    print(paste('Processing community',c_id,'...'))
    community_event_log = arrange(filter(communities_events, community_id==c_id),
                                  creation_datetime)
    community_event_log = filter(community_event_log, creation_datetime!='')
    idea_creation_log = filter(community_event_log, type_event=='idea')
    comment_creation_log = filter(community_event_log, type_event=='comment')
    # Inspect idea creation events
    if (nrow(idea_creation_log) > 1) {
      current_dt = idea_creation_log[1,'creation_datetime']
      author_id = idea_creation_log[1,'author_id']
      current_row = 1
      first_time = T
      for (row in 2:nrow(idea_creation_log)) {
        next_dt = idea_creation_log[row,'creation_datetime']
        diff = as.numeric(round(difftime(as.POSIXlt(next_dt,format="%Y-%m-%d %H:%M:%S",tz="GMT"), 
                                         as.POSIXlt(current_dt,format="%Y-%m-%d %H:%M:%S",tz="GMT"), 
                                         units = "secs"),1))
        if (diff < 30 && author_id == idea_creation_log[row,'author_id']) {
          #print(paste('Probable burst event in the creation of ideas (diff: ',diff,', row: ',row,
          #            '), community: ',c_id,sep=''))
          if (first_time) {
            communities_with_burst = rbind(communities_with_burst, 
                                           idea_creation_log[current_row,])  
            first_time = F
          }
          communities_with_burst = rbind(communities_with_burst, 
                                         idea_creation_log[row,])
        } else {
          current_dt = next_dt
          author_id = idea_creation_log[row,'author_id']
          current_row = row
          first_time = T
        }
      }  
    } else {
      print(paste('Community',c_id,'does not have ideas'))
    }
    # Inspect comment creation events
    if (nrow(comment_creation_log) > 1) {
      current_dt = comment_creation_log[1,'creation_datetime']
      author_id = comment_creation_log[1,'author_id']
      current_row = 1
      first_time = T
      for (row in 2:nrow(comment_creation_log)) {
        next_dt = comment_creation_log[row,'creation_datetime']
        diff = as.numeric(round(difftime(as.POSIXlt(next_dt,format="%Y-%m-%d %H:%M:%S",tz="GMT"), 
                                         as.POSIXlt(current_dt,format="%Y-%m-%d %H:%M:%S",tz="GMT"), 
                                         units = "secs"),1))
        if (diff < 30 && author_id == comment_creation_log[row,'author_id']) {
          if (first_time) {
            communities_with_burst = rbind(communities_with_burst, 
                                           comment_creation_log[current_row,])
            first_time = F
          }
          communities_with_burst = rbind(communities_with_burst, 
                                         comment_creation_log[row,])  
        } else {
          current_dt = next_dt
          author_id = comment_creation_log[row,'author_id']
          current_row = row
          first_time = T
        }
      }
    } else {
      print(paste('Community',c_id,'does not have comments'))
    }
  }
  
  tot_sp = nrow(communities_with_burst)
  tot_co = nrow(communities_events)
  print(paste('Sporious content represent ',round(tot_sp/tot_co,3),'% of total',sep=''))
}


##
# Compute descriptive statistics about the communities
##
compute_statistics = function(communities_events) {
  types = c('comment','vote','user','inter_idea','inter_comment','inter_vote')
  
  grouped_content = group_by(filter(communities_events,type_event=='idea'), community_id)
  summarized_content = summarise(grouped_content, total=n())
  summary_table = c(summary(summarized_content$total)[1:6],round(sd(summarized_content$total),2),
                    sum(summarized_content$total))
  for (type in types) {
    grouped_content = group_by(filter(communities_events,type_event==type), community_id)
    summarized_content = summarise(grouped_content, total=n())
    summary_table = rbind(summary_table, c(summary(summarized_content$total)[1:6], 
                                           round(sd(summarized_content$total),2),
                                           sum(summarized_content$total)))  
  }
  inter_total = filter(communities_events,type_event=='inter_idea'|type_event=='inter_comment'
                       |type_event=='inter_vote')
  grouped_content = group_by(inter_total, community_id)
  summarized_content = summarise(grouped_content, total=n())
  summary_table = rbind(summary_table, c(summary(summarized_content$total)[1:6],
                                         round(sd(summarized_content$total),2),
                                         sum(summarized_content$total)))  
  rownames(summary_table) = c('ideas',types,'total interventions')
  colnames(summary_table) = c(colnames(summary_table)[1:6],'St. Dev.','Total')
  return(summary_table)
}

# Analyze correlations between interested variables
# and independent variables
get_corr_matrix = function(data, int_var, ind_vars) {
  m = length(ind_vars)
  n = 3
  mat_cor = matrix(sample(0, m * n, replace = TRUE), ncol=n,nrow=m)
  for (i in 1:length(ind_vars)) {
    i_var = ind_vars[i]
    cor_test = cor.test(data[,int_var],data[,i_var])
    if (abs(cor_test$estimate) > 0.5) {
      if (cor_test$p.value < 0.05) {
        row = c(round(as.numeric(cor_test$estimate),2), '*', '*')
      } else {
        row = c(round(as.numeric(cor_test$estimate),2), '*', ' ')
      }
    } else {
      row = c(round(as.numeric(cor_test$estimate),2), ' ', ' ')
    }
    mat_cor[i,] = row
  }
  colnames(mat_cor) = c(int_var,'important','significant')
  rownames(mat_cor) = ind_vars
  
  return (mat_cor)
}

tukey_outlier = function (data) 
{
  descriptives = summary(data)
  Q1 = descriptives[[2]]
  Q3 = descriptives[[5]]
  IQR = Q3 - Q1
  out = subset(data, data <= (Q1 - 1.5 * IQR) | data >= (Q3 + 
                                                           1.5 * IQR))
  sub = subset(data, data > (Q1 - 1.5 * IQR) & data < (Q3 + 
                                                         1.5 * IQR))
  return(list(outliers = out, subset = sub))
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