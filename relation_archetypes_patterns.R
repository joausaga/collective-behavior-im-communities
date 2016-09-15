library(dplyr)
library(reshape2)
library(ggplot2)

# Load dataset
communities = read.csv('data/tagged_communities_and_archetypes.csv', header=T)

# Select important columns
arc_pat = dplyr::select(communities, user_cluster, idea_cluster, comment_cluster, 
                 vote_cluster, archetype)

# Convert columns to factor
arc_pat$user_cluster = as.factor(arc_pat$user_cluster)
arc_pat$idea_cluster = as.factor(arc_pat$idea_cluster)
arc_pat$comment_cluster = as.factor(arc_pat$comment_cluster)
arc_pat$vote_cluster = as.factor(arc_pat$vote_cluster)
arc_pat$archetype = as.factor(arc_pat$archetype)

# Case 1: correlation between archetypes and member reg patterns
cont_table = table(arc_pat$archetype,arc_pat$user_cluster)
chisq.test(cont_table)

# Case 2: correlation between archetypes and idea generation patterns
cont_table = table(arc_pat$archetype,arc_pat$idea_cluster)
chisq.test(cont_table)

# Case 3: correlation between archetypes and comment generation patterns
cont_table = table(arc_pat$archetype,arc_pat$comment_cluster)
chisq.test(cont_table)

# Case 4: correlation between archetypes and vote generation patterns
cont_table = table(arc_pat$archetype,arc_pat$vote_cluster)
chisq.test(cont_table)
df_table = data.frame(cont_table)
colnames(df_table) = c('archetype','pattern','freq')
df_table$pattern = factor(df_table$pattern, levels = c('5','4','3','2','1'))
ggplot(df_table, aes(x = pattern, y = freq, fill = archetype)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_grey(start = 0, end = .9) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        panel.margin = element_blank())

cont_table[1,]/sum(cont_table[1,])
