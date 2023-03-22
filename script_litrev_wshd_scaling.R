###############################################################################
# Text Analysis Lit. Review for Scaling Watershed Fucntion
###############################################################################

#By : Francisco Guerrero
#Data source: Research Rabbit + Zotero Compile list of Abstracts

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,
                 tidyr,
                 readr,
                 reshape2,
                 tidytext,
                 pluralize,
                 quanteda,
                 widyr,
                 igraph,
                 ggraph,
                 wordcloud,
                 ggraph,
                 graphlayouts,
                 cowplot,
                 ggwordcloud)
set.seed(2703)


################################################################################
# Relative paths: 

# Data import

import_data <- "../1-swf-knowledge.base/assets/data/raw/"

# Assets exports

assets_data <- "../1-swf-knowledge.base/assets/data/processed/"
assets_plot <- "../1-swf-knowledge.base/assets/plots/"
################################################################################

l_df <- as_tibble(read.csv(paste0(import_data,"230304_scaling_pubs.csv")))

# We have 87 fields (mostly NAs), from which we only need a few:

tidy_ldf <- dplyr::select(l_df,Key,Publication.Year,Author,Title,Abstract.Note)

# Let's now rename the columns for easier manipulation

tidy_ldf <- rename(tidy_ldf,
                   key = Key,
                   year = Publication.Year,
                   author = Author,
                   title = Title,
                   abstract = Abstract.Note)

# Let's take a look at the distribution of number of papers per year

hist(tidy_ldf$year)

# It looks like most papers were published around 2010. We will use this year as
# a breakpoint to create wordclouds.

tidy_ldf <- tidy_ldf %>% 
  mutate(period = ifelse(year < 2009, "Pre-Cole et al., 2007","Post-Cole et al., 2007"))
  
tidy_ldf$period <- factor(tidy_ldf$period,levels = c("Pre-Cole et al., 2007",
                                                     "Post-Cole et al., 2007"))

# Let's start with a wordcloud for titles

# First, we need to tokenize the titles

title_tokens <- tidy_ldf %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = title, drop = FALSE) %>%
  anti_join(stop_words, by = "word") %>% 
  filter(str_detect(word,"[:alpha:]")) %>% 
  rowwise() %>% mutate(word = singularize(word)) %>% 
  distinct() %>% 
  group_by(period) %>% 
  count(word,sort = TRUE) %>% 
  mutate(length = nchar(word))
  
# We will now use ggwordclouds to plot wordclouds for the two periods of interest

# By inspecting our data set with summary, 

summary(title_tokens$n)

##we find that most words are only used once across multiple titles
# let's start with words that are used at least twice.

p <- ggplot(filter(title_tokens,n > 1),
            aes(label = word,
                size = n,
                color = period)) +
  geom_text_wordcloud(area_corr_power = 1)+
  scale_radius(range = c (0,20),
               limits = c(0,NA)) +
  facet_wrap(~period, ncol = 2)
p
  
# Let's now explore the abstracts: 

abst_tokens <- tidy_ldf %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = abstract, drop = FALSE) %>%
  anti_join(stop_words, by = "word") %>% 
  filter(str_detect(word,"[:alpha:]")) %>% 
  rowwise() %>% mutate(word = singularize(word)) %>% 
  distinct() %>% 
  group_by(period) %>% 
  count(word,sort = TRUE) %>% 
  mutate(length = nchar(word))

p1 <- ggplot(filter(abst_tokens,n > 2),
            aes(label = word,
                size = n,
                color = period)) +
  geom_text_wordcloud(area_corr_power = 1)+
  scale_radius(range = c (0,20),
               limits = c(0,NA)) +
  facet_wrap(~period, ncol = 2)
p1




