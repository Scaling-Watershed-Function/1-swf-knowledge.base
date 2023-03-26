require(librarian)
librarian::shelf(plyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud,tm, plotly)



# Local Import Path
assets_pubs <- "../1-swf-knowledge.base/assets/data/raw" 
t_df <- as_tibble(read_csv(paste(assets_pubs,"230321_research_rabbit_scaling_pubs.csv",sep='/'),show_col_types = FALSE))
t_df

# For our analyses below, we won't need DOIs, PMIDs or arXiv IDs. We will focus 
# on Title, Abstract, Authors, Journal, Year. To keep column names formatting 
# simple, I use lowercase and spaces are added with (_) when needed. 

# Since research rabbit pulls information from multiple databases, in some cases
# is possible that the abstracts for the papers are not retrieved. In such cases, 
# the value returned would be NA (which is read as a character string and not as 
# the typical NA). It could also happen that the abstracts could be partially 
# retrieved. To filter these cases out, we will add another column to the data 
# frame to count the character lenght of each abstract, and remove those that are 
# less than 20 characters long. Finally, we will add a sequential id and make it 
# a factor for visualization purposes.

t_df_c <- t_df %>% 
  select(Title,Abstract,Authors,Journal,Year) %>% 
  rename(title = Title,
         abstract = Abstract,
         authors = Authors,
         journal = Journal,
         year = Year) %>% 
  mutate(abstract_lenght = nchar(abstract)) %>% 
  filter(abstract_lenght > 20)

t_df_c <- t_df_c%>% 
  mutate(id = seq(from =1, to= nrow(t_df_c),by=1)) %>% 
  mutate(id = factor(id))

t_df_c

# In this case we went from 104 publication items, to 96 (i.e. 8 items with no
# abstract retrieved.)

# To make sure all words are processed correctly, we need to do some additional 
# cleaning on the text data. That includes unnesting each publication component 
# (pub_comp) into tokens (i.e. single words), performing the cleaning tasks 
# (i.e. singularizing,removing punctuations and digits, removing stop words (a,
# an, by, ...)), and finally putting the tokens back together (i.e. nesting)

# How to unnest and nest text data in using tidytext? check this post: 
# https://stackoverflow.com/questions/49118539/opposite-of-unnest-tokens-in-r

# Choosing analysis level

# You can choose the publication component (pub_comp) you want to focus your 
# analysis on. In this case, our options are title or abstract

pub_comp <- "title"

# Preparing the data

pub_comp_c <- select(t_df_c,pub_comp) %>% 
  rename(pub_comp_words = pub_comp) %>% 
  unnest_tokens(output = word, input = pub_comp_words, drop = FALSE) %>% 
  rowwise() %>% mutate(word = tolower(word)) %>% 
  rowwise() %>% mutate(word = if_else(word == "data","data",singularize(word))) %>%  
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  filter(!word %in% c(stop_words$word)) %>% 
  nest(word) %>% 
  mutate(!!paste0(pub_comp) := map_chr(map(data, unlist), paste, collapse = " "))
  
# Combining our original data frames for text analysis

t_df_c_m <- t_df_c %>% 
  select(year,authors,journal)

pub_comp_m <- select(pub_comp_c,!!paste0(pub_comp))

pub_dat <- as_tibble(cbind(pub_comp_m,t_df_c_m))

pub_dat
                      
  
# Tokenization

# Our first step to analyze the selected publication component, is to break it 
# into individual words (or tokens) and store the tokens in a new data frame for 
# (pub_tokens). There are several packages you could use to tokenize a piece of 
# text, here we will use the tidytext package for most of our analysis. 

# What the following chunk of code does is: 1) call the pub_dat dataset, 2) break the 
# chunk of (nested) text into tokens (output = word) by using the function unnest_tokens(),
# 3) eliminating duplicated words with distinct(), 4) grouping the tokens (word) by years,
# 5) calculating the frequency of a given token in each year -count(), and adding a new
# column with the numnber of characters -nchar for each token, so we can filter monosyllabes.

pub_tokens <- pub_dat %>% 
  unnest_tokens(output = word, input = pub_comp, drop = FALSE)%>%
  distinct() %>% 
  group_by(year) %>% 
  count(word, sort = TRUE) %>%
  mutate(length = nchar(word)) %>% 
  filter(length>2) 

# Time-indexed word clouds
res_plot <- 0.2
depth <- res_plot*nrow(pub_tokens)

p <- ggplot(pub_tokens[c(1:depth),], 
            aes(x = year,
                y = n,
                label = word, 
                size = n, 
                color = as.factor (year))) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_radius(range = c(0, 25),
               limits = c(0, NA)) +
  scale_y_log10()+
  xlab("Year")+
  ylab("Frequency (n)")+
  theme_minimal()
p

# Conceptual maps from n-grams

gram_l = 2
breath = 200
time_window = 2008


n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')

pub_ngrams <- pub_dat %>%
  ungroup() %>%
  unnest_tokens(n_gram, pub_comp, token = "ngrams", n = gram_l) %>%
  # separate(n_gram, columns, sep = " ", remove = FALSE) %>%
  group_by(year) %>% 
  # count(across(all_of(columns), ~.x), sort = TRUE) %>%
  count(n_gram, sort = TRUE) %>% 
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(pub_ngrams)

# n_gram_res determines the rank threshold to be used to filter the number of
# words included in the visualization. When multiplied by the total number of 
# words (nrow(pub_ngrams)), yields a fraction of the total.

# Low values for ngram_res, correspond to lower number of words used.

# For titles I will use a larger number than for abstracts (see below)

ngram_graph <- pub_ngrams %>%
  filter(rank < breath) %>%
  filter(year < time_window) %>% 
  graph_from_data_frame()
ngram_graph


p2 <- ggraph(ngram_graph,
             layout = "fr")+
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "sienna3") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines"), size = 2.5) +
  theme_void()
p2







