require(librarian)
librarian::shelf(plyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud,tm, plotly)



# Local Import Path
assets_pubs <- "../1-swf-knowledge.base/assets/data/raw" 
t_df <- as_tibble(read_csv(paste(assets_pubs,"230321_research_rabbit_scaling_pubs.csv",sep='/'),show_col_types = FALSE))
# 
# write.table(t_df,paste(assets_pubs,"230321_research_rabbit_scaling_pubs.csv",sep='/'))
# read.table( pipe( paste0( "sed s'/[[:punct:]]//g' /Users/Simon/input.txt" ) ) , head=TRUE)

# Cleaning the data set
pub_dat <- t_df %>%
  select(Title, Abstract,Authors,Journal,Year) %>% 
  rename(title = Title,
         abstract = Abstract,
         authors = Authors,
         journal = Journal,
         year = Year) %>% 
  mutate(id = seq(from =1, to= nrow(t_df),by=1)) %>% 
  mutate(abstract=removeNumbers(abstract)) %>%
  mutate(title = removeNumbers(title)) %>% 
  mutate(title = tolower(title)) %>% 
  mutate(abstract = tolower(abstract)) %>% 
  mutate(journal = tolower(journal)) %>% 
  mutate(abstract=gsub(paste0('\\b',tm::stopwords("english"), '\\b', 
                              collapse = '|'), '', abstract)) %>% 
  mutate(title=gsub(paste0('\\b',tm::stopwords("english"), '\\b', 
                              collapse = '|'), '', title)) %>% 
  mutate(id = factor(id))

# Global removal of stopwords: 
#https://stackoverflow.com/questions/64361808/r-remove-stopwords-from-text-without-tokenizing-transforming-data-into-list

# Titles

pub_cmp <- "title"

pub_tokens <- pub_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = pub_cmp, drop = FALSE)%>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(year) #%>% 
  # count(word) %>% 
  # mutate(length = nchar(word)) 

# Word clouds
p1 <- ggplot(pub_tokens, 
             aes(x = year,
                 y = n,
                 label = word, 
                 size = n, 
                 color = as.factor(year))) +
  geom_text_wordcloud(area_corr_power = 1) +
  # scale_y_log10()+
  scale_radius(range = c(0, 20),
               limits = c(0, NA))+
  theme_minimal()
p1

# Title n-grams

gram_l = 2
n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')

pub_ngrams <- pub_dat %>%
  ungroup() %>%
  filter(str_detect(pub_cmp,"[:alpha:]")) %>%
  unnest_tokens(n_gram, pub_cmp, token = "ngrams", n = gram_l) %>%
  filter(!str_detect(n_gram, "[:punct:]|[:digit:]")) %>% 
  filter(!n_gram %in% c(stop_words$word)) %>%
  separate(n_gram, columns, sep = " ", remove = FALSE) %>%
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(pub_ngrams)

# n_gram_res determines the rank threshold to be used to filter the number of
# words included in the visualization. When multiplied by the total number of 
# words (nrow(pub_ngrams)), yields a fraction of the total.

# Low values for ngram_res, correspond to lower number of words used.

# For titles I will use a larger number than for abstracts (see below)

n_gram_res = 0.25

ngram_graph <- pub_ngrams %>%
  filter(rank < n_gram_res*nrow(pub_ngrams)) %>%
  graph_from_data_frame()
ngram_graph

p2 <- ggraph(ngram_graph,
             layout = "fr")+
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "sienna3") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
p2


# Abstracts

pub_cmp <- "abstract"

pub_tokens <- pub_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = pub_cmp, drop = FALSE)%>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(year) %>% 
  count(word, sort = TRUE) %>% 
  mutate(length = nchar(word)) 

# Word clouds
p1 <- ggplot(pub_tokens, 
             aes(x = year,
                 y = n,
                 label = word, 
                 size = n, 
                 color = as.factor(year))) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_y_log10()+
  scale_radius(range = c(0, 20),
               limits = c(0, NA))+
  theme_minimal()
p1

# Abstracts n-grams

gram_l = 3
n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')

n_gram_res <- 0.02


pub_ngrams <- pub_dat %>%
  ungroup() %>%
  filter(str_detect(pub_cmp,"[:alpha:]")) %>%
  unnest_tokens(n_gram, pub_cmp, token = "ngrams", n = gram_l) %>%
  filter(!str_detect(n_gram, "[:punct:]|[:digit:]")) %>% 
  filter(!n_gram %in% c(stop_words$word)) %>%
  separate(n_gram, columns, sep = " ", remove = FALSE) %>%
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(pub_ngrams)

ngram_graph <- pub_ngrams %>%
  filter(rank < n_gram_res*nrow(pub_ngrams)) %>%
  graph_from_data_frame()
ngram_graph

p2 <- ggraph(ngram_graph,
             layout = "fr")+
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "orchid4") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
p2



































# Titles

titl_tokens <- pub_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = title, drop = FALSE)%>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  # anti_join(stop_words, by = "word")%>%
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(year) %>% 
  count(word, sort = TRUE) %>% 
  mutate(length = nchar(word)) 

# Word clouds
p <- ggplot(filter(titl_tokens), 
            aes(label = word, 
                size = n, 
                color = as.factor(year))) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_radius(range = c(0, 20),
               limits = c(0, NA))+
  # facet_wrap(~year)+
  theme_minimal()
p

p1 <- ggplot(titl_tokens, 
             aes(x = year,
                 y = n,
                 label = word, 
                 size = n, 
                 color = as.factor(year))) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_y_log10()+
  scale_radius(range = c(0, 20),
               limits = c(0, NA))+
  theme_minimal()
p1

# Title n-grams

#Define the length of the gram

gram_l = 4
n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')

pub_cmp <- "title"


pub_ngrams <- pub_dat %>%
  ungroup() %>%
  filter(str_detect(pub_cmp,"[:alpha:]")) %>%
  unnest_tokens(n_gram, pub_cmp, token = "ngrams", n = gram_l) %>%
  filter(!str_detect(n_gram, "[:punct:]|[:digit:]")) %>% 
  filter(!n_gram %in% c(stop_words$word)) %>%
  separate(n_gram, columns, sep = " ", remove = FALSE) %>%
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(pub_ngrams)

n_gram_res <- 0.2

ngram_graph <- pub_ngrams %>%
  filter(rank < n_gram_res*nrow(pub_ngrams)) %>%
  graph_from_data_frame()
ngram_graph

p2 <- ggraph(ngram_graph,
             layout = "fr")+
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
p2
























set.seed(2017)

l <- layout_with_fr(ngram_graph)
e <- get.edgelist(ngram_graph,names=FALSE)
m <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(ngram_graph))
deg <- degree(ngram_graph,mode="all")
fsize <- degree(ngram_graph, mode= "all")

#png(filename=paste("assets/NetworkAnalysis_words_",Sys.Date(),".png", sep = ""), res = 100)

plot(ngram_graph,
     layout=m, 
     edge.arrow.size =.05,
     vertex.color = "pink", 
     vertex.size =150,
     vertex.frame.color="deeppink",
     vertex.label.color="black", 
     vertex.label.cex=0.75, #fsize/2,
     vertex.label.dist=0.6,
     edge.curve = 0.85,
     edge.color="skyblue",
     edge.label.family="Ariahttp://127.0.0.1:35229/graphics/plot_zoom_png?width=1536&height=898l", 
     rescale=F, 
     axes = FALSE, 
     ylim = c(-150,150), 
     xlim = c(-150,150),
     asp =0)

a <- graph_from_data_frame(abs_ngrams)



plotly::plot_ly(ngram_graph)


# Using ggwordclouds:
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

p <- ggplot(filter(titl_tokens), 
            aes(label = word, 
                size = n, 
                color = as.factor(year))) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_radius(range = c(0, 20),
               limits = c(0, NA))
p

abst_tokens <- pub_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = abstract, drop = FALSE)%>%
  anti_join(stop_words, by = "word")%>%
  filter(str_detect(word,"[:alpha:]"))%>%
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(year) %>% 
  count(word, sort = FALSE) %>% 
  mutate(length = nchar(word)) 

summary(abst_tokens)

  p <- ggplot(filter(abst_tokens,n>1), 
              aes(label = word, 
                  size = n, 
                  color = as.factor(year))) +
    geom_text_wordcloud(area_corr_power = 1) +
    scale_radius(range = c(0, 20),
                 limits = c(0, NA)) 
  p

# n-grams
  
#Define the length of the gram
  
gram_l = 3
n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')


abs_ngrams <- pub_dat %>%
  ungroup() %>%
  filter(str_detect(abstract,"[:alpha:]")) %>%
  unnest_tokens(n_gram, abstract, token = "ngrams", n = gram_l) %>%
  separate(n_gram, columns, sep = " ") %>%
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(abs_ngrams)

options(ggrepel.max.overlaps = Inf)

abs_ngrams %>% 
  filter(n>1) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "tomato") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
  
  
a <- graph_from_data_frame(abs_ngrams)

b <- ggraph(a,layout = "fr")+
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "tomato") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
b



n_gram_res <- 0.02

ngram_graph <- abs_ngrams %>%
  filter(rank < n_gram_res*nrow(abs_ngrams)) %>%
  graph_from_data_frame()
ngram_graph

set.seed(2017)

l <- layout_with_fr(ngram_graph)
e <- get.edgelist(ngram_graph,names=FALSE)
m <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(ngram_graph))
deg <- degree(ngram_graph,mode="all")
fsize <- degree(ngram_graph, mode= "all")

#png(filename=paste("assets/NetworkAnalysis_words_",Sys.Date(),".png", sep = ""), res = 100)

plot(ngram_graph,
     layout=m, 
     edge.arrow.size =.05,
     vertex.color = "pink", 
     vertex.size =deg*150,
     vertex.frame.color="deeppink",
     vertex.label.color="black", 
     vertex.label.cex=0.75,
     vertex.label.dist=0.25,
     edge.curve = 0.75,
     edge.color="skyblue",
     edge.label.family="Arial", 
     rescale=F, 
     axes = FALSE, 
     ylim = c(-150,170), 
     xlim = c(-150,200),
     asp =0)

#####################################
plot(ngram_graph,
     layout=m, 
     edge.arrow.size =.05,
     vertex.color = "pink", 
     vertex.size =500,
     vertex.frame.color="deeppink",
     vertex.label.color="black", 
     vertex.label.cex=fsize/3,
     vertex.label.dist=0.6,
     edge.curve = 0.75,
     edge.color="skyblue",
     edge.label.family="Ariahttp://127.0.0.1:35229/graphics/plot_zoom_png?width=1536&height=898l", 
     rescale=F, 
     axes = FALSE, 
     ylim = c(-190,190), 
     xlim = c(-180,200),
     asp =0)
