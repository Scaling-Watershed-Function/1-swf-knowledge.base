# Rnotes 
[[Rnotes]] are used to document progress in [[RStudio]] along a selected path within  [[Workflows]]

## Quick note
**Date**: 2023-03_29
**People:** [[Francisco J. Guerrero]]
**Products:**
**Platforms:**

#### Note
# To R Notes

Help from Nick Lyon

pub_comp_c <- t_df_c %>% 
  dplyr::select(dplyr::contains(pub_comp)) %>%  
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

#### Tasks



## Quick note
**Date**: 2023-03_20
**People:**[[Francisco J. Guerrero]]
**Products:**[[Scripts]]
**Platforms:**[[RStudio]]

#### Note
How to style tables in Quarto documents using html code:

1. [Option to left align table captions](https://github.com/quarto-dev/quarto-cli/discussions/1042)
2. [Changing font size in a table](https://github.com/marp-team/marp/discussions/217)



## Quick note
**Date**: 2023-03_17
**People:**[[Francisco J. Guerrero]]
**Products:**[[Scripts]]
**Platforms:**[[RStudio]], [[ChatGPT]]

#### Note
You
how to fix this code, so I don't have to write the variable names within count: 
```{r}
abs_ngrams <- pub_dat%>% ungroup() %>% 
filter(str_detect(abstract,"[:alpha:]"))%>% 
unnest_tokens(n_gram, abstract, token = "ngrams", n = gram_l) %>% separate(n_gram,columns, sep = " ") %>%  
count(word1,word2,word3,word4,word5,sort = TRUE) %>% 
mutate(rank = row_number(), 
total=sum(n), 
t_freq = n/total) 
head(abs_ngrams)
```

ChatGPT

To avoid writing variable names within count, we can use the `across()` function to apply the `count()` function to multiple columns. Here's the updated code:
```
abs_ngrams <- pub_dat %>%
ungroup() %>%
filter(str_detect(abstract,"[:alpha:]")) %>%
unnest_tokens(n_gram, abstract, token = "ngrams", n = gram_l) %>%
separate(n_gram, columns, sep = " ") %>%
count(across(columns, ~.x), sort = TRUE) %>%
mutate(rank = row_number(),
total = sum(n),
t_freq = n/total)
head(abs_ngrams)
```
In this updated code, instead of passing variable names within count (word1, word2, word3, etc.), we use the `across()` function to select columns by their names or positions. The `~.x` notation is used to reference each selected column within the anonymous function passed to `across()`. This will count the frequency of each combination of words, regardless of the number of words in each n-gram.
#### Tasks