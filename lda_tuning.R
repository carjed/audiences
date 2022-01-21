result <- FindTopicsNumber(
       bios_dtm,
       topics = seq(from = 5, to = 25, by = 1),
       metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
       method = "VEM",
       control = list(seed = 77),
       mc.cores = 10L,
       verbose = TRUE
   )

FindTopicsNumber_plot(result)

bios_lda <- LDA(bios_dtm, k = 12, control = list(alpha=0.1, seed = 5678), method="VEM")
bios_lda_td <- tidy(bios_lda)

top_terms <- bios_lda_td %>%
  group_by(topic) %>%
  top_n(30, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% 
  # mutate(term = text_to_emoji(term)) %>%
  mutate(term = gsub("hashtag", "#", term))

topics_terms <- top_terms %>% 
  dplyr::select(-beta) %>% 
  # mutate(topic=paste0("t", topic)) %>%
  group_by(topic) %>% 
  summarise(top_10=paste(term, collapse=", ")) %>% 
  ungroup()

topics_terms_levels <- paste0(topics_terms$topic, ": ", topics_terms$top_10)

bios_lda_gamma <- tidy(bios_lda, matrix = "gamma") %>%
  rowwise() %>%
  mutate(gamma=gamma+runif(1,0,0.0001))
