lookup <- lookup %>%
  mutate(Unicode2=gsub("U[+]", "", Unicode)) %>%
  mutate(Unicode2=gsub(" ", "-", Unicode2)) %>%
  mutate(Unicode2=gsub("00", "", Unicode2))

lookup$url <- paste0("https://abs.twimg.com/emoji/v1/72x72/", lookup$Unicode2, ".png")

lookup$html <- tolower(paste0("<img src='", lookup$url, "' width='", 6, "'/>"))

# emoji_to_link <- function(x){
#   # img_formatted <- lookup %>% dplyr::filter(Native==x) %>% pull(html) %>% tolower()
#   img_formatted <- lookup %>% dplyr::filter(Desc_Sub==x) %>% pull(html) %>% tolower()
#   if(length(img_formatted)==1){
#     return(img_formatted)
#   } else {
#     return(" ")
#   }
# }

emoji_to_link <- function(x) Reduce(function(x,r) gsub(lookup$Native[r],lookup$html[r],x,fixed=T),seq_len(nrow(lookup)),x)

########################################
for(panel in 1:4){
  article_id <- article_ids[panel]
  
  panel_title <- title <- summary_data_df %>% 
    dplyr::filter(altmetric_id==article_id) %>% 
    mutate(caption=paste0(toupper(letters[panel]), ". \"", gsub(", bioRxiv,.*", "", title), ",\"\n", 
                          gsub(",.*", "", authors), " et al., ", gsub(".*20", "20", title))) %>% 
    pull(caption)
  
  bios_fh <- paste0(datadir, "/article_data/bios_dtm_", article_id, ".rds")
  
  bios_dtm <- readRDS(bios_fh)
  
  bios_lda6 <- LDA(bios_dtm, k = 12, control = list(alpha=0.1, seed = 5678), method="VEM")
  bios_lda_td <- tidy(bios_lda6)
  
  top_terms <- bios_lda_td %>%
    group_by(topic) %>%
    top_n(30, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>% 
    mutate(term = text_to_emoji(term)) %>%
    mutate(term = gsub("hashtag", "#", term))
  
  topics_terms <- top_terms %>% 
    dplyr::select(-beta) %>% 
    # mutate(topic=paste0("t", topic)) %>%
    group_by(topic) %>% 
    summarise(top_10=paste(term, collapse=", ")) %>% 
    ungroup()
  
  topics_terms <- topics_terms %>% 
    mutate(top_10=str_wrap(sub(sprintf("^((.*?,\ ){%d}.*?),\ .*", 15-1), "\\1", top_10), 80)) %>%
    mutate(top_10=emoji_to_link(top_10))
  
  topics_terms_levels <- paste0(topics_terms$topic, ": ", topics_terms$top_10)
  
  bios_lda_gamma <- tidy(bios_lda6, matrix = "gamma") %>%
    rowwise() %>%
    mutate(gamma=gamma+runif(1,0,0.0001))
  
<<<<<<< HEAD
  userlookup <- lookup_users(unique(bios_lda_gamma$document))

  fig_dat <- left_join(bios_lda_gamma, userlookup %>% 
                         dplyr::select(user_id, document=screen_name)) %>% 
    dplyr::filter(!is.na(user_id)) %>% 
    dplyr::select(user_id, topic, gamma)
  
  write_tsv(fig_dat, paste0("audiences/paper_data/fig1", letters[panel], ".txt"))
  
=======
>>>>>>> 64cbec55ab53574be1346db0d0b7ae9cce410194
  docs_order <- bios_lda_gamma %>%
    group_by(document) %>%
    arrange(topic, -gamma) %>%
    top_n(1, gamma) %>%
    rename(topic_group = topic) %>%
    dplyr::select(-gamma)
  
  
  # top topic per user
  lda_gammas <- bios_lda_gamma %>%
    ungroup() %>%
    mutate(document=factor(document, levels=docs_order$document)) %>%
    left_join(topics_terms, by="topic") %>%
    mutate(topic=paste0(topic, ": ", top_10)) %>%
    mutate(topic=factor(topic, levels=topics_terms_levels)) %>%
    group_by(document) %>%
    arrange(topic, -gamma) %>%
    top_n(1, gamma) %>%
    rename(account=document)
    
  lda_gammas_count <- bios_lda_gamma %>% 
    group_by(topic) %>% 
    summarise(n=sum(gamma)) %>%
    left_join(topics_terms, by="topic") %>%
    mutate(topic=paste0(topic, ": ", top_10)) %>%
    mutate(topic=factor(topic, levels=topics_terms_levels)) %>%
    ungroup() %>%
    mutate(pct=n/sum(n)) %>%
    dplyr::select(topic, top_10, n, pct)
  
  acad_topics <- bios_lda_td %>%
    group_by(topic) %>%
    top_n(30, beta) %>%
    # mutate(beta=ceiling(beta*100)) %>%
    # uncount(beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term = text_to_emoji(term)) %>%
    mutate(term = gsub("hashtag", "#", term)) %>% 
    # dplyr::select(-beta) %>% 
    # mutate(topic=paste0("t", topic)) %>%
    group_by(topic) %>% 
    summarise(top_10=paste(term, collapse=" ")) %>% 
    ungroup() %>%
    dplyr::filter(grepl("phd|md|dr", top_10)) %>%
    dplyr::filter(grepl("university|institute|universidad|lab|college", top_10)) %>%
    dplyr::filter(grepl("student|estudiante|postdoc|professor|profesor|prof", top_10)) %>%
    mutate(top_10 = gsub("phd|university|uni|professor|research|student|studying|lab|scientist|dr|doctor|candidate|postdoc|fellow|prof|school|grad|institute|director|lecturer|assistant|author|teacher|science|researcher|writer|music|chair|enthusiast|editor|associate|news|passionate", "", top_10)) %>%
    mutate(content_clean=top_10) %>%
    rowwise() %>%
    mutate(score=get_best_match(content_clean, lword_counts, wiki_cat))
  
  topic_ids <- paste0("topic", acad_topics$topic)
  
  plotdat <- bios_lda_gamma %>%
    mutate(topic_lab=paste0("topic", topic)) %>%
    ungroup() %>%
    mutate(document=factor(document, levels=docs_order$document)) %>%
    left_join(topics_terms, by="topic") %>%
    mutate(topic=paste0(topic, ": ", top_10)) %>%
    mutate(topic=ifelse(topic_lab %in% topic_ids, paste0(emoji_to_link("🎓"),  " ", topic), topic)) %>%
    mutate(topic=gsub("\n", "<br>", topic)) %>%
    mutate(topic=factor(topic, levels=unique(.$topic)))

  p <- ggplot(plotdat, aes(x=document, y=gamma, fill=topic, colour=topic))+
    geom_bar(stat="identity", position="stack", size=0.1, key_glyph = draw_key_point)+
    scale_fill_manual(values=cols)+
    scale_colour_manual(values=cols)+
    scale_y_continuous(expand = c(0,0))+
    scale_x_discrete(limits = rev(levels(plotdat$document)), position = "bottom")+ 
    xlab("Account")+
    ylab("Probability")+
    coord_flip()+
    guides(fill=guide_legend(ncol=1, override.aes = list(shape = 16, size=1)))+
    ggtitle(panel_title)+
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.margin=margin(l=-10),
          legend.justification="left",
          # legend.box.background = element_rect(colour = "black"),
          legend.text=element_markdown(size=12, margin = margin(b=2, t=2, l=-5, unit='pt')),
          legend.key = element_rect(fill = "white"),
          # legend.text=element_text(family='fontawesome-webfont'),
          # legend.key.size=unit(1,"point"),
          # legend.key.size = unit(0.5, "cm"),
          # axis.title.x=element_blank(),
          # axis.text.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  p
  
  ggsave(paste0("fig1", letters[panel], "test1.tiff"), device="tiff", dpi=300, width=3.2, height=4)
}

# ggsave("fig1a.test.pdf", device="pdf", dpi=300, width=2, height=4)
# gsave("fig1a.test.eps", device="eps", dpi=300, width=2, height=4)
