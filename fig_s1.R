summary_data_df_full <- data.frame(title, doi, categories_trim=categories, id_score, pct_acad_am, pct_acad_aud,
                                   wn_means_02, wn_means_05, wn_means_10, wn_means_20, n_users) %>%
  left_join(article_am_df %>% dplyr::select(-title), by=c("doi"))


sd_match <- summary_data_df_full %>% 
  dplyr::filter(grepl("iberian pen|risky|exacerb|birth weight|modified penetrance|genetic meta-analysis|compensation|sequence capture", tolower(title))) %>% 
  dplyr::select(title, journal, pct_acad_am, pct_acad_aud, wn_means_02, n_users) %>% 
  arrange(title, n_users) %>% 
  as_tibble

sd_match$paper <- paste0("paper", rep(seq(1, 8), each=2))

paper_titles <- sd_match %>% dplyr::filter(is.na(journal)) %>% pull(title)

sd_match %>% 
  pivot_longer(cols=c(pct_acad_am, pct_acad_aud, wn_means_02, n_users)) %>% 
  mutate(name=dplyr::recode(name, n_users="Tweets Received", pct_acad_am="fraction academic audience (Altmetric estimate)", pct_acad_aud="fraction academic audience (our estimate)", wn_means_02="fraction of audience with h>2%")) %>% 
  mutate(journal=replace_na(journal, "peer-reviewed")) %>%
  mutate(journal=dplyr::recode(journal, bioRxiv="preprint")) %>%
  mutate(journal=factor(journal, levels=c("preprint", "peer-reviewed"))) %>%
  mutate(paper=dplyr::recode(paper, paper1=paper_titles[1],
                             paper2=paper_titles[2],
                             paper3=paper_titles[3],
                             paper4=paper_titles[4],
                             paper5=paper_titles[5],
                             paper6=paper_titles[6],
                             paper7=paper_titles[7],
                             paper8=paper_titles[8])) %>%
  ggplot(aes(x=str_wrap(paper, width = 30), y=value, fill=journal))+
  geom_bar(stat="identity", position="dodge")+
  #scale_fill_manual(values=c("#486a7e", "#bd2736"))+
  scale_fill_manual(values=c("#bd2736", "#486a7e"))+
  facet_wrap(~name, scales="free_y", ncol=1)+
  theme_classic()+
  theme(axis.text.x=element_text(size=12, angle=75, hjust=1), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_blank(), 
        legend.position="bottom",
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        strip.text.x=element_text(size=12))