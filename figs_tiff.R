sddf3 <- left_join(sddf2, summary_data_df2)

sddf3a <- sddf3 %>% 
  dplyr::select(title, doi, date_posted=added_on, biorxiv_category=categories_trim,
pct_acad_am, pct_acad_aud,
wn_means_02, wn_means_05, wn_means_10, wn_means_20,
n_users, score, cited_by_msm_count, ncites, topic, topic_fraction=pct)


fig4 <- wn_dat %>% dplyr::filter(wn_means=="h=2%") %>% mutate(categories_trim=gsub("-and-.*", "", categories_trim)) %>%
  mutate(categories_trim=as.factor(categories_trim)) %>%
  ggplot(aes(y=h, x=reorder(categories_trim, desc(categories_trim)), colour=categories_trim, label=title))+
  geom_boxplot(outlier.shape=NA, outlier.colour="white", fill=NA)+
  geom_quasirandom(varwidth = TRUE,  groupOnX=TRUE, alpha=0.6)+
  # scale_fill_manual(values=cat_pal)+
  scale_colour_manual(values=cat_pal)+
  scale_y_continuous(expand=c(0.01,0.01))+
  # scale_y_log10(breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))+
  # scale_y_continuous(breaks=seq(0, 0.6, 0.05), limits=c(-0.1,0.6))+
  # facet_wrap(~wn_means, ncol=1, strip.position="right")+
  ylab(paste0("Fraction of users with >", 2, "% white nationalist follower homophily"))+
  theme_classic()+
  theme(#axis.text.x=element_text(angle=-45, hjust=1, size=10),
    legend.position = "none",
    #axis.text.x=element_text(angle=45, hjust=1),
    axis.text.x=element_text(size=18),
    axis.text.y=element_text(size=18),
    axis.ticks.y=element_blank(),
    legend.title=element_blank(),
    # strip.text = element_text(size=18),
    axis.title.x=element_text(size=24),
    axis.title.y=element_blank())+coord_flip()

ggsave("audiences/fig4v1.tiff", fig4, device="tiff", dpi=300, width=5.2, height=7.2, units="in")


s5_all <- wn_dat %>% mutate(categories_trim=gsub("-and-.*", "", categories_trim)) %>%
  mutate(categories_trim=as.factor(categories_trim)) %>%
  ggplot(aes(y=h, x=reorder(categories_trim, desc(categories_trim)), colour=categories_trim, label=title))+
  geom_boxplot(outlier.shape=NA, outlier.colour="white", fill=NA)+
  geom_quasirandom(varwidth = TRUE,  groupOnX=TRUE, alpha=0.6)+
  # scale_fill_manual(values=cat_pal)+
  scale_colour_manual(values=cat_pal)+
  scale_y_continuous(expand=c(0.01,0.01))+
  # scale_x_discrete(limits = rev(levels(categories_trim)))+
  # scale_y_log10(breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))+
  # scale_y_continuous(breaks=seq(0, 0.6, 0.05), limits=c(-0.1,0.6))+
  facet_wrap(~wn_means, nrow=1, strip.position="top", scales="free_x")+
  ylab(paste0("Fraction of users with >", "h", "% white nationalist follower homophily"))+
  theme_classic()+
  theme(#axis.text.x=element_text(angle=-45, hjust=1, size=10),
    legend.position = "none",
    #axis.text.x=element_text(angle=45, hjust=1),
    axis.text.x=element_text(size=18),
    axis.text.y=element_text(size=18),
    axis.ticks.y=element_blank(),
    legend.title=element_blank(),
    strip.text = element_text(size=18),
    axis.title.x=element_text(size=24),
    axis.title.y=element_blank())+coord_flip()

s5_all
ggsave("audiences/figS5.tiff", s5_all, device="tiff", dpi=300, width=7.2, height=5.2, units="in")
