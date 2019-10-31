# helper script for translating Altmetric urls to DOIs

article_urls <- read.csv("audiences/papers_altmetric.txt", col_names=F, sep="\t")

dois <- c()
for (article_full_url in article_urls$X1){
  
  # scrape doi and abstract
  article_id <- gsub(".*.details/", "", article_full_url)
  
  summary_page <- read_html(article_full_url)
  doi <- summary_page %>% 
    html_nodes("div.document-details-table tr:nth-child(3) :nth-child(1)") %>% 
    html_text()
  
  dois <- c(dois, doi[2])
}

write.table(dois, "audiences/papers.txt", quote=F, row.names=F, col.names=F)
