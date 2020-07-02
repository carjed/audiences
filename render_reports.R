#-----------------------------------------------------------------------------
# load packages
#-----------------------------------------------------------------------------
cat("\n=== running setup ===\n")
library(RCurl)
library(rtweet)
library(tweetscores)
library(knitr)
library(markdown)
library(rmarkdown)
library(rAltmetric)
library(rvest)
library(rcrossref)
library(crevents)
library(tidyverse)
library(yaml)
library(anytime)
library(here)
library(jsonlite)
library(stringr)
library(text2vec)
library(tidytext)
library(tm)
library(umap)
library(httr)

#-----------------------------------------------------------------------------
# Setup options 
#-----------------------------------------------------------------------------
# load history from crashed session
# loadhistory("~/.rstudio/history_database")

options(timeout = 600)

# set relative path for calling external files from within this script
# datadir <- dirname(sys.frame(1)$ofile)
datadir <- paste0(getwd(), "/audiences")

# Twitter oauth keys, email, other private data (e.g., hf_path)
keys <- yaml.load_file(paste0(datadir, "/_config.yaml"))

hf_path <- keys$hf_path

outdir <- paste0(datadir, "/content/reports")
dir.create(outdir, recursive=TRUE, showWarnings = FALSE)

# handles of training data 
td_handles <- read.table("~/td_handles.txt") %>% pull

# biorxiv categories, excluding scicomm and clinical trials
categories_trim <- c("animal-behavior-and-cognition", 
                     "biochemistry", "bioengineering", "bioinformatics",
                     "biophysics", "cancer-biology", "cell-biology", "clinical-trials",
                     "developmental-biology", "ecology",
                     "epidemiology", "evolutionary-biology", 
                     "genetics", "genomics", "immunology", "microbiology", 
                     "molecular-biology", "neuroscience", "paleontology",
                     "pathology", "pharmacology-and-toxicology", "physiology", "plant-biology",
                     "scientific-communication-and-education",
                     "synthetic-biology", "systems-biology", "zoology")

# biorxiv categories matched to wiki entries
categories_wiki <- gsub("-", "_", c("ethology",
                                    "biochemistry", "bioengineering", "bioinformatics", 
                                    "biophysics", "oncology", "cell-biology", "clinical-trial",
                                    "developmental-biology", "ecology",
                                    "epidemiology", "evolutionary-biology",
                                    "genetics", "genomics", "immunology", "microbiology",
                                    "molecular-biology", "neuroscience", "paleontology",
                                    "pathology", "pharmacology", "physiology", "botany",
                                    "science-communication",
                                    "synthetic-biology", "systems-biology", "zoology"))

cat_match <- data.frame(categories_trim, categories_wiki) %>% arrange(categories_wiki)

# define stopwords
custom_stopwords <- c("https", "http", "tco", "gmailcom", "views", "love", "lover", "tweets",
                      "rts", "follow", "twitter", "endorsement", "fan", "james", "michael",
                      "andrew", "ryan", "chris", "matt", "och", "rt", "opinions", "paul",
                      "juan", "carlos", "luis", "jose", "maria", "jorge", "alex",
                      "endorsements", "account", "life", "john", "david", "social", "retweets",
                      stopwords(kind="en"), stopwords(kind="danish"), stopwords(kind="dutch"), 
                      stopwords(kind="finnish"), stopwords(kind="french"), stopwords(kind="german"),
                      stopwords(kind="hungarian"), stopwords(kind="italian"), stopwords(kind="norwegian"),
                      stopwords(kind="portuguese"), stopwords(kind="russian"), stopwords(kind="spanish"),
                      stopwords(kind="swedish"))

#-----------------------------------------------------------------------------
# Setup API keys
# To use the Twitter API, sign up for a developer account and obtain the 
# necessary API tokens, following this guide:
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
# For security, these are stored in a separate `_config.yaml` file.
#-----------------------------------------------------------------------------

# oauth for tweetscores functions

my_oauth <- list(consumer_key = keys$consumer_key,
                 consumer_secret = keys$consumer_secret,
                 access_token = keys$access_token,
                 access_token_secret = keys$access_secret)

my_oauth2 <- list(consumer_key = keys$consumer_key_dev,
                 consumer_secret = keys$consumer_secret_dev,
                 access_token = keys$access_token_dev,
                 access_token_secret = keys$access_secret_dev)

# my_oauth3 <- list(consumer_key = keys$consumer_key_stage,
#                  consumer_secret = keys$consumer_secret_stage,
#                  access_token = keys$access_token_stage,
#                  access_token_secret = keys$access_secret_stage)


# set token for rtweet
token1 <- create_token(
  app = keys$app_name,
  consumer_key = keys$consumer_key,
  consumer_secret = keys$consumer_secret,
  access_token=keys$access_token,
  access_secret=keys$access_secret,
  set_renv = FALSE)

token2 <- create_token(
  app = keys$app_name_dev,
  consumer_key = keys$consumer_key_dev,
  consumer_secret = keys$consumer_secret_dev,
  access_token=keys$access_token_dev,
  access_secret=keys$access_secret_dev,
  set_renv = FALSE)

cat("done\n")
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#-----------------------------------------------------------------------------
# get bios of up to 10k followers
#-----------------------------------------------------------------------------
# testing with timelines instead of bios
# tmls <- get_timelines(plotdat$account[1:4], n = 3200)

get_follower_bios <- function(followers, token){
  f5k <- unlist(followers)
  f5k <- head(f5k, 10000)
  
  lookup_users(f5k, token=token)
}


#-----------------------------------------------------------------------------
# function counts number of test account's followers in each of 
# training accounts' follower groups
#-----------------------------------------------------------------------------
match_followers <- function(a1, test_df=training_data_full){
  
  a1 <- unlist(a1)
  a2_list <- test_df$followers
  
  match_counts <- lapply(a2_list, function(x){sum(unlist(x) %in% a1)/length(a1)})
  names(match_counts) <- test_df$account
  
  return(data.frame(match_counts))
}


#-----------------------------------------------------------------------------
# define emoji lookup functions
# https://stackoverflow.com/questions/31828218/replace-values-in-character-vector-using-lookup-table-in-r
#-----------------------------------------------------------------------------
emoji_to_text <- function(x) Reduce(function(x,r) gsub(lookup$Native[r],lookup$Desc_Sub[r],x,fixed=T),seq_len(nrow(lookup)),x)
text_to_emoji <- function(x) Reduce(function(x,r) gsub(lookup$Desc_Sub2[r],lookup$Native[r],x,fixed=T),seq_len(nrow(lookup)),x)


#-----------------------------------------------------------------------------
# functions for processing Wikipedia data
#-----------------------------------------------------------------------------
getContent <- function(article_title){
  
  page_url <- paste0("https://en.wikipedia.org/w/api.php", 
                     "?action=query&redirects=1&prop=extracts&explaintext&rvsection=0&format=json&titles=", article_title)
  page_json <- try(fromJSON(page_url))
  
  if(!inherits(page_json, "try-error")){
    # field_title <- data.frame(page_json$query)[2]
    field_data <- data.frame(page_json$query, stringsAsFactors=F)
    # names(field_data)[4] <- "content"
    
    content <- field_data[,names(field_data)[grepl("extract", names(field_data))]]
    
    return(content)
  } else {
    return("")
  }

}


stripTags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


prep_fun = function(x) {
  x %>%
    # make text lower case
    str_to_lower %>%
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>%
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}


get_best_match <- function(keywords, lword_counts, target_category){
  
  keywords_split <- unlist(strsplit(keywords, " "))
  
  score <- lword_counts %>%
    dplyr::filter(tolower(article_title)==target_category) %>%
    mutate(cc=strsplit(content_clean, " ")) %>%
    rowwise() %>%
    mutate(score=sum(keywords_split %in% unlist(cc[1:100]))) %>%
    dplyr::select(score) %>%
    as.numeric()
  
  return(score)
}

#-----------------------------------------------------------------------------
# function for getting cosine similarity matrix between two corpuses of documents
#-----------------------------------------------------------------------------
get_sim_matrix <- function(doc1, doc2){
  it1 <- itoken(doc1$content_clean, progressbar = FALSE)
  it2 <- itoken(doc2$content_clean, progressbar = FALSE)
  
  it <- itoken(c(doc1$content_clean, doc2$content_clean), progressbar = FALSE)
  
  v <- create_vocabulary(it) #%>% 
    # prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 1)
  
  vectorizer <- vocab_vectorizer(v)
  dtm1 <- create_dtm(it1, vectorizer)
  dtm2 <- create_dtm(it2, vectorizer)
  
  # tfidf = TfIdf$new()
  # dtm_tfidf1 = fit_transform(dtm1, tfidf)
  # 
  # tfidf = TfIdf$new()
  # dtm_tfidf2 = fit_transform(dtm2, tfidf)
  
  sim_matrix <- sim2(dtm1, dtm2, method = "cosine", norm = "l2")
  # sim_matrix <- sim2(dtm_tfidf1, dtm_tfidf2, method = "cosine", norm = "l2")
  
  sim_scores <- as_tibble(as.matrix(sim_matrix))
  return(sim_scores)
}

#-----------------------------------------------------------------------------
# lookup discipline-discipline similarity
#-----------------------------------------------------------------------------
dd_sim <- function(topic, category, sim_scores){
  out <- sim_scores %>%
    dplyr::filter(title==topic) %>%
    dplyr::select(category) %>%
    as.numeric()
  
  return(out)
}

#-----------------------------------------------------------------------------
# get list of follower files from generated reports
#-----------------------------------------------------------------------------
update_follower_files <- function(datadir){
  follower_files_details <- file.info(
    paste0(datadir, "/article_data/", 
           list.files(path=paste0(datadir, "/article_data"),
                      pattern="altmetric_data_")))
  follower_files_details <- rownames_to_column(follower_files_details) %>%
    arrange(desc(mtime))
  
  chk_files <- follower_files_details$rowname
  
  # return only files that haven't been added before
  return(chk_files)
}


get_follower_cache <- function(high_followers, files_to_scan, files_scanned){
  
  # get only new files
  follower_files <- files_to_scan[!files_to_scan %in% files_scanned]
  
  if (length(follower_files) > 0) {
    high_followers_new <- follower_files %>%
      map_dfr(readRDS) %>%
      rowwise() %>% 
      mutate(nfollowers = length(followers)) %>% 
      arrange(desc(nfollowers)) %>% 
      dplyr::filter(nfollowers>100)
  } else {
    high_followers_new <- tibble(account=character(), followers=list())
  }
  
  cat(paste0("loaded data for ", nrow(high_followers_new), " additional users\n"))
  
  high_followers_out <- bind_rows(high_followers, high_followers_new) %>%
    rowwise() %>% 
    mutate(nfollowers = length(followers)) %>% 
    group_by(account) %>% 
    arrange(desc(nfollowers)) %>% 
    slice(1L)
  
  cat(paste0("High-follower cache updated: ", nrow(high_followers_out), " total users\n"))
  
  return(high_followers_out)
}

#-----------------------------------------------------------------------------
# if crossref data not found, scrape from Altmetric with rvest
#-----------------------------------------------------------------------------
events_from_altmetric <- function(article_full_url){
  summary_page <- read_html(article_full_url)
  twitter_url <- paste0(article_full_url, "/twitter")
  twitter_page <- read_html(twitter_url)
  
  # number of pages is the ceiling of total tweets/100
  totals <- twitter_page %>% html_nodes("div.text strong") %>% html_text()
  npages <- ceiling(as.integer(totals[1])/100)
  
  # loop through pages of tweets on altmetric to get handles of tweeting users
  events <- data.frame()
  for(page in 1:npages){
    # url <- paste0(article_base_url, id, "/twitter/page:", page)
    page_url <- paste0(twitter_url, "/page:", page)
    page <- read_html(page_url)
    
    names <- gsub("@", "", html_nodes(page, "div.handle") %>% html_text())
    
    status <- gsub(".*tweet_id=", "", html_nodes(page, "a.favorite") %>% 
                     html_attr("href"))
    
    timestamps <- html_nodes(page, "time") %>% html_attrs() %>% unlist()
    
    events <- bind_rows(events, data.frame(names, timestamps, status))
  }
  
  return(events)
}


#-----------------------------------------------------------------------------
# scrape follower info (or load from cached data) and cache to disk
#
# due to Twitter API limits, this will take at least N minutes, 
# where N is the number of unique users that have tweeted about an article
#
# users with >5,000 followers will require multiple API calls to scrape their
# full follower lists, so a user with 75,000 followers will take 
# the same amount of time to process as 15 users with <5,000 followers each 
# (~15 minutes)
#-----------------------------------------------------------------------------
compile_follower_data <- function(datadir, article_id, user_data, high_followers){
  # load cached full data (for data collected before chunk caching implemented)
  follower_lists_full_fh <- paste0(datadir, 
                                   "/article_data/altmetric_data_full_", article_id, ".rds")
  
  if(file.exists(follower_lists_full_fh)){
    follower_lists_full <- readRDS(follower_lists_full_fh)
  } else {
    
    # load cached chunks
    chunk_files <- file.info(
             list.files(path=paste0(datadir, "/article_data"), 
                        pattern=paste0("altmetric_data_[0-9]+_", article_id),
                        full.names = TRUE))
    chunk_files <- rownames_to_column(chunk_files) %>%
      arrange(mtime)
    files <- chunk_files$rowname
    
    if(length(files) != 0){
      follower_lists_cache <- files %>%
        map_dfr(readRDS)
      
      cat(paste0("loaded data for ", length(unique(follower_lists_cache$account)), " users\n"))
          
    } else {
      follower_lists_cache <- tibble(account=character(), followers=list())
    }
    follower_lists_full <- tibble(account=character(), followers=list())
    
    
    # scrape new data if out of date or partially complete
    if(length(unique(follower_lists_cache$account))/nrow(user_data)<0.95){
      
      # get follower metadata from Twitter API
      # sleep interval--if more than 15 API calls will be required,
      # use one call per minute to minimize weird timeout issues
      fc_mod <- ceiling(user_data$followers_count/5000)
      sleep <- ifelse(sum(fc_mod)>15, 60, 0)
      
      follower_list_sub <- tibble(account=character(), followers=list())
      
      i <- 1
      j <- 1
      for(user in unique(user_data$screen_name)){
        
        cat(paste0(user, " (", i , "/", nrow(user_data), ")..."))
        if(user %in% c("RZRPhoenix1", "CalifQuail1969", "xkrxoqd")){
          next
        }
        
        # pull from cache if user exists in high-follower database
        if(user %in% high_followers$account){
          
          cat("cached in high-follower list \n")
          follower_list_user <- high_followers %>%
            dplyr::filter(account==user)
          
        } else if(user %in% follower_lists_cache$account){
          
          cat("cached in previous scrape \n")
          follower_list_user <- follower_lists_cache %>%
            dplyr::filter(account==user)
          
        } else {
          
          cat("new\n")
          
          if(j %% 2 == 1){
            use_oauth = my_oauth
          } else {
            use_oauth = my_oauth2
          }
          
          follower_list_user <- user_data %>%
            dplyr::filter(screen_name == user) %>%
            dplyr::select(account = screen_name) %>% #head
            mutate(followers = getFollowers(screen_name = account, 
                                            # oauth = my_oauth, 
                                            oauth = use_oauth, 
                                            sleep = 20) %>% 
                     data.frame %>% 
                     as.list)
          j <- j+1
        }  
        follower_list_sub <- bind_rows(list(follower_list_sub, follower_list_user))
        
        # cache to disk every 10 users
        if(i %% 50 == 0 | i==nrow(user_data)){
          cat("caching to disk\n")
          follower_list_sub_fh <- paste0(datadir, 
                                         "/article_data/altmetric_data_", 
                                         as.character(i), "_", article_id, ".rds")
          saveRDS(follower_list_sub, follower_list_sub_fh)
          follower_list_sub <- tibble(account=character(), followers=list())
        }
        
        follower_lists_full <- bind_rows(list(follower_lists_full, follower_list_user))
        i <- i+1
      }
    } else {
      follower_lists_full <- follower_lists_cache
    }
  }
  
  return(follower_lists_full)
}

#-----------------------------------------------------------------------------
# scrape and cache follower bios
#-----------------------------------------------------------------------------
compile_follower_bios <- function(datadir, article_id, follower_lists_full){
  follower_bios_fh <- paste0(datadir, "/article_data/follower_bios_", article_id, ".rds")
  
  if(file.exists(follower_bios_fh)){
    out_df <- readRDS(follower_bios_fh)
  } else {
    
    # load cached chunks
    chunk_files <- file.info(
             list.files(path=paste0(datadir, "/article_data"), 
                        pattern=paste0("follower_bios_[0-9]+_", article_id),
                        full.names = TRUE))
    chunk_files <- rownames_to_column(chunk_files) %>%
      arrange(mtime)

    files <- chunk_files$rowname
    
    
    if(length(files) != 0){
      out_df <- files %>%
        map_dfr(readRDS)
      
      cat(paste0("loaded follower bios for ", length(unique(out_df$account)), " users\n"))
      
    } else {
      out_df <- tibble(account=character(), bios=character())
    }
    
    # skip individual bio checks if we have follower bios for at least 90% of users
    tot_users <- length(unique(follower_lists_full$account))
    if(length(unique(out_df$account))/tot_users < 0.95){
      
      # if document term matrix has been generated & cached from a previous run
      # and data for new users will be added, remove the file
      bios_dtm_fh <- paste0(datadir, "/article_data/bios_dtm_", article_id, ".rds")
      if(file.exists(bios_dtm_fh)){
        file.remove(bios_dtm_fh)
        cat("removed cached document term matrix file\n")
      }
      
      out_df_sub <- tibble(account=character(), bios=character())
      
      i <- 1
      j <- 1
      for(user in unique(follower_lists_full$account)){
        
        cat(paste0(user, " (", i , "/", tot_users, ")..."))
        
        if(user %in% out_df$account){
          
          cat("cached in previous scrape \n")
          acct_follower_bios <- out_df %>%
            dplyr::filter(account==user) #%>%
            # distinct(account, .keep_all = T)
          
          out_df_sub <- bind_rows(list(out_df_sub, acct_follower_bios))
        } else {
          cat("new \n")
          
          if(j %% 2 == 1){
            token <- token1
          } else {
            token <- token2
          }
          
          
          test_user <- follower_lists_full %>% 
            dplyr::filter(account==user) %>%
            distinct(account, .keep_all = T)
          
          if(length(unlist(test_user$followers)) > 5){
            bios <- try(get_follower_bios(followers=test_user$followers, token=token))
            
            # check that bios df has at least 80 columns
            if(!inherits(bios, "try-error") & length(unique(names(bios))) > 80){
              acct_follower_bios <- data.frame(account=test_user$account, bios)
              
              out_df_sub <- bind_rows(list(out_df_sub, acct_follower_bios))
              out_df <- bind_rows(list(out_df, acct_follower_bios))
            } else {
              cat(paste0("Encountered error for user—results will not be included\n"))
            }
            
            j <- j+1
            Sys.sleep(2)
          }
        }
        # out_df_sub <- bind_rows(list(out_df_sub, acct_follower_bios))
        
        # cache to disk every 50 users
        if(i %% 50 == 0 | i == tot_users){
          cat("caching to disk\n")
          follower_bios_sub_fh <- paste0(datadir, 
                                         "/article_data/follower_bios_", 
                                         as.character(i), "_", article_id, ".rds")
          saveRDS(out_df_sub, follower_bios_sub_fh)
          out_df_sub <- tibble(account=character(), bios=character())
        }
        
        
        i <- i+1
      }
    }
  }
  
  return(out_df)
}

#-----------------------------------------------------------------------------
# function for rendering report
#-----------------------------------------------------------------------------
run_report <- function(doi, outdir, datadir,
                       article_df, cr_data,
                       user_data, events,
                       follower_lists_full, 
                       follower_bios_full,
                       category) {
  
  article_id <- article_df$altmetric_id
  
  title <- gsub("\"|/|:|\\$", "", cr_data$title)
  nb_prefix <- paste0(gsub(" ", "_", title), "_", article_id)
  

  nb_file <- paste0(nb_prefix, ".html")
  nb_title <- paste0(title, ", ",
                     article_df$journal, ", ", cr_data$created)
  
  if(grepl("10.1101", doi)){
    nb_desc <- gsub("</*jats:[A-z]*>", "", cr_data$abstract)
  } else {
    nb_desc <- html_nodes(page, css="#Abs1-section :nth-child(1)") %>% html_text("p") %>% tail(1) 
  }
  
  nb_desc <- gsub("\"|/|:", "", nb_desc)
  nb_desc <- gsub("\n", "", nb_desc)
  
  # get Altmetric URL, specifying journal-specific subdomain if needed
  if(grepl("10.1101", doi)){
    subdomain <- "biorxiv"
  } else if(grepl("10.1016", doi)){
    subdomain <- "cell"
  } else {
    subdomain <- "www"
  }
  
  article_full_url <- paste0("https://", 
                             subdomain, ".altmetric.com/details/", article_id)
  
  n_analyzed <- nrow(follower_lists_full)
  count_group <- ifelse(n_analyzed < 100, "0-100-users",
                        ifelse(n_analyzed < 200, "100-200-users",
                               ifelse(n_analyzed < 500, "200-500-users", "500+-users")))
  
  cat("\n=== rendering report ===\n")
  rmarkdown::render(paste0(datadir, "/report_template.rmd"),
                    output_file =  nb_file, 
                    output_dir = outdir,
                    params = list(title=nb_title, 
                                  abstract=nb_desc,
                                  datadir=datadir,
                                  article_id=article_id,
                                  doi=doi))
  
  # update HTML to render with Hugo
  fileConn <- file(paste0(outdir, "/", nb_file), 'r+')
  Lines <- readLines(fileConn)
  
  # replace links to report-specific js libraries to common directory
  Lines2 <- gsub(paste0(nb_prefix, "_files"), "/reports/src", Lines)
  
  writeLines("---", fileConn)
  writeLines(paste0("title: ", nb_title), fileConn)
  writeLines("aliases:", fileConn)
  writeLines(paste0("    - /reports/", doi, "/"), fileConn)
  writeLines(paste0("date: ", cr_data$created), fileConn)
  writeLines(paste0("description: ", nb_desc), fileConn)
  writeLines("tags:", fileConn)
  writeLines(paste0("    - ", tolower(article_df$journal)), fileConn)
  writeLines(paste0("    - ", category), fileConn)
  writeLines(paste0("    - ", count_group), fileConn)
  writeLines(paste0("    - ", substr(cr_data$created, 1, 4)), fileConn)
  writeLines("---", fileConn)
  writeLines(Lines2, fileConn)
  close(fileConn)
  
  # remove article-specific js libraries
  unlink(paste0(outdir, "/", nb_prefix, "_files"), recursive = TRUE)  
  
  cat("done\n")
}


#-----------------------------------------------------------------------------
# summary report
#-----------------------------------------------------------------------------
run_summary <- function(datadir){
  rmarkdown::render(paste0(datadir, "/summary_template.rmd"),
                    output_file =  "_index.html",
                    output_dir = paste0(datadir, "/content/"),
                    params = list(title="Audiences",
                                  datadir=datadir))
}


#-----------------------------------------------------------------------------
# get emoji lookup table
#-----------------------------------------------------------------------------
cat("\n=== loading emoji table ===\n")

if(!exists("lookup")){
  source(paste0(datadir, "/R/scrape_emoticons.R"))
  
  lookup <- alltogether %>% 
    # ensure textualized emoji is separated by space
    mutate(Desc_Sub=tolower(paste0(" emoji", gsub("[+]|-| ", "", Description), " "))) %>% 
    # strip spaces for proper text -> emoji matching 
    mutate(Desc_Sub2=gsub(" ", "", Desc_Sub)) 
}

cat("done\n")


#-----------------------------------------------------------------------------
# load Wikipedia article data
#-----------------------------------------------------------------------------
cat("\n=== loading academic field descriptions ===\n")

fields_page <- read_html("https://en.wikipedia.org/wiki/Outline_of_academic_disciplines")

# page <- read_html("https://en.wikipedia.org/wiki/Index_of_branches_of_science")

links_fh <- paste0(datadir, "/training_data/links.rds")

if(file.exists(links_fh)){
  links <- readRDS(links_fh)
} else {
  
  # links <- html_nodes(fields_page, ".column-width > ul > li > a") %>%
    links <- html_nodes(fields_page, "ul > li > a") %>%
      html_attr("href") %>%
      data.frame() %>% 
      dplyr::filter(grepl("^/wiki/", .)) %>% 
      dplyr::filter(!grepl(":|\\(|Main|Outline|List", .)) %>%
      dplyr::filter(!grepl("#", .)) %>%
      dplyr::filter(!grepl("Outline", .)) %>%
      mutate(url=paste0("https://en.wikipedia.org", .)) %>%
      mutate(article_title=gsub("/wiki/", "", .)) %>% 
      rowwise() %>%
      mutate(content = getContent(article_title)) %>%
      ungroup() %>%
      distinct(content, .keep_all = TRUE) %>%
      mutate(content = gsub("See Also.*", "", content)) %>%
      mutate(content_clean = gsub("\n", " ", content)) %>%
      mutate(content_clean = gsub('[[:digit:]]+', '', content_clean)) %>%
      mutate(content_clean = gsub("see also.*", "", content_clean)) %>%
      mutate(content_clean = prep_fun(content_clean)) 
    
    saveRDS(links, links_fh)
    
}

skipped_fields <- c("Geobiology")

links_tokenized <- head(links, -12) %>%
  dplyr::filter(!(article_title %in% skipped_fields)) %>%
  dplyr::filter(!grepl("studies", article_title)) %>%
  mutate(content_clean = gsub("see also.*", "", content_clean)) %>%
  dplyr::select(-content) %>%
  mutate(nchar=nchar(content_clean)) %>%
  dplyr::filter(nchar>=5000) %>%
  unnest_tokens(word, content_clean)

# apply default stopword list and count frequencies
lword_counts <- links_tokenized %>%
  count(article_title, word, sort = TRUE) %>%
  anti_join(stop_words) %>%
  dplyr::filter(!(word %in% custom_stopwords)) %>%
  group_by(article_title) %>%
  top_n(100, n) %>%
  summarise(content_clean=paste0(word, sep= " ", collapse=""))

map_scores <- get_sim_matrix(lword_counts, lword_counts)
ms_pca <- prcomp(map_scores, center=T, scale=T)
ms_umap <- umap(ms_pca$x, n_neighbors=30)

cat("done\n")

# categories_wiki <- categories_wiki[order(categories_wiki)]

# get wiki content specific to biorxiv categories
cat("\n=== loading field descriptions for biorxiv categories ===\n")

biorxiv_content_fh <- paste0(datadir, "/training_data/biorxiv_content.rds")
if(file.exists(biorxiv_content_fh)){
  biorxiv_content <- readRDS(biorxiv_content_fh)
} else {
  
  biorxiv_content <- tibble(article_title=character(),
                            url=character(),
                            content=character(),
                            content_clean=character())
  for(category in categories_wiki){
    article_content = try(getContent(category))
    cat(paste0(category, "\n"))
    while(inherits("try-error", article_content)){
      article_content = try(getContent(category))
    }
    
    biorxiv_row <- data.frame(article_title=category, stringsAsFactors = F) %>%
      mutate(url=paste0("https://en.wikipedia.org/wiki/", article_title)) %>%
      mutate(content=article_content) %>%
      mutate(content_clean = gsub("\n", " ", content)) %>%
      mutate(content_clean = gsub('[[:digit:]]+', '', content_clean)) %>%
      mutate(content_clean = gsub("see also.*", "", content_clean)) %>%
      mutate(content_clean = prep_fun(content_clean)) 
    
    biorxiv_content <- bind_rows(biorxiv_content, biorxiv_row)
    Sys.sleep(1)
  }
  
  links_tokenized_b <- biorxiv_content %>%
    dplyr::select(-content) %>%
    unnest_tokens(word, content_clean)
  
  # apply default stopword list and count frequencies
  biorxiv_content <- links_tokenized_b %>%
    count(article_title, word, sort = TRUE) %>%
    anti_join(stop_words) %>%
    dplyr::filter(!(word %in% custom_stopwords)) %>%
    group_by(article_title) %>%
    top_n(100, n) %>%
    summarise(content_clean=paste0(word, sep= " ", collapse=""))
  
  
  saveRDS(biorxiv_content, biorxiv_content_fh)
  
}

biorxiv_sim_scores <- get_sim_matrix(lword_counts, biorxiv_content)
names(biorxiv_sim_scores) <- cat_match$categories_trim
biorxiv_sim_scores$title <- lword_counts$article_title
biorxiv_sim_scores <- biorxiv_sim_scores %>% unique()

cat("done\n")


#-----------------------------------------------------------------------------
# read ref panel
#-----------------------------------------------------------------------------
cat("\n=== loading training data ===\n")

if(!exists("training_data_full")){

  training_data_full_fh <- paste0(datadir, "/training_data/training_data_full2.rds")
  
  force_update_training <- FALSE
  if(!file.exists(training_data_full_fh) | force_update_training){
    training_user_data <- lookup_users(td_handles) %>%
      mutate(account=screen_name) %>%
      left_join(training_data, "account")
    
    fc_mod <- ceiling(training_user_data$followers_count/5000)
    
    sleep <- ifelse(sum(fc_mod)>15, 60, 0)
    
    # get followers for accounts in training data
    training_data_full <- training_user_data %>% #head
      rowwise %>% 
      mutate(followers=getFollowers(screen_name=account, oauth=my_oauth, sleep=sleep) %>% 
               data.frame %>% 
               as.list)
    
    saveRDS(training_data_full, training_data_full_fh)
    
  } else {
    training_data_full <- readRDS(training_data_full_fh)
  }
}

cat("done\n")


#-----------------------------------------------------------------------------
# Cache data frame of high-follower accounts, scraped from previous analyses
#-----------------------------------------------------------------------------
cat("\n=== caching high-follower accounts ===\n")

if(!exists("high_followers")){
  high_fol_details <- file.info(
    list.files(path=hf_path, 
               pattern=paste0("high_followers"),
               full.names = TRUE))
  
  high_fol_details <- rownames_to_column(high_fol_details) %>%
    arrange(mtime)
  
  high_fol_files <- high_fol_details$rowname
  
  if(length(high_fol_files) != 0){
    high_followers_all <- high_fol_files %>%
      map_dfr(readRDS)
    
    cat(paste0("loaded data for ", nrow(high_followers_all), " users from high-follower cache\n"))
    
  }
  
  files_to_scan <- update_follower_files(datadir)
  
  high_followers <- files_to_scan %>%
    map_dfr(readRDS) %>%
    rowwise() %>% 
    mutate(nfollowers = length(followers)) %>% 
    arrange(desc(nfollowers)) %>% 
    dplyr::filter(nfollowers>100) %>% 
    dplyr::select(-nfollowers) %>%
    dplyr::filter(!(account %in% high_followers_all$account))
  
  cat(paste0("loaded data for ", nrow(high_followers), " additional users from other reports\n"))
  
  high_followers <- bind_rows(high_followers_all, high_followers) %>%
    rowwise() %>% 
    mutate(nfollowers = length(followers)) %>% 
    group_by(account) %>% 
    arrange(desc(nfollowers)) %>% 
    slice(1L)
  
  files_scanned <- files_to_scan
}

cat(paste0("loaded data for ", nrow(high_followers), " total users\n"))


#-----------------------------------------------------------------------------
# Read list of article DOIs and Altmetric URLs
# - in the future, this will be purely DOI-based
# - can also pull in list of popular bioRxiv papers using the Rxivist API
#-----------------------------------------------------------------------------
check_file <- FALSE
report <- TRUE
# skip <- 535
skip <- 4
metric <- "twitter"
# metric <- "downloads"
select_dois <- c("10.1101/509315")

# group of DOIs to analyze
# doi_group <- "select"
# doi_group <- "biorxiv"
# doi_group <- "nature"
doi_group <- "covid19"

# list of DOIs to skip because they lack enough tweets or are missing metadata
banned_dois <- c("10.1101/397067", "10.1101/501494", "10.1101/066803", "10.1101/461004",
                 "10.1101/460899", "10.1101/234799", "10.1101/233007", "10.1101/556019", "10.1101/718395") #?)

if (doi_group=="select") {
  dois <- scan(paste0(datadir, "/papers.txt"), what="", sep="\n")
  
  for (doi in dois) {
    run_report(doi, outdir, check_file)
  }
  
} else if (doi_group=="biorxiv") {

  cat(paste0("\n=== scraping top preprints from rxivist ===\n"))

  # old version--scrape using rvest
  rvest_scrape <- FALSE
  if (rvest_scrape) {
    cat("(rvest)...")
    page_url <- paste0("https://rxivist.org/?q=&metric=twitter&category=", category, 
                       "&timeframe=alltime&page_size=20&view=standard")

    page <- read_html(page_url)

    dois_df <- html_nodes(page, ".btn-sm.btn-altcolor") %>%
      html_attr("href") %>%
      data.frame() %>%
      dplyr::rename(doi=".") %>%
      dplyr::filter(grepl("doi", doi)) %>%
      dplyr::mutate(doi=gsub("https://doi.org/", "", doi))
    
  } else {
    # new version--scrape using rxivist API
    cat("(API)...")
    
    # api_url1 <- paste0("https://api.rxivist.org/v1/papers?metric=", metric, "&page_size=250&timeframe=alltime&page=", 
    # api_url2 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=1"
    # api_url3 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=2"
    # api_url4 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=3"
    # api_url5 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=4"
    # api_url6 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=5"
    # api_url7 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=6"
    # api_url8 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=7"

    
    api_urls <- paste0("https://api.rxivist.org/v1/papers?metric=", metric, "&page_size=250&timeframe=alltime&page=", seq(0,10))
    
    json_data <- lapply(api_urls, fromJSON)
    
    dois_df <- bind_rows(lapply(json_data, function(x){data.frame(x$results)})) %>%
      as_tibble() %>%
      dplyr::filter(!doi %in% banned_dois)
    
    if(length(select_dois) > 0){
      dois_df <- dois_df %>%
        dplyr::filter(doi %in% select_dois)
    }
    
    # # api_url1 <- "https://api.rxivist.org/v1/papers?metric=downloads&page_size=250&timeframe=alltime"
    # # api_url2 <- "https://api.rxivist.org/v1/papers?metric=downloads&page_size=250&timeframe=alltime&page=1"
    # # api_url3 <- "https://api.rxivist.org/v1/papers?metric=downloads&page_size=250&timeframe=alltime&page=2"
    # # api_url4 <- "https://api.rxivist.org/v1/papers?metric=downloads&page_size=250&timeframe=alltime&page=3"
    # 
    # 
    # cat_json1 <- fromJSON(api_url5)
    # cat_json2 <- fromJSON(api_url6)
    # cat_json3 <- fromJSON(api_url7)
    # cat_json4 <- fromJSON(api_url8)
    # dois_df <- bind_rows(data.frame(cat_json1$results),
    #                      data.frame(cat_json2$results),
    #                      data.frame(cat_json3$results),
    #                      data.frame(cat_json4$results)) %>%
    #   # dplyr::filter(metric>=70 & metric <1500) %>% 
    #   as_tibble() %>%
    #   dplyr::filter(!doi %in% banned_dois)
  }

  dois <- dois_df$doi
  cat("done\n")
  
  # for (i in (1+skip):nrow(dois_df)) {
  for (i in skip+1:nrow(dois_df)) {
    
    if(i==-1){
      doi_it <- "10.1126/science.aat7693"
      doi <- doi_it
      category <- "genetics"
    } else {
      paper_it <- dois_df[i,]
      article_title <- paper_it$title
      doi_it <- paper_it$doi
      doi <- doi_it
      category <- paper_it$category
    }
    
    cat(paste0("\n===", article_title, " ", doi, " ===\n"))

    cat("\n=== caching high-follower accounts ===\n")
    
    files_to_scan <- update_follower_files(datadir)
    high_followers <- get_follower_cache(high_followers, files_to_scan, files_scanned)
    files_scanned <- files_to_scan
    
    cat(paste0("done. ", length(files_scanned), " additional files scanned for high-follower users.\n"))

    
    # cat(paste0("\n=== Scraping data for '", paper_it$title, "' (", doi_it, ") ===\n")) 
    
    
    cat("\n=== getting altmetric metadata ===\n")
    
    article_am <- try(altmetrics(doi = doi))
    
    if(inherits(article_am, "try-error")){
      cat(paste0(doi, ": not indexed by Altmetric\n"))
      next
    }
    
    article_df <- altmetric_data(article_am)
    
    if(is.null(article_df$cited_by_tweeters_count)){
      cat(paste0(doi, ": not enough tweets indexed\n"))
      next
    }
    
    article_id <- article_df$altmetric_id
    
    if(i==0){
      subdomain <- "www"
    } else {
      subdomain <- "biorxiv"
    }
    article_full_url <- paste0("https://", subdomain, ".altmetric.com/details/", article_id)
    
    cat("done\n")
    
    
    cat("\n=== getting paper metadata from crossref ===\n")
    
    cr_data <- cr_works(doi = doi)$data
    
    title <- gsub("\"|/|:|\\$", "", cr_data$title)
    nb_prefix <- paste0(gsub(" ", "_", title), "_", article_id)
    
    nb_file <- paste0(nb_prefix, ".html")
    nb_title <- paste0(title, ", ",
                       article_df$journal, ", ", cr_data$created)
    
    cat("done\n")
    
    
    if(file.exists(paste0(outdir, "/", nb_file)) & check_file){
      cat(paste0("report for ", nb_file, " already exists\n"))
      next
    }
    
    
    cat("\n=== querying event data ===\n")
    
    method <- "altmetric"
    # method <- "crossref"
    
    if (method == "crossref"){
      cr_event_url <- paste0("https://api.eventdata.crossref.org/v1/",
                             "events?source=twitter&rows=10000&from-occurred-date=2010-10-01&obj-id=", doi)
      
      req <- httr::RETRY("GET", cr_event_url, pause_min=0.1, pause_base=0.1, pause_cap=3, times=60, httr::timeout(3))
      stop_for_status(req)
      warn_for_status(req)
      message_for_status(req)
      
      e2 <- fromJSON(jsonlite::prettify(rawToChar(req$content)), flatten=TRUE)
      status <- e2$status
      
      events <- e2$message$events %>% 
        data.frame() %>%
        dplyr::select(names = subj.author.url, 
                      timestamps = timestamp, 
                      status = subj_id) %>%
        mutate(names=gsub("http://www.twitter.com/", "", names),
               status=gsub("http://twitter.com/.*statuses/", "", status)) %>%
        mutate(names=gsub("twitter://user\\?screen_name=", "", names),
               status=gsub("twitter://status\\?id=", "", status))
      
      handles <- unique(events$names) 
    } else if (method == "altmetric"){
      
      events <- events_from_altmetric(article_full_url)
      handles <- unique(events$names)
      
    }
    
    cat("done\n")
    
    
    cat("\n=== getting user and event metadata from twitter API ===\n")
    
    skip_langs <- c("ar", "ja", "zh-CN", "ko")
    
    user_data <- lookup_users(handles) %>%
      dplyr::filter(!(account_lang %in% skip_langs) & protected==FALSE)
    
    tweets <- lookup_tweets(events$status) %>%
      dplyr::select(names=screen_name, 
                    tweets=text, 
                    retweet_screen_name, 
                    status=status_id)
    
    events <- events %>%
      left_join(tweets)
    
    # separate data by original tweets and RTs
    original <- events %>% 
      dplyr::filter(is.na(retweet_screen_name)) %>% 
      group_by(names, tweets) %>% 
      count %>% 
      arrange(desc(n))
    
    rts <- events %>% 
      dplyr::filter(!is.na(retweet_screen_name)) %>% 
      group_by(retweet_screen_name, tweets) %>% 
      count %>% 
      arrange(desc(n))
    
    cat("done\n")
    
    if(length(unique(events$names))<50){
      next
    }
    
    cat("\n=== getting follower lists for users ===\n")
    
    follower_lists_full <- compile_follower_data(datadir, 
                                                 article_id, 
                                                 user_data, 
                                                 high_followers)
    cat("done\n")
    
    
    cat("\n=== getting follower bios ===\n")
    
    follower_bios_full <- compile_follower_bios(datadir, 
                                                article_id, 
                                                follower_lists_full)
    
    cat("done\n")
    
    
    if(report){
      cat("\n=== generating report ===\n")
      
      run_report(doi=doi_it, outdir=outdir, datadir=datadir,
                 article_df=article_df, cr_data=cr_data,
                 user_data=user_data, events=events,
                 follower_lists_full=follower_lists_full, 
                 follower_bios_full=follower_bios_full,
                 category)
      
      cat("done\n")
      
      
      cat("\n=== updating summary ===\n")
      
      run_summary(datadir)
      
      cat("done\n")
    }
    
  }
  
} else if (doi_group=="covid19") {
  ncov_json <- rjson::fromJSON(file="audiences/covid19_twitter.json")
  ncov_df <- do.call(rbind, lapply(ncov_json$rels, as.data.frame))
  
  for (i in 1:nrow(ncov_df)) {
    # for (i in 1:2) {
    
    paper_it <- ncov_df[i,]
    article_title <- paper_it$rel_title
    doi_it <- paper_it$rel_doi
    doi <- doi_it
    category <- "covid19"
    
    cat(paste0("\n===", article_title, " ", doi, " ===\n"))
    
    cat("\n=== caching high-follower accounts ===\n")
    
    files_to_scan <- update_follower_files(datadir)
    high_followers <- get_follower_cache(high_followers, files_to_scan, files_scanned)
    files_scanned <- files_to_scan
    
    cat(paste0("done. ", length(files_scanned), " additional files scanned for high-follower users.\n"))
    
    
    # cat(paste0("\n=== Scraping data for '", paper_it$title, "' (", doi_it, ") ===\n")) 
    
    
    cat("\n=== getting altmetric metadata ===\n")
    
    article_am <- try(altmetrics(doi = doi))
    
    if(inherits(article_am, "try-error")){
      cat(paste0(doi, ": not indexed by Altmetric\n"))
      next
    }
    
    article_df <- altmetric_data(article_am)
    
    if(is.null(article_df$cited_by_tweeters_count)){
      cat(paste0(doi, ": not enough tweets indexed\n"))
      next
    }
    
    article_id <- article_df$altmetric_id
    
    if(i==0){
      subdomain <- "www"
    } else {
      subdomain <- "biorxiv"
    }
    article_full_url <- paste0("https://", subdomain, ".altmetric.com/details/", article_id)
    
    cat("done\n")
    
    
    cat("\n=== getting paper metadata from crossref ===\n")
    
    cr_data <- cr_works(doi = doi)$data
    
    title <- gsub("\"|/|:|\\$", "", cr_data$title)
    nb_prefix <- paste0(gsub(" ", "_", title), "_", article_id)
    
    nb_file <- paste0(nb_prefix, ".html")
    nb_title <- paste0(title, ", ",
                       article_df$journal, ", ", cr_data$created)
    
    cat("done\n")
    
    
    if(file.exists(paste0(outdir, "/", nb_file)) & check_file){
      cat(paste0("report for ", nb_file, " already exists\n"))
      next
    }
    
    
    cat("\n=== querying event data ===\n")
    
    method <- "altmetric"
    # method <- "crossref"
    
    if (method == "crossref"){
      cr_event_url <- paste0("https://api.eventdata.crossref.org/v1/",
                             "events?source=twitter&rows=10000&from-occurred-date=2010-10-01&obj-id=", doi)
      
      req <- httr::RETRY("GET", cr_event_url, pause_min=0.1, pause_base=0.1, pause_cap=3, times=60, httr::timeout(3))
      stop_for_status(req)
      warn_for_status(req)
      message_for_status(req)
      
      e2 <- fromJSON(jsonlite::prettify(rawToChar(req$content)), flatten=TRUE)
      status <- e2$status
      
      events <- e2$message$events %>% 
        data.frame() %>%
        dplyr::select(names = subj.author.url, 
                      timestamps = timestamp, 
                      status = subj_id) %>%
        mutate(names=gsub("http://www.twitter.com/", "", names),
               status=gsub("http://twitter.com/.*statuses/", "", status)) %>%
        mutate(names=gsub("twitter://user\\?screen_name=", "", names),
               status=gsub("twitter://status\\?id=", "", status))
      
      handles <- unique(events$names) 
    } else if (method == "altmetric"){
      
      events <- events_from_altmetric(article_full_url)
      handles <- unique(events$names)
      
    }
    
    cat("done\n")
    
    
    cat("\n=== getting user and event metadata from twitter API ===\n")
    
    skip_langs <- c("ar", "ja", "zh-CN", "ko")
    
    user_data <- lookup_users(handles) %>%
      dplyr::filter(!(account_lang %in% skip_langs) & protected==FALSE)
    
    tweets <- lookup_tweets(events$status) %>%
      dplyr::select(names=screen_name, 
                    tweets=text, 
                    retweet_screen_name, 
                    status=status_id)
    
    events <- events %>%
      left_join(tweets)
    
    # separate data by original tweets and RTs
    original <- events %>% 
      dplyr::filter(is.na(retweet_screen_name)) %>% 
      group_by(names, tweets) %>% 
      count %>% 
      arrange(desc(n))
    
    rts <- events %>% 
      dplyr::filter(!is.na(retweet_screen_name)) %>% 
      group_by(retweet_screen_name, tweets) %>% 
      count %>% 
      arrange(desc(n))
    
    cat("done\n")
    
    if(length(unique(events$names))<50){
      next
    }
    
    cat("\n=== getting follower lists for users ===\n")
    
    follower_lists_full <- compile_follower_data(datadir, 
                                                 article_id, 
                                                 user_data, 
                                                 high_followers)
    cat("done\n")
    
    
    cat("\n=== getting follower bios ===\n")
    
    follower_bios_full <- compile_follower_bios(datadir, 
                                                article_id, 
                                                follower_lists_full)
    
    cat("done\n")
    
    
    if(report){
      cat("\n=== generating report ===\n")
      
      run_report(doi=doi_it, outdir=outdir, datadir=datadir,
                 article_df=article_df, cr_data=cr_data,
                 user_data=user_data, events=events,
                 follower_lists_full=follower_lists_full, 
                 follower_bios_full=follower_bios_full,
                 category)
      
      cat("done\n")
      
      
      cat("\n=== updating summary ===\n")
      
      run_summary(datadir)
      
      cat("done\n")
    }
    
  }
} else if (doi_group=="nature") {
  
  #-----------------------------------------------------------------------------
  # peer-reviewed journal family reports
  #-----------------------------------------------------------------------------
  nat_data <- list()
  
  prefixes <- c(
    #"10.1016", # Cell
    "10.1038" #, # Nature
    #"10.1126", # Science
    #"10.1371"  # PLoS
    )
  
  cat("\n=== scraping top papers from Altmetric API ===\n")
  for(prefix in prefixes){
    for(page in 1:20){
      am_api_url <- paste0("https://api.altmetric.com/v1/citations/1y", 
                           "?num_results=100&doi_prefix=", prefix, 
                           "&page=", page)
      
      nat_json <- fromJSON(am_api_url)
      nat_data[[paste0("doi", prefix, "-page", page)]] <- data.frame(nat_json$results) %>% 
        dplyr::select(title, journal, doi, cited_by_tweeters_count, altmetric_jid, type)
    }
  }

  data_full <- bind_rows(nat_data) %>%
    dplyr::filter(type=="article") %>%
    dplyr::filter(cited_by_tweeters_count>=80) %>%
    group_by(journal) %>%
    mutate(tot=n()) %>%
    dplyr::filter(tot>20) %>%
    group_by(journal) %>%
    top_n(100, cited_by_tweeters_count) %>%
    dplyr::filter(cited_by_tweeters_count<3000) %>%
    # dplyr::filter(journal=="Nature Genetics") %>%
    # dplyr::filter(journal=="Nature") %>%
    dplyr::filter(grepl("genetic", tolower(title))) %>% dplyr::filter(journal != "Nature Genetics") %>% 
    arrange(desc(cited_by_tweeters_count)) 
  
  dois <- data_full$doi
  cat("done\n")
  
  # for (doi_it in dois){
  for (i in (1+skip):length(dois)) {
    doi_it <- dois[i]
    cat("\n=== caching high-follower accounts ===\n")
    
    files_to_scan <- update_follower_files(datadir)
    high_followers <- get_follower_cache(high_followers, files_to_scan, files_scanned)
    files_scanned <- files_to_scan
    
    cat(paste0("done. ", length(files_scanned), " additional files scanned for high-follower users.\n"))
    
    
    cat("\n=== # Getting article topics ===\n")
    
    article_str <- gsub("10.1038", "articles", doi_it)
    page <- read_html(paste0("https://www.nature.com/", article_str))
    categories <- html_nodes(page, css=".mb2") %>% html_text("href")
    category <- categories[1]
    
    category <- "genetics"
    
    cat("done\n")
    
    
    cat("\n=== getting altmetric metadata ===\n")
    
    article_am <- altmetrics(doi = doi_it)
    article_df <- altmetric_data(article_am)
    article_id <- article_df$altmetric_id
    
    # get Altmetric URL, specifying journal-specific subdomain if needed
    if(grepl("10.1101", doi_it)){
      subdomain <- "biorxiv"
    } else if(grepl("10.1016", doi_it)){
      subdomain <- "cell"
    } else {
      subdomain <- "www"
    }
    
    article_full_url <- paste0("https://", 
                               subdomain, ".altmetric.com/details/", article_id)
    
    cat("done\n")
    
    
    cat("\n=== getting paper metadata from crossref ===\n")
    
    cr_data <- cr_works(doi = doi_it)$data
    
    title <- gsub("\"|/|:|\\$", "", cr_data$title)
    nb_prefix <- paste0(gsub(" ", "_", title), "_", article_id)
    
    nb_file <- paste0(nb_prefix, ".html")
    nb_title <- paste0(title, ", ",
                       article_df$journal, ", ", cr_data$created)
    
    nb_desc <- html_nodes(page, css="#Abs1-section :nth-child(1)") %>% html_text("p") %>% tail(1)
    
    nb_desc <- gsub("</*jats:[A-z]*>", "", nb_desc)
    nb_desc <-  gsub("\"|/|:", "", nb_desc)
    nb_desc <- gsub("\n", "", nb_desc)
    
    cat("done\n")
    
    
    if(file.exists(paste0(outdir, "/", nb_file)) & check_file){
      cat(paste0("report for ", nb_file, " already exists\n"))
      next
    }
    
    
    cat("\n=== querying crossref event data ===\n")
    
    method <- "altmetric"
    
    if (method == "crossref"){
      cr_event_url <- paste0("https://api.eventdata.crossref.org/v1/",
                             "events?source=twitter&rows=10000&from-occurred-date=2010-10-01&obj-id=", doi)
      
      req <- httr::RETRY("GET", cr_event_url, pause_min=0.1, pause_base=0.1, pause_cap=3, times=60, httr::timeout(3))
      stop_for_status(req)
      warn_for_status(req)
      message_for_status(req)
      
      e2 <- fromJSON(jsonlite::prettify(rawToChar(req$content)), flatten=TRUE)
      status <- e2$status
      
      events <- e2$message$events %>% 
        data.frame() %>%
        dplyr::select(names = subj.author.url, 
                      timestamps = timestamp, 
                      status = subj_id) %>%
        mutate(names=gsub("http://www.twitter.com/", "", names),
               status=gsub("http://twitter.com/.*statuses/", "", status)) %>%
        mutate(names=gsub("twitter://user\\?screen_name=", "", names),
               status=gsub("twitter://status\\?id=", "", status))
      
      handles <- unique(events$names) 
    } else if (method == "altmetric"){

      events <- events_from_altmetric(article_full_url)
      handles <- unique(events$names)
      
    }
    
    cat("done\n")
    


    cat("\n=== getting user and event metadata from twitter API ===\n")
    
    skip_langs <- c("ar", "ja", "zh-CN", "ko")
    
    user_data <- lookup_users(handles) %>%
      dplyr::filter(!(account_lang %in% skip_langs) & protected==FALSE)
    
    tweets <- lookup_tweets(events$status) %>%
      dplyr::select(names=screen_name, 
                    tweets=text, 
                    retweet_screen_name, 
                    status=status_id)
    
    events <- events %>%
      left_join(tweets)
    
    # separate data by original tweets and RTs
    original <- events %>% 
      dplyr::filter(is.na(retweet_screen_name)) %>% 
      group_by(names, tweets) %>% 
      count %>% 
      arrange(desc(n))
    
    rts <- events %>% 
      dplyr::filter(!is.na(retweet_screen_name)) %>% 
      group_by(retweet_screen_name, tweets) %>% 
      count %>% 
      arrange(desc(n))
    
    cat("done\n")
    
    
    cat("\n=== getting follower lists for users ===\n")
    
    follower_lists_full <- compile_follower_data(datadir, 
                                                 article_id, 
                                                 user_data, 
                                                 high_followers)
    cat("done\n")
    
    
    cat("\n=== getting follower bios ===\n")
    
    follower_bios_full <- compile_follower_bios(datadir, 
                                                article_id, 
                                                follower_lists_full)
    
    n_analyzed <- nrow(follower_lists_full)
    count_group <- ifelse(n_analyzed < 100, "0-100-users",
                          ifelse(n_analyzed < 200, "100-200-users",
                                 ifelse(n_analyzed < 500, "200-500-users", "500+-users")))
    
    cat("done\n")
    
    
    cat(paste0("\n=== generating report for ", doi_it, " ===\n"))
    
    # run_report(doi, outdir, check_file)
    
    run_report(doi=doi_it, outdir=outdir, datadir=datadir,
               article_df=article_df, cr_data=cr_data,
               user_data=user_data, events=events,
               follower_lists_full=follower_lists_full, 
               follower_bios_full=follower_bios_full,
               category)
    
    cat("done\n")

        
    cat("\n=== updating summary ===\n")
    
    run_summary(datadir)
    
    cat("done\n")
    
  }
}

run_summary(datadir)

cat("done\n")
