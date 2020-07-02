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

keys <- yaml.load_file(paste0(getwd(), "/_bots/battlestar_remix/_config.yaml"))

# load history from crashed session
# loadhistory("~/.rstudio/history_database")

# oauth for tweetscores functions
# cat("setting up twitter oauth...")
token <- rtweet::create_token(app="testapp",
                              consumer_key=keys$api_key,
                              consumer_secret=keys$api_secret,
                              access_token=keys$access_token,
                              access_secret=keys$access_token_secret)

my_oauth <- list(consumer_key=keys$api_key,
                 consumer_secret=keys$api_secret,
                 access_token=keys$access_token,
                 access_token_secret=keys$access_token_secret)

hf_path <- "/mnt/alduin/follower_data"

# group of DOIs to analyze
# doi_group <- "select"
doi_group <- "biorxiv"
# doi_group <- "nature"

outdir <- paste0(datadir, "/content/reports")
dir.create(outdir, recursive=TRUE, showWarnings = FALSE)

cat("done\n")

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
# get list of follower files from generated reports
#-----------------------------------------------------------------------------
update_follower_files <- function(){
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
compile_follower_data <- function(datadir, article_id, user_data){
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
    if(nrow(follower_lists_cache)/nrow(user_data)<0.9){
      
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
        # pull from cache if user exists in high-follower database
        if(user %in% hf_handles){
          
          cat("cached in high-follower list \n")
          # i <- i+1
          # follower_list_user <- tibble(account=user, followers=list())
          # next
          
        } else if (user %in% follower_lists_cache$account) {
          
          cat("cached in previous scrape \n")
          follower_list_user <- follower_lists_cache %>%
            dplyr::filter(account==user)
          
          follower_list_sub <- bind_rows(list(follower_list_sub, follower_list_user))
          follower_lists_full <- bind_rows(list(follower_lists_full, follower_list_user))
          
        } else {
          
          cat("new\n")
          
          if(j %% 2 == 1){
            use_oauth = my_oauth
          } else {
            use_oauth = my_oauth
          }
          
          follower_list_user <- user_data %>%
            dplyr::filter(screen_name == user) %>%
            dplyr::select(account = screen_name) %>% #head
            mutate(followers = getFollowers(screen_name = account, 
                                            # oauth = my_oauth, 
                                            oauth = use_oauth, 
                                            sleep = 60) %>% 
                     data.frame %>% 
                     as.list)
          j <- j+1
        
          follower_list_sub <- bind_rows(list(follower_list_sub, follower_list_user))  
          follower_lists_full <- bind_rows(list(follower_lists_full, follower_list_user))
        }  
        
        
        # cache to disk every 10 users
        if(i %% 50 == 0 | i==nrow(user_data)){
          cat("caching to disk\n")
          follower_list_sub_fh <- paste0(datadir, 
                                         "/article_data/altmetric_data_", 
                                         as.character(i), "_", article_id, ".rds")
          saveRDS(follower_list_sub, follower_list_sub_fh)
          follower_list_sub <- tibble(account=character(), followers=list())
        }
        
        
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
            token <- token
          } else {
            token <- token
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
            Sys.sleep(5)
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
# Read list of article DOIs and Altmetric URLs
# - in the future, this will be purely DOI-based
# - can also pull in list of popular bioRxiv papers using the Rxivist API
#-----------------------------------------------------------------------------
check_file <- FALSE
report <- TRUE
skip <- 326

# list of DOIs to skip because they lack enough tweets or are missing metadata
banned_dois <- c("10.1101/397067", "10.1101/501494", "10.1101/066803") #?)

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
    
    api_url1 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime"
    api_url2 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=1"
    api_url3 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=2"
    api_url4 <- "https://api.rxivist.org/v1/papers?metric=twitter&page_size=250&timeframe=alltime&page=3"
    
    cat_json1 <- fromJSON(api_url1)
    cat_json2 <- fromJSON(api_url2)
    cat_json3 <- fromJSON(api_url3)
    cat_json4 <- fromJSON(api_url4)
    dois_df <- bind_rows(data.frame(cat_json1$results),
                         data.frame(cat_json2$results),
                         data.frame(cat_json3$results),
                         data.frame(cat_json4$results)) %>%
      dplyr::filter(metric>=80 & metric <1500) %>% as_tibble() %>%
      dplyr::filter(!doi %in% banned_dois) %>%
      arrange(metric)
  }
  
  dois <- dois_df$doi
  cat("done\n")
  
  for (i in (1+skip):nrow(dois_df)) {
    hf_handles_fh <- paste0(datadir, "/training_data/high_follower_handles.rds")
    hf_handles <- readRDS(hf_handles_fh)
    
    paper_it <- dois_df[i,]
    doi_it <- paper_it$doi
    doi <- doi_it
    category <- paper_it$category
    
    cat("\n=== caching high-follower accounts ===\n")
    
    files_to_scan <- update_follower_files()
    # high_followers <- get_follower_cache(high_followers, files_to_scan, files_scanned)
    files_scanned <- files_to_scan
    
    cat(paste0("done. ", length(files_scanned), " additional files scanned for high-follower users.\n"))
    
    
    cat(paste0("\n=== Scraping data for '", paper_it$title, "' (", doi_it, ") ===\n")) 
    
    
    cat("\n=== getting altmetric metadata ===\n")
    
    article_am <- altmetrics(doi = doi)
    article_df <- altmetric_data(article_am)
    article_id <- article_df$altmetric_id
    
    cat("done\n")
    
    
    cat("\n=== getting paper metadata from crossref ===\n")
    
    cr_data <- cr_works(doi = doi)$data
    
    title <- gsub("\"|/|:", "", cr_data$title)
    nb_prefix <- paste0(gsub(" ", "_", title), "_", article_id)
    
    nb_file <- paste0(nb_prefix, ".html")
    nb_title <- paste0(title, ", ",
                       article_df$journal, ", ", cr_data$created)
    
    cat("done\n")
    
    
    if(file.exists(paste0(outdir, "/", nb_file)) & check_file){
      cat(paste0("report for ", nb_file, " already exists\n"))
      next
    }
    
    
    cat("\n=== querying crossref event data ===\n")
    
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
    
    cat("done\n")
    
    # For papers where Crossref is missing >50% of tweets, fall back to
    # scraping from Altmetric
    # if(length(handles)/as.numeric(article_df$cited_by_tweeters_count) < 0.5){
    #   
    #   Sys.sleep(5)
    #   
    #   cat("\n=== crossref and altmetric data do not match—scraping from altmetric ===\n")
    #   events <- events_from_altmetric(article_full_url)
    #   handles <- unique(events$names)
    #   cat("done\n")
    # }
    
    
    cat("\n=== getting user and event metadata from twitter API ===\n")
    
    skip_langs <- c("ar", "ja", "zh-CN", "ko")
    
    user_data <- lookup_users(handles) %>%
      dplyr::filter(!(account_lang %in% skip_langs) & protected==FALSE)
    
    cat("done\n")
    
    
    cat("\n=== getting follower lists for users ===\n")
    
    follower_lists_full <- compile_follower_data(datadir, 
                                                 article_id, 
                                                 user_data)
    cat("done\n")
    
    
    cat("\n=== getting follower bios ===\n")
    
    follower_bios_full <- compile_follower_bios(datadir, 
                                                article_id, 
                                                follower_lists_full)
    
    cat("done\n")
    
  }
  
}