# load packages
library(RCurl)
library(rtweet)
library(tweetscores)
library(knitr)
library(markdown)
library(rmarkdown)
library(rAltmetric)
library(rvest)
library(rcrossref)
library(tidyverse)
library(yaml)
library(anytime)
library(here)

#-----------------------------------------------------------------------------
# Setup API keys
# To use the Twitter API, [sign up for a developer account and obtain the necessary API tokens](https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html). For security, these are stored in a separate `_config.yaml` file.
#-----------------------------------------------------------------------------
# set relative path for calling external files from within this script
datadir <- dirname(sys.frame(1)$ofile)

keys <- yaml.load_file(paste0(datadir, "/_config.yaml"))

# load history from crashed session
# loadhistory("~/.rstudio/history_database")

# oauth for tweetscores functions
my_oauth <- list(consumer_key=keys$consumer_key,
                 consumer_secret=keys$consumer_secret,
                 access_token=keys$access_token,
                 access_token_secret=keys$access_secret)

# set token for rtweet
token <- create_token(
  app = keys$app_name,
  consumer_key = keys$consumer_key,
  consumer_secret = keys$consumer_secret,
  access_token=keys$access_token,
  access_secret=keys$access_secret)

# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#-----------------------------------------------------------------------------
# functions for topic modeling
#-----------------------------------------------------------------------------
# tmls <- get_timelines(plotdat$account[1:4], n = 3200) # testing with timelines instead of bios

get_follower_bios <- function(followers){
  f5k <- unlist(followers)
  f5k <- head(f5k, 10000)
  
  lookup_users(f5k, token=token)
}

# get emoji lookup table
source(paste0(datadir, "/scrapeEmoticons.R"))

# source script directly from from https://github.com/today-is-a-good-day/emojis
# script <- getURL("https://raw.githubusercontent.com/today-is-a-good-day/emojis/master/scrapeEmoticons.R", ssl.verifypeer = FALSE)
# eval(parse(text = script))

lookup <- alltogether %>% 
  mutate(Desc_Sub=tolower(paste0(" emoji", gsub("[+]|-| ", "", Description), " "))) %>% # ensure textualized emoji is separated by space
  mutate(Desc_Sub2=gsub(" ", "", Desc_Sub)) # strip spaces for proper text -> emoji matching 

# define lookup matching functions
# https://stackoverflow.com/questions/31828218/replace-values-in-character-vector-using-lookup-table-in-r
emoji_to_text <- function(x) Reduce(function(x,r) gsub(lookup$Native[r],lookup$Desc_Sub[r],x,fixed=T),seq_len(nrow(lookup)),x)
text_to_emoji <- function(x) Reduce(function(x,r) gsub(lookup$Desc_Sub2[r],lookup$Native[r],x,fixed=T),seq_len(nrow(lookup)),x)

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
# read reference data from scientist/WN classifications
#-----------------------------------------------------------------------------

# basic data frame containing curated usernames of WN
training_data_fh <- paste0(datadir, "/training_data/training_data_handles.rds")
training_data <- readRDS(training_data_fh)

# extended data frame containing user metadata & follower lists
training_data_full_fh <- paste0(datadir, "/training_data/training_data_full2.rds")

if(file.exists(training_data_full_fh)){
  training_data_full <- readRDS(training_data_full_fh)
} else {
  training_user_data <- lookup_users(training_data$account) %>%
    mutate(account=screen_name) %>%
    left_join(training_data, "account")
  
  fc_mod <- ceiling(training_user_data$followers_count/5000)
  
  sleep <- ifelse(sum(fc_mod)>15, 60, 0)
  
  # get followers for accounts in training data
  training_data_full <- training_user_data %>% #head
    rowwise %>% 
    mutate(followers=getFollowers(screen_name=account, oauth=my_oauth, sleep=sleep) %>% 
             data.frame %>% 
             as.list) # tweetscores
  # old version using rtweet API calls--requires fixed n
  # mutate(followers=get_followers(account, n=20000, token=token, retryonratelimit = TRUE) %>% as.list)
  
  saveRDS(training_data_full, training_data_full_fh)
}

#-----------------------------------------------------------------------------
# Read list of article DOIs and Altmetric URLs
# - in the future, this will be purely DOI-based
# - can also pull in list of popular bioRxiv papers using the Rxivist API
#-----------------------------------------------------------------------------
dois <- scan(paste0(datadir, "/papers.txt"), what="", sep="\n")
# article_urls <- scan(paste0(datadir, "/papers_altmetric.txt"), what="", sep="\n")

#-----------------------------------------------------------------------------
# Generate reports
#-----------------------------------------------------------------------------
for (doi in dois){
  # metadata <- cr_works(dois = doi)
  
  # altmetric metadata from API
  article_am <- altmetrics(doi = doi)
  article_df <- altmetric_data(article_am)
  article_id <- article_df$altmetric_id
  
  # get Altmetric URL, specifying journal-specific subdomain if needed
  if(grepl("10.1101", doi)){
    subdomain <- "biorxiv"
  } else if(grepl("10.1016", doi)){
    subdomain <- "cell"
  } else {
    subdomain <- "www"
  }
  
  article_full_url <- paste0("https://", subdomain, ".altmetric.com/details/", article_id)
  
  # get abstract if it's available in Crossref metadata
  abstract <- try(cr_abstract(doi))
  if(inherits(abstract, "try-error")){
    abstract <- "" 
  }
  
  nb_file <- gsub(" ", "_", paste0(gsub(",.*", "", article_df$authors1), " et al-", 
                                   gsub("\"|/", "", article_df$title), "_", article_id, ".html"))
  nb_title <- paste0("Twitter Audience Analysis of '", article_df$title, 
                     "' by ", gsub(",.*", "", article_df$authors1), " et al., published in ",
                     article_df$journal, " on ", anydate(as.integer(article_df$added_on)))
  
  # render HTML report for paper
  rmarkdown::render(paste0(datadir, "/report_template.rmd"),  # file 2
                    # output_file =  paste0(nb_file, '_', Sys.Date(), ".html"), 
                    output_file =  nb_file, 
                    output_dir = paste0(datadir, "/output/reports"),
                    params = list(title=nb_title, 
                                  abstract=abstract,
                                  datadir=datadir,
                                  doi=doi))
  
}
