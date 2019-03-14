# load packages
library(RCurl)
library(rtweet)
library(tweetscores)
library(knitr)
library(markdown)
library(rmarkdown)
library(rAltmetric)
library(rvest)
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
# Define list of articles
#-----------------------------------------------------------------------------
article_urls <- c(
                  "https://cell.altmetric.com/details/34376150",    # Browning et al (denisovan admixture) XX
                  "https://biorxiv.altmetric.com/details/52355933", # Ragsdale et al (ghost admixture) XX
                  "https://biorxiv.altmetric.com/details/34668368", # # Durvasula et al (ghost admixture) XX
                  "https://biorxiv.altmetric.com/details/52608959", # Jensen et al (Stone Age chewing gum) XX
                  "https://biorxiv.altmetric.com/details/43592322", # Villanea & Schraiber (ghost admixture) XX
                  "https://www.altmetric.com/details/46833965",     # Slon et al (Neanderthal/Denisovan offspring) XX
                  "https://www.altmetric.com/details/45430386",     # Lee et al (educational attainment) XX
                  "https://biorxiv.altmetric.com/details/16179150", # Hill et al (family intelligence biorxiv version) XX
                  "https://www.altmetric.com/details/31492953",     # Hill et al (family intelligence Mol Psych version) XX
                  "https://biorxiv.altmetric.com/details/50501527", # Abdellaoui et al (social strat~genetics) XX [nice UMAP structure; shows WN-affiliated psych/econ/philosophy/polisci]
                  "https://pnas.altmetric.com/details/15551866",    # Kong et al (selection for educational attainment vars) XX [nice UMAP structure; shows WN-affiliated psych/econ/philosophy/polisci]
                  "https://biorxiv.altmetric.com/details/53351163", # MacLean et al (dog behavior) XX [example of non-human]
                  "https://cell.altmetric.com/details/49945431",    # ASHG statement XX [heavily polarized]
                  "https://www.altmetric.com/details/53917059",     # Lakhani et al (health insurance claims) XX
                  "https://www.altmetric.com/details/55811639",     # Cao et al (organogenesis) XX
                  "https://www.altmetric.com/details/51402455",     # Sherman et al (African pan genome) XX
                  "https://biorxiv.altmetric.com/details/55925121", # Bridavsky et al (Lil Bub genome) XX
                  "https://www.altmetric.com/details/49530141",     # Erlich et al (DNA id) XX
                  "https://cell.altmetric.com/details/49530208",    # Kim et al (DNA id 2) XX
                  "https://www.altmetric.com/details/12603108",     # Field et al (human adaptation) XX
                  "https://www.altmetric.com/details/443774",       # Behar et al (Jewish genetics) XX [virtually no sci clusters]
                  "https://www.altmetric.com/details/2118810",      # Hellenthal et al (Global admixture) XX
                  "https://www.altmetric.com/details/2858415",      # Seguin-Orlando et al (Euro history) XX
                  "https://biorxiv.altmetric.com/details/34262871", # Bycroft et al (Spanish pop struct) XX [good UMAP cosine structure]
                  "https://www.altmetric.com/details/115659",       # Novembre et al (genes mirror geography) XX
                  "https://www.altmetric.com/details/27367975",     # Crawford et al (skin pigmentation) XX # [suprisingly few wn clusters]
                  "https://biorxiv.altmetric.com/details/49534330", # Martin et al (PRS risk) XX
                  "https://biorxiv.altmetric.com/details/48265719", # Albers & McVean (dating variants) XX
                  "https://biorxiv.altmetric.com/details/10104753", # Lawson et al (STRUCTURE tutorial biorxiv version) XX
                  "https://www.altmetric.com/details/46498440"      # Lawson et al (STRUCTURE tutorial Nat Comm version) XX
                  )     

#-----------------------------------------------------------------------------
# Generate reports
#-----------------------------------------------------------------------------
for (article_full_url in article_urls){
  
  # scrape doi and abstract
  article_id <- gsub(".*.details/", "", article_full_url)
  
  summary_page <- read_html(article_full_url)
  doi <- summary_page %>% 
    html_nodes("div.document-details-table tr:nth-child(3) :nth-child(1)") %>% 
    html_text()
  
  article_doi <- doi[2]
  
  if(grepl("biorxiv", article_full_url)){
    biorxiv_page <- read_html(paste0("https://www.biorxiv.org/content/", article_doi, "v1"))
    abstract <- biorxiv_page %>% 
      html_nodes("div.abstract #p-2") %>% 
      html_text()  
  } else {
    abstract1 <- summary_page %>% 
      html_nodes("div.content-wrapper tr:nth-child(6) :nth-child(1)") %>% 
      html_text()
    abstract <- gsub("\n", "", abstract1[2])
  }
  
  # altmetric metadata from API
  article_am <- altmetrics(doi = article_doi)
  article_df <- altmetric_data(article_am)
  
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
                                  doi=article_doi))
  
}