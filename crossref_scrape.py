# old verison used to scrape data from Crossref Events API
# deprecated in favor of the crevents R package to streamline
# analysis within the R scripts https://github.com/ropensci/crevents

import requests
import re
import pandas as pd
import yaml

with open("papers.txt") as f:
    dois = f.read().splitlines()

# dois[1] = "10.1038/s41588-018-0147-3"

for doi in dois:

    parameters = {"mailto": "NA",
                 "obj-id": doi,
                 "source": "twitter",
                 "from-collected-date": "2012-01-01",
                 "rows": 2000}
                 
    response = requests.get("https://api.eventdata.crossref.org/v1/events", 
        params=parameters)
    tweets = response.json()
    
    tw_url = "http://www.twitter.com/"
        
    df_cols = ["handles", "original", "tweets", "timestamps"]
    tweet_df = pd.DataFrame(columns=df_cols)
    for tweet in tweets["message"]["events"]:
        account_url = tweet["subj"]["author"]["url"]
        account = re.sub("http://www.twitter.com/", "", account_url)
        account = re.sub("twitter://user\?screen_name=", "", account)
    #     print(account)
        original_author = tweet["subj"]["original-tweet-author"]
        if original_author is not None:
            original_author = re.sub("http://www.twitter.com/", "", original_author)
            original_author = re.sub("twitter://user\?screen_name=", "", original_author)
        
        # print(account)
        
        url = tweet["subj"]["pid"]
        url = re.sub("twitter://status\?id=", 
                    "http://www.twitter.com/"+account+"/statuses/", url)
        date = tweet["subj"]["issued"]
        
    #     row_df = pd.DataFrame({"account": account, "original": original_author, "tweets": url, "timestamps": date})
    #     print(row_df)
        tweet_df = tweet_df.append({"handles": account, 
                "original": original_author, 
                "tweets": url, 
                "timestamps": date},
            ignore_index=True)
            
    
    print("doi: "+doi, tweet_df.shape)
            
    tweet_df.to_csv("article_data/"+re.sub("/", "-", doi)+".txt", sep="\t", index=False)