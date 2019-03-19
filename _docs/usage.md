# Setup Twitter API

To reproduce these analyses or run Audiences on your own paper(s), you will first need to set up a Twitter developer account for access to the Twitter API. (Documentation for setting up a Twitter dev account is available [here](https://rtweet.info/articles/auth.html)). Once completed, update the app name, consumer keys, and access keys in `config.yaml`.

# Generate reports

Running `render_reports.R` will generate a separate report for each of the papers listed in `papers.txt` by their Altmetric URLs (one per line). Reports are based on the `report_template.rmd` RMarkdown template.

Reports will be written to `output/reports` and thumbnail images for each report to `output/figures`.

# Build site

```
# generate data/items.toml, containing the links to reports to include in landing page
python generate_links.py

# build the landing page into _docs/static/ based on the hugrid Hugo template in themes/hugrid/
# - requires config.toml and data/items.toml
hugo

# build with mkdocs into docs/
# - requires mkdocs.yml and contents of _docs/
mkdocs build -d docs 

# copy the reports & thumbnails into docs/static/
rsync -r output/ docs/static

# push changes to github and the documentation will be available at https://carjed.github.io/audiences/
```
