![](_docs/title.png)

![](_docs/audiences.gif)

> Thy various works, imperial queen, we see, <br />
    How bright their forms! how deck'd with pomp by thee!

_(from 'On Imagination' by Phillis Wheatley [1753–1784])_

# Introduction

This repository contains code and results for analyzing the Twitter audiences of various papers and preprints in genetics and genomics.

# Setup Twitter API

To reproduce these analyses or run Audiences on your own paper(s), you will first need to set up a Twitter developer account for access to the Twitter API. Once completed, update the app name, consumer keys, and access keys in `config.yaml`.

# Generate reports

Running `render_reports.R` will generate a separate report for each of the papers listed in `papers.txt` by their Altmetric URLs (one per line). Reports are based on the `report_template.rmd` RMarkdown template.

Reports will be written to `output/reports` and thumbnail images for each report to `output/figures`.

# Build site

A custom landing page for the reports, hosted on Github Pages, can be generated with the following commands. Note that this requires [Hugo](https://gohugo.io/) and [MkDocs](https://www.mkdocs.org/) with the [cinder](https://sourcefoundry.org/cinder/) theme are installed on your system.

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
