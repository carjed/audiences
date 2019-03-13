![](title.png)

![](audiences.gif)

# Background

Audiences is a framework for exploring the various audiences that are engaging with academic publications on Twitter.

Audiences is written primarily in R, and collects data using the Twitter API.

# Setup

## Prerequisites and dependencies

You will need a recent version of RStudio if you wish to use the interactive notebook capabilities.

## Dependencies

Audiences requires several R packages to run. 

## Twitter API access

Once you have a developer account set up, copy and paste the API keys into `config.yaml`

# Running Audiences

`render_reports.R` is a wrapper script to generate the reports for a list of papers. `report_template.rmd` is an R Markdown-formatted template.

# Serving as a webpage

The reports are formatted as interactive HTML documents, making them ideal to share with others on a website. Each report is a self-contained .html file, so you can simply to your own personal website. (e.g., if you have a list of your lab's papers on your website, you can generate a report for each and add a link to the corresponding .html)

Alternatively, if you have forked the Audiences Github repository, you can use Github pages to host the reports.

I am using Hugo with the hugrid template to create a simple static landing page with tiles that link to `static/reports/report.html`. The website files are hosted in `docs`, and the Github project page is set up to point to this directory.


