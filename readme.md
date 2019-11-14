# Build holdings database from SEC Filings

This is a repo dedicated to building a complete list of SEC filed holdings. Certainly a work in progress, with more details to come.

## Status

* The SEC Crawlers are working
* Multiple working test casts for html filings
* Prototype text parser, and filter that guesses text vs. html
* Tests have worked with multiple filing types
* Header parsing works for all filing types
* Column format classifiers are working as intended

## To-Do

* Organize build process
* Leverage header information to properly split tables between funds
* Write out fund-level dataset to local .csv file in the data/ folder
* Scale up test cases on the order of 100 randomly sampled filings
* Figure out how to properly parse bond data...

# Requirements

* R programming language

# Goals, Structure, Layout

## Design Goals

* General algorithm for parsing the vast majority of tables in the Edgar database
* Buildable from anyone's laptop who has R installed
* Generate comparison with current holdings data in e.g. the CRSP mutual fund database

## Structure

### Scripts

/build - This contains information used by the build process. The parse-crawler and build-crawler-dataset is finished, but parse-filing is still working through a few unit tests to make sure I'm getting the desired behavior.

/helpers - These are helper functions for the parsers, that help to work with text, ensure consistent column formats, identify striped vs. regular table structures, etc.

/parsers - These are functions specifically for parsing tables from filings. The html filings are parsed by functions in the html-parsers.R file, while the text filings are parsed by test-parsers.R. SEC header info is parsed by header-parsers.R, and general-parsers.R contains functions that generalize these calls so the end user doesn't have to specify (e.g. functions that guess text or html, etc.).


The build process creates a data folder with the structure

```
_data  
├── _raw  
|   └── _year  
|       └── _quarter 
|           ├── ugly-sec-text.txt
|           └──crawler.idx
├── _processed  
    └── _year  
        └── _quarter 
            ├── pretty-holdings-table.csv
            └── crawler.csv

```

Each filing will have its own parsed table with the CIK (SEC given ID) and filing type in the name of the file. This is going to parse all fiings from years 1993 through the present.

## Use and Build Process

The src/ folder will contain most of the source code. parse-crawler.R will grab the relevant parsing files from the SEC Edgar website, and those can subsequently be used by parse-filings.R. The functions in the src/helpers folder are functions used by the various parsers. The src/build scripts will actually be responsible for building the end result. parse-crawler is working now, but parse-filing is not. The src/algorithm-attempts folder contains the current attempts to build parsers, some of which are text based and/or html based and some of which are built on OCR tools.

There will be an all-encompassing build script, but since almost all of this does not yet do what it sets out to, it wouldn't really build anything! As we get to a point where the parser has a decent level of coverage, I'll add in a build script. If you feel like playing around with it, start with parse-crawler; parse-filing relies on that script.
 
# Filing Types

Largely these holdings tables will be based on the N-Q filings, although there have been recent changes on filing formats. Depending on what is easiest, we may switch which ones we parse in the more recent periods.

# Approach

The main issue with these filings is that there is no standard table structure. Some use ascii characters and spaces to denote their tables, with random linebreaks to make them look nice, and others use html table structures. Even those that use html tables have improper linebreaks, as instead of using some kind of css formatting they decided to make the html look better by putting line breaks where they wouldn't otherwise go.

For now I have taken the approach of grabbing the text in the SEC filings, normalizing some common financial dataset features (e.g. \$   10000 -> \$10000), 
