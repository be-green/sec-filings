# Build holdings database from SEC Filings

This is a repo dedicated to building a complete list of SEC filed holdings. Certainly a work in progress, with more details to come.

# Requirements

* R programming language
* Java SE 8 or higher.

# Goals, Structure, Layout

## Design Goals

* General algorithm for parsing the vast majority of tables in the Edgar database
* Buildable from anyone's laptop who has R installed
* Generate comparison with current holdings data in e.g. the CRSP mutual fund database

## Structure

The current layout puts most of the emphasis in the src/ folder, which contains scripts for the build process, helpers for text and table format parsing, and attempts to implement a set of different algorithms. The build process creates a data folder with the structure

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
 
## Filing Types

Largely these holdings tables will be based on the N-Q filings, although there have been recent changes on filing formats. Depending on what is easiest, we may switch which ones we parse in the more recent periods.

## Approach

The main issue with these filings is that there is no standard table structure. Some use ascii characters and spaces to denote their tables, with random linebreaks to make them look nice, and others use html table structures. Even those that use html tables have improper linebreaks, as instead of using some kind of css formatting they decided to make the html look better by putting line breaks where they wouldn't otherwise go.

This makes a general text parser close to impossible, so we are taking a split approach using both more traditional text/html/xml parsers and OCR tools when those fail. This is all still experimental, so if you have comments or suggestions please leave a note.



