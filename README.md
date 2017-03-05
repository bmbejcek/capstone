# Data Analytics Capstone Project: PwC Insurance Data

INTRODUCTION
------------

This project focuses on real world big data implementations of data in the insurance industry. Our goal is to analyze the customer segmentation to provide recommendations on how to minimize the loss ratio.

BUILDING SQLITE DATABASE
------------

As the original excel data file was separated into multiple tabs, we decided the best way to manipulate the data was to build a SQLite Database. The information for this database is located under the data file. Specifically, the word document "FromExceltoSQLite.docx" discusses this process. 

CREATING SINGLE .CSV FILE
------------

We wanted all of our analysis to be based off of a single .csv file that contained all pertinent information grouped by driverID. As the claims table could have multiple claims per driverID, we summed this amount of claims to get a singular driver view. From here, we joined this table on data from other table to get one final file which would allow for in-depth analysis.

AUTHORS
-------
* Rafah Asadi
* Brett Bejcek
* Peter Jacobs
* Kyle Voytovich
