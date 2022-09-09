# Poland_Biodiversity

 
## Check out the dashboard [HERE](https://dash-boards.shinyapps.io/Poland_Biodiversity/)
![image](https://user-images.githubusercontent.com/50706468/189398747-7c21e950-cf9f-4de0-b66a-87ae5666e485.png)

This is a repository for the Poland Biodiversity Species, data was obtained from [GBIF.org](https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165). GBIF.org (06 September 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.2jywgt

The original dataset is tremendously large and covers the whole world however, for thi app only need observations from Poland. In this regard the Original zip file which was 20gb zip file was stored into an sqlite3 database to allow for import into r.

## Sourcing data
First i uzipped the compressed data files using R as described in the Script.Rmd file and the went ahead to create an sqlite database. Tis is also possibe iside R but I choose to use the sqlite3 program in command line.

To create the sqlite3 database (Biodiversity.db) in the data folder, I used the comandline to navigate into the directory and ran the following commands

- $ sqlite3 
- $ .open Biodiversity.db
- $ .mode csv
- $ .import ocurrence.csv Biodiversity 

All steps of how I have substed the data are availbale in the Script.Rmd file

The app has been deployed on shinnyapps.io
