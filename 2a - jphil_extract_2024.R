# I've done this to pull all things where 'philos' is in the cited source, so this gets *way* too much
# Have to filter afterwards to get the JPhil results

# The directory the RData will be saved to is hard coded

# This produces 'raw' results - have to 'fix' them after compiling
# After compiling can sort the sources and visually check which things are journal of philosophy

# There is an error in 2021-008 that you just have to work around - it will throw an error, then have to manually restart
# Mostly the 'try' command lets you roll through the ones that are not journals.
# For them, xml_child(xml_parent(f), 3) doesn't exist, so code will break with an error

require(tidyverse)
require(xml2)

rm(list = ls())
setwd("/Volumes/Weatra-Backup/web-of-science")
filelist <- list.files(pattern = "xml$", recursive = TRUE)
todolist <- (filelist %>% str_replace(".xml",""))

name_tibble <- tibble(x = todolist) |>
  mutate(short_name = paste0(
    str_sub(x, 14, 17),
    "_",
    str_sub(x, 39, 42))) |>
  mutate(year = as.numeric(
    str_sub(
      short_name,
      1,
      4
    )
  )) |>
  filter(year >= 1971) |>
  filter(short_name != "2021_0008")

master_refs <- c()

start_time <- Sys.time()

for (todoname in name_tibble$x){
  
  stage_01 <-read_xml(paste0(todoname,".xml"))
  
  source_nodes <- xml_find_all(stage_01,".//d1:citedWork")
  
  therefs <- c()
  
  for (f in source_nodes){
    if (#xml_text(f) == "J PHILOS" | 
        #xml_text(f) == "JOURNAL OF PHILOSOPHY" | 
        #xml_text(f) == "J Philos" | 
        #xml_text(f) == "J Philos." |
        #xml_text(f) == "J. Philos." |
        #xml_text(f) == "J. Philos" |
        #xml_text(f) == "Journal of Philosophy" |
        #xml_text(f) == "The Journal of Philosophy" |
        #xml_text(f) == "The Journal Of Philosophy" |
        grepl("philos", xml_text(f), ignore.case = TRUE) #|
        #xml_text(f) == "Journal Of Philosophy"
        )
      {
      y <- try(xml_child(xml_parent(f), 3), silent = TRUE)
      if (try(xml_text(y), silent = TRUE) > 1970 & try(xml_text(y), silent = TRUE) < 1975){
        h <- xml_parent(f)
        therefs <- bind_rows(
          therefs,
          as_tibble(as_list(h))
        )     
      }
      y <- 0
    }
  }
  
  save(therefs, file = 
         paste0(
           "/Users/weath/Documents/citations-2024/philos_process/philos_",
           str_sub(todoname, 14, 17),
           "_",
           str_sub(todoname, 39, 42),
           "a.RData"
           )
  )
}

end_time <- Sys.time()

end_time - start_time


