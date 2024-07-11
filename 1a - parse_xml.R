require(tidyverse)
require(xml2)

rm(list = ls())
setwd("/Volumes/Weatra-Backup/web-of-science") # Make sure external drive is plugged in
filelist <- list.files(pattern = "xml$", recursive = TRUE) # The $ is end-of-string - this gets rid of .xml.gz
todolist <- filelist %>% str_replace(".xml","")

authadjust <- function(x){
  paste0(str_extract(x, '\\b[^,]+$'), " ", str_to_title(str_extract(x,".+(?=,)")))
}

extract_info <- function (x){
  enframe(x) %>% pivot_wider(names_from = name, values_from = value, values_fn = list)
}

master_refs <- c()

for (todoname in todolist){

  temp_name <- paste(
    substr(todoname,14,17), # This should be 4, 7 if all the files are in the directory currently in
                          # This version is if you are sitting one level up, and running the script over sub-directories
    substr(todoname,nchar(todoname) - 2,nchar(todoname)),
    sep = "-"
  )
  
  stage_01 <- read_xml(paste0(todoname,".xml"))

  subject_nodes <- xml_find_all(stage_01,".//d1:subject")

  stage_03 <- c()

  for (a in subject_nodes){

    if (xml_text(a) == "Philosophy" | xml_text(a) == "History & Philosophy Of Science"){

      b <- a
      for (i in 1:5){
        b <- xml_parent(b)
      }

      stage_02a <- as_list(b)
      stage_02aa <- stage_02a[1:2]
      stage_02b <- as_tibble(stage_02aa, .name_repair = "unique")
      stage_02c <- list(UID = stage_02a[1], static.data = stage_02a[2])
      stage_02d <- tribble(~records,
                     stage_02c)
      stage_03 <- bind_rows(stage_03, stage_02d)
    }
  }

  stage_04 <- stage_03 %>%
    mutate(info = lapply(records, unlist))

  stage_05 <- as_tibble(stage_04)

  stage_06 <- lapply(stage_05$info, extract_info) %>%
    bind_rows() %>%
    select(id = UID.UID,
           source = static.data.static_data.summary.titles.title,
           author = static.data.static_data.summary.names.name.display_name,
           type = static.data.static_data.summary.doctypes.doctype,
           pages = static.data.static_data.summary.pub_info.page,
           refs = static.data.static_data.fullrecord_metadata.references.reference.uid,
           subject = static.data.static_data.fullrecord_metadata.category_info.subjects.subject,
           bib = static.data.static_data.item.bib_id)

  stage_07 <- stage_06 %>%
    rowwise() %>%
    mutate(journal = source[1]) %>%
    mutate(art_title = source[length(source)]) %>%
    mutate(type = type[1]) %>%
    filter(type == "Article") %>%
    mutate(journal = str_to_title(journal),
           art_title = str_to_title(art_title)) %>%
    rowwise() %>%
    mutate(auth = case_when(length(author) == 1 ~ authadjust(author[1]),
                            length(author) == 2 ~ paste0(authadjust(author[1]), " and ", authadjust(author[2])),
                            TRUE ~ paste0(authadjust(author[1]), ", ", authadjust(author[2]), ", et al")
    )) %>%
    mutate(longcite = paste(
      auth, art_title, journal, bib, sep = ", "
    )) %>%
    mutate(year = as.numeric(str_sub(bib, -4))) %>%
    mutate(shortcite = paste(
      str_to_title(str_extract(author[1],".+(?=,)")),
      year,
      sep = " "
    ))

  stage_08 <- stage_07 %>%
    distinct(id, .keep_all = TRUE)
  save(stage_08, file = paste0("/Users/weath/Documents/citations-2024/raw_data/",temp_name,".RData"))
}

