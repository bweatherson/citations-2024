require(tidyverse)
require(xml2)

rm(list = ls())

filelist <- list.files(pattern = "xml")
todolist <- filelist %>% str_replace(".xml","")

master_refs <- c()

start_time <- Sys.time()

for (todoname in todolist){

stage_01 <-read_xml(paste0(todoname,".xml"))

source_nodes <- xml_find_all(stage_01,".//d1:citedWork")

therefs <- c()

for (f in source_nodes){
  if (xml_text(f) == "J PHILOS" | xml_text(f) == "JOURNAL OF PHILOSOPHY" | xml_text(f)){
    y <- xml_child(xml_parent(f), 3)
    if (xml_text(y) > 1970 & xml_text(y) < 1975){
    h <- xml_parent(f)
      therefs <- bind_rows(
        therefs,
        as_tibble(as_list(h))
      )
    }
  }
}

#fixedrefs <- therefs %>%
#  rowwise() %>%
#  mutate(year = year[1],
#         uid = uid[1],
#         citedAuthor = citedAuthor[1],
#         page = ifelse(is.null(unlist(page)), "0", unlist(page)),
#         volume = volume[1],
#         citedWork = citedWork[1])

master_refs <- bind_rows(master_refs, therefs)

}

end_time <- Sys.time()

end_time - start_time

save(master_refs, file = "jphil_98-00.RData")

master_refs_fixed <- master_refs %>%
 rowwise() %>%
 mutate(year = year[1],
        uid = uid[1],
        citedAuthor = citedAuthor[1],
        page = ifelse(is.null(unlist(page)), "0", unlist(page)),
        volume = ifelse(is.null(unlist(volume)), "0", unlist(volume)),
        citedWork = citedWork[1])

save(master_refs_fixed, file = "jphil_98-00.RData")