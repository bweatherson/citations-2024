require(tidyverse)

load("/Users/weath/Documents/citations-2024/philo_bib_fix_without_jphil.RData")

# Set working directory (setwd) to wherever the jphil data is before running this
setwd("~/Documents/citations-2024/philos_process")
filelist <- list.files(pattern = "RData$", recursive = TRUE)

master_refs <- c()

for (filename in filelist){
  load(filename)
  master_refs <- bind_rows(master_refs, therefs)
}

master_refs_fixed <- master_refs %>%
  rowwise() %>%
  mutate(year = ifelse(is.null(unlist(year)), "0", unlist(year)),
         uid = ifelse(is.null(unlist(uid)), "0", unlist(uid)),
         citedAuthor = ifelse(is.null(unlist(citedAuthor)), "0", unlist(citedAuthor)),
         page = ifelse(is.null(unlist(page)), "0", unlist(page)),
         volume = ifelse(is.null(unlist(volume)), "0", unlist(volume)),
         citedWork = citedWork[1]) |>
  filter(
        citedWork == "Journal of Philosophy" |
        citedWork == "The Journal of Philosophy" |
        citedWork == "Journal of Philosophy." |
        citedWork == "The Journal of philosophy" |
        citedWork == "The Journal of Philosophy." |
        citedWork == "The Journal of Philosophy," |
        citedWork == "Journal of Philosophy " |
        citedWork == "J PHILOS" |
        citedWork == "J PHILOSOPHY" |
        citedWork == "J Philos" |
          citedWork == "J Philos." |
          citedWork == "J Philosophy" |
          citedWork == "J. Philos." |
          citedWork == "J. Philos" |
          citedWork == "J. Philosophy" |
          citedWork == "Causation. J. Philos." |
          citedWork == "The J. Philosophy" |
          citedWork == "J PHILOS         AUG" |
          citedWork == "J PHILOS         APR" |
          citedWork == "J PHILOS         JAN" |
          citedWork == "J PHILOS         MAY")



setwd("/Users/weath/Documents/citations-2024")
jphil_total <- master_refs_fixed

save(jphil_total, file = "jphil_total.RData")


