## ---- load-databases
co_creation_database <- read.csv("input/co_creation_database_v1.5.csv", 
                 header = TRUE, 
                 sep = ";")

paper_four_search <- read.csv("input/paper_four_search.csv")

paper_four_scopus_results <- read.csv("input/paper_four_scopus_results.csv")

paper_four_pubmed_results <- read.csv("input/paper_four_pubmed_results.csv")
