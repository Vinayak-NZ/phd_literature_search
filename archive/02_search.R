
search_index_one <- co_creation_database[grepl
                             ("co-creat*.*digital health|digital health.*co-creat*", 
                               co_creation_database$abstract), ]

search_index_two <- co_creation_database[grepl
                                         ("co-creat*.*eHealth|eHealth.*co-creat*", 
                                           co_creation_database$abstract), ]

search_index_one <- search_index_one[, c("record_id", 
                                         "title", 
                                         "abstract", 
                                         "doi", 
                                         "year")]

search_index_two <- search_index_two[, c("record_id", 
                                         "title", 
                                         "abstract", 
                                         "doi", 
                                         "year")]

search_index_n <- rbind(search_index_one, search_index_two)

search_index_n <- search_index_n[!duplicated(search_index_n), ]

term_one <- paper_four_search[1, ]

paper_four_search$paper_four_search <- 
  ifelse(paper_four_search$operator == "AND", 
         paste0(paper_four_search$term_a,
                ".",
                "*",
                paper_four_search$term_b, 
                "|",
                paper_four_search$term_b, 
                ".", 
                "*",
                paper_four_search$term_a))



search_terms_list <- list()

for(i in 1:length(search_terms)){
  
  search_terms_list[[i]] <- co_creation_database[grepl
                                                 (search_terms[[i]], 
                                                   co_creation_database$abstract), ]
  
}

data.table_rbindlist <- 
  as.data.frame(data.table::rbindlist(search_terms_list))

search_index_n <- 
  data.table_rbindlist[!duplicated(data.table_rbindlist), ]


test <- paper_four_search[1, c("paper_four_search")]

search_index_one <- co_creation_database[grepl
                                         (test, 
                                           co_creation_database$abstract), ]