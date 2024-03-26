## ---- create-grepl-terms

create_grepl_terms <- function(search_list_raw){
  
  search_list_raw[["grepl_search_term"]] <- 
    ifelse(search_list_raw[["operator"]] == "AND", 
           paste0(search_list_raw[["term_a"]],
                  ".",
                  "*",
                  search_list_raw[["term_b"]], 
                  "|",
                  search_list_raw[["term_b"]], 
                  ".", 
                  "*",
                  search_list_raw[["term_a"]]))
  
  return(search_list_raw)
  
}

## ---- extract-grepl-terms

extract_grepl_terms <- function(all_terms){
  
  search_terms <- list()
  
  for(i in 1:nrow(all_terms)){
    
    search_terms[[i]] <- all_terms[i, c("grepl_search_term")]
    
    
  }
  
  return(search_terms)
  
}

## ---- apply-grepl-terms

apply_grepl_terms <- function(database, grepl_search_terms, search_section = "abstract"){
  
  search_output_list <- list()
  
  for(i in 1:length(grepl_search_terms)){
    
    search_output_list[[i]] <- database[grepl(grepl_search_terms[[i]], 
                                              database[[search_section]]), ]
    
  }
  
  return(search_output_list)
  
}

## ---- filter-grepl-output

filter_grepl_output <- function(grepl_search_output, threshold = 100){
  
  retain_list <- list()
  
  refine_list <- list()
  
  for(i in 1:length(grepl_search_output)){
    
    if(nrow(grepl_search_output[[i]]) < threshold){
      
      retain_list[[i]] <- grepl_search_output[[i]]
      
    } else{
      
      refine_list[[i]] <- grepl_search_output[[i]]
      
    }
    
  }
  
  return(list(retain_list, refine_list))
  
}

## ---- edit-grepl-output

edit_grepl_output <- function(grepl_search_output){
  
  data.table_rbindlist <- 
    as.data.frame(data.table::rbindlist(grepl_search_output))
  
  references <- 
    data.table_rbindlist[!duplicated(data.table_rbindlist), ]
  
  return(references)
  
}

## ---- refine-grepl-output

refine_grepl_output <- function(grepl_search_output_large, refine_terms){
  
  grepl_search_output_large <- edit_grepl_output(grepl_search_output_large)
  
  refined_output <- apply_grepl_terms(grepl_search_output_large, refine_terms)
  
  refined_output_filter <- filter_grepl_output(refined_output, threshold = 50)
  
  refined_output_small <- refined_output_filter[[1]] 
  
  refined_output_final <- edit_grepl_output(refined_output_small)
  
  return(refined_output_final)
  
}

## ---- collate-final-list

collate_final_list <- function(initial_list, refined_list){
  
  final_list <- list()
  
  final_list[[1]] <- initial_list
  
  final_list[[2]] <- refined_list
  
  final_list_output <- edit_grepl_output(final_list)
  
  final_list_output <- final_list_output[, c("record_id", "year","title", "abstract", "doi")]
  
  return(final_list_output)
  
}

## ---- extract-hc-database

extract_hc_database <- function(database, input_search_list, additional_terms){
  
  grepl_terms <- create_grepl_terms(input_search_list)
  
  grepl_terms_extracted <- extract_grepl_terms(grepl_terms)
  
  grepl_search_output <- apply_grepl_terms(database, grepl_terms_extracted)
  
  filtered_output <- filter_grepl_output(grepl_search_output)
  
  initial_output <- edit_grepl_output(filtered_output[[1]])
  
  refined_output <- refine_grepl_output(filtered_output[[2]], refine_terms = additional_terms)
  
  extraction <- collate_final_list(initial_output, refined_output)
  
  return(extraction)
  
}

## ---- subset-scopus-reviews

subset_scopus_reviews <- function(scopus_results){
  
  scopus_results_subset <- scopus_results[scopus_results$Document.Type == "Review", ] 
  
  return(scopus_results_subset)
  
}

## ---- refine-scopus-results

refine_scopus_results <- function(input_search_list, scopus_results){
  
  grepl_terms <- create_grepl_terms(input_search_list)
  
  grepl_terms_extracted <- extract_grepl_terms(grepl_terms)
  
  grepl_search_output <- apply_grepl_terms(scopus_results, 
                                           grepl_terms_extracted, 
                                           "Indexed.Keywords")
  
  scopus_extraction <- edit_grepl_output(grepl_search_output)
  
  return(scopus_extraction)
  
}

## ---- extract-scopus-results

extract_scopus_results <- function(input_search_list, scopus_results){
  
  scopus_subset <- subset_scopus_reviews(scopus_results)
  
  scopus_refined <- refine_scopus_results(input_search_list, scopus_subset)
  
  return(scopus_refined)
  
}

## ---- subset-var-records

subset_var_records <- function(record_input){
  
  record_output <- record_input[, c("title", "year", "doi")]
  
}

## ---- edit-scopus-input

edit_scopus_input <- function(scopus_input){
  
  names(scopus_input)[names(scopus_input) == "Titles"] <- "title"
  names(scopus_input)[names(scopus_input) == "Year"] <- "year"
  names(scopus_input)[names(scopus_input) == "DOI"] <- "doi"
  
  scopus_output <- subset_var_records(scopus_input)
  
  return(scopus_output)
  
}

## ---- edit-pubmed-input

edit_pubmed_input <- function(pubmed_input){
  
  names(pubmed_input)[names(pubmed_input) == "Title"] <- "title"
  names(pubmed_input)[names(pubmed_input) == "Publication.Year"] <- "year"
  names(pubmed_input)[names(pubmed_input) == "DOI"] <- "doi"
  
  pubmed_output <- subset_var_records(pubmed_input)
  
  return(pubmed_output)
  
}

## ---- combine-records

combine_records <- function(a, b, c){
  
  all_records <- rbind(a, b, c)
  
  all_records <- all_records[order(all_records[,'title']), ]
  
  all_records <- all_records[!duplicated(all_records$title), ]
  
  return(all_records)
  
}

## ---- create-lit-list

create_lit_list <- function(hc_output, scopus_output, pubmed_output, project){
  
  project <- "paper_four"
  
  folder <- paste0("C:/Users/vanandkuma/OneDrive - Jacobs University/output/", project)
  
  file <- paste0("/", project, "_literature_review_output.xlsx")
  
  a <- subset_var_records(hc_output)
  
  b <- edit_scopus_input(scopus_output)
  
  c <- edit_pubmed_input(pubmed_output)
  
  xlsx::write.xlsx(a, 
             file = paste0(folder, file), 
             sheetName = "hc_database", 
             col.names = TRUE, 
             row.names = FALSE, 
             append = TRUE)
  
  xlsx::write.xlsx(b, 
             file = paste0(folder, file), 
             sheetName = "scopus", 
             col.names = TRUE, 
             row.names = FALSE, 
             append = TRUE)
  
  xlsx::write.xlsx(c, 
             file = paste0(folder, file), 
             sheetName = "pub_med_mesh", 
             col.names = TRUE, 
             row.names = FALSE, 
             append = TRUE)
  
  output <- combine_records(a, b, c)
  
  xlsx::write.xlsx(output, 
             file = paste0(folder, file), 
             sheetName = "title_screened", 
             col.names = TRUE, 
             row.names = FALSE, 
             append = TRUE)
  
  return(output)
  
}

