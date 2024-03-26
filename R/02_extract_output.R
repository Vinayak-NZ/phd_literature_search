## ---- extract-articles
hc_extraction <- extract_hc_database(database = co_creation_database, 
                            input_search_list = paper_four_search, 
                            additional_terms = list("healthcare"))

scopus_extraction <- extract_scopus_results(input_search_list = paper_four_search, 
                                            scopus_results = paper_four_scopus_results)
