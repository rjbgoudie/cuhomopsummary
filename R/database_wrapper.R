
#' Summarise an entire OMOP database
#' 
#' @param source Either a list of data frames or a DBI connection
#' @param schema The schema name (if using DBI)
#' @export
summarise_omop_database <- function(source, schema = NULL) {
  is_db <- inherits(source, 'DBIConnection')
  table_names <- if(is_db) DBI::dbListTables(source) else names(source)
  
  concept_tbl <- NULL
  if (is_db) {
    if ('concept' %in% table_names) {
      concept_tbl <- dplyr::tbl(source, if(!is.null(schema)) dbplyr::in_schema(schema, 'concept') else 'concept')
    }
  } else if ('concept' %in% names(source)) {
    concept_tbl <- source$concept
  }

  results <- purrr::map(table_names, function(nm) {
    data_tbl <- if(is_db) {
      dplyr::tbl(source, if(!is_null(schema)) dbplyr::in_schema(schema, nm) else nm)
    } else {
      source[[nm]]
    }
    summarise_omop_table(nm, data_tbl, concept_tbl)
  })
  
  names(results) <- table_names
  return(results)
}
