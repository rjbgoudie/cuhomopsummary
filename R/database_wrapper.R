#' Summarise an entire OMOP database
#'
#' @param source Either a list of data frames or a DBI connection
#' @param schema The schema name (if using DBI)
#' @export
summarise_omop_database <- function(source, schema = NULL) {
  is_db <- inherits(source, 'DBIConnection')

  cli::cli_h1('cuhomop: Initialising Summary')

  # Logic to get table names based on schema
  if (is_db) {
    if (!is.null(schema)) {
      cli::cli_alert_info('Fetching tables for schema: {.val {schema}}')
      # We query information_schema if possible, otherwise fallback to filtering ListObjects
      table_names <- tryCatch({
        DBI::dbGetQuery(source,
                        paste0("SELECT table_name FROM information_schema.tables WHERE table_schema = '", schema, "'")
        )[[1]]
      }, error = function(e) {
        cli::cli_alert_warning('information_schema not accessible, attempting driver fallback...')
        all_tabs <- DBI::dbListTables(source)
        # Often drivers return 'schema.table', so we filter for that
        all_tabs[grepl(paste0('^', schema, '\\\\.'), all_tabs)]
      })
    } else {
      cli::cli_alert_warning('No schema specified. Listing all accessible tables (may be slow).')
      table_names <- DBI::dbListTables(source)
    }
  } else {
    table_names <- names(source)
  }

  if (length(table_names) == 0) {
    cli::cli_abort('No tables found in the specified source/schema.')
  }

  concept_tbl <- NULL
  if (is_db) {
    if ('concept' %in% table_names) {
      concept_tbl <- dplyr::tbl(source, if (!is.null(schema)) dbplyr::in_schema(schema, 'concept') else 'concept')
    }
  } else if ('concept' %in% names(source)) {
    concept_tbl <- source$concept
  }

  table_names <- table_names |>
    stringr::str_subset("^concept", negate = TRUE) |>
    # Remove measurement for now
    stringr::str_subset("^measurement", negate = TRUE)

  results <- purrr::map(table_names, function(nm) {
    # Update progress bar status and increment
    cli::cli_h2(paste0('Analysing: ', nm))

    data_tbl <- if (is_db) {
      dplyr::tbl(source, if (!is.null(schema)) dbplyr::in_schema(schema, nm) else nm)
    } else {
      source[[nm]]
    }
    summarise_omop_table(nm, data_tbl, concept_tbl)
  })

  cli::cli_alert_success('Full database summary complete!')

  names(results) <- table_names
  return(results)
}
