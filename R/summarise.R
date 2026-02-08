
#' Summarise a single OMOP table
#' 
#' @param table_name Name of the table
#' @param data A data frame or a tbl_sql object
#' @param concept_table The OMOP concept table (optional)
#' @return A list containing row counts, column summaries, and plots
#' @export
summarise_omop_table <- function(table_name, data, concept_table = NULL) {
  
  # Row Count
  row_count <- data |> dplyr::tally() |> dplyr::collect() |> dplyr::pull(n)
  
  # Column Summaries
  cols <- colnames(data)
  col_summary <- purrr::map_df(cols, function(col) {
    # Check completeness (all 0, NA, or NULL)
    null_count <- data |> 
      dplyr::filter(is.na(.data[[col]]) | .data[[col]] == 0) |> 
      dplyr::tally() |> 
      dplyr::collect() |> 
      dplyr::pull(n)
    
    is_incomplete <- if(row_count == 0) FALSE else (null_count == row_count)
    
    # Concept ID analysis
    concept_info <- 'N/A'
    if (grepl('_concept_id$', col) && !is.null(concept_table)) {
       concepts <- data |> 
         dplyr::select(!!rlang::sym(col)) |> 
         dplyr::distinct() |> 
         dplyr::inner_join(concept_table, by = rlang::set_names('concept_id', col)) |> 
         dplyr::collect()
       
       if(nrow(concepts) > 0) {
         vocabs <- paste(unique(concepts$vocabulary_id), collapse = ', ')
         std_perc <- mean(concepts$standard_concept == 'S', na.rm = TRUE)
         concept_info <- paste0('Vocabs: ', vocabs, ' | Std: ', round(std_perc*100), '%')
       }
    }
    
    data.frame(
      column = col,
      incomplete = is_incomplete,
      details = concept_info,
      stringsAsFactors = FALSE
    )
  })
  
  # Temporal Plots
  plots <- list()
  date_cols <- cols[grepl('_date$|_datetime$', cols)]
  
  if (length(date_cols) > 0 && row_count > 0) {
    date_col <- date_cols[1]
    df_dates <- data |> 
      dplyr::select(!!rlang::sym(date_col)) |> 
      dplyr::collect() |> 
      dplyr::mutate(
        date_val = as.Date(.data[[date_col]]),
        year = lubridate::year(date_val),
        dow = lubridate::wday(date_val, label = TRUE)
      ) |>
      dplyr::filter(!is.na(year))
    
    if(nrow(df_dates) > 0) {
      plots$year <- ggplot2::ggplot(df_dates, ggplot2::aes(x = year)) + 
        ggplot2::geom_bar(fill = '#2c3e50') + ggplot2::theme_minimal() +
        ggplot2::labs(title = paste('Events per Year:', table_name))
        
      plots$dow <- ggplot2::ggplot(df_dates, ggplot2::aes(x = dow)) + 
        ggplot2::geom_bar(fill = '#e74c3c') + ggplot2::theme_minimal() +
        ggplot2::labs(title = 'Events by Day of Week')
    }
  }
  
  list(name = table_name, count = row_count, cols = col_summary, plots = plots)
}
