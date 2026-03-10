is_all_na_null_or_zero <- function(x, col, is_concept_id_col) {
  if (is_concept_id_col) {
    x |>
      mutate(is_na = is.null(.data[[col]]) | .data[[col]] == 0L) |>
      distinct(is_na) |>
      pull(is_na) |>
      all()
  } else {
    x |>
      mutate(is_na = is.null(.data[[col]])) |>
      distinct(is_na) |>
      pull(is_na) |>
      all()
  }
}

is_all_na_null_or_zero <- function(x, col, is_concept_id_col) {
  if (is_concept_id_col) {
    x |>
      summarise(is_na = any(!is.null(.data[[col]]) | .data[[col]] != 0L)) |>
      pull(is_na)
  } else {
    x |>
      mutate(is_na = is.null(.data[[col]])) |>
      distinct(is_na) |>
      pull(is_na) |>
      all()
  }
}


is_whole_column_na_null_or_zero <- function(x, col, is_concept_id_col) {
  all_na <- x |>
    head(1000) |>
    is_all_na_null_or_zero({{ col }}, is_concept_id_col)
  if (!all_na) {
    FALSE
  } else {
    all_na <- x |>
      is_all_na_null_or_zero({{ col }}, is_concept_id_col)

    if (!all_na) {
      FALSE
    } else {
      TRUE
    }
  }
}


n_distinct_concepts <- function(x, col) {
  x |>
    distinct(.data[[col]]) |>
    count() |>
    pull(n)
}

n_concept_zero <- function(x, col) {
  x |>
    filter(.data[[col]] == 0) |>
    count() |>
    pull(n)
}

concept_cross_tab <- function(x, col, concept_table) {
  x |>
    count(.data[[col]], sort = TRUE) |>
    inner_join(concept_table, by = rlang::set_names("concept_id", col))
}

#' Summarise a single OMOP table
#'
#' @param table_name Name of the table
#' @param data A data frame or a tbl_sql object
#' @param concept_table The OMOP concept table (optional)
#' @return A list containing row counts, column summaries, and plots
#' @export
summarise_omop_table <- function(table_name, data, concept_table = NULL) {
  # Row Count
  cli::cli_progress_step("{table_name}: Calculating row count")

  have_concept_table <- !is.null(concept_table)

  row_count <- data |>
    dplyr::tally() |>
    dplyr::collect() |>
    dplyr::pull(n)

  # Column Summaries
  cols <- colnames(data)

  col_summary <- purrr::map_df(cols, function(col) {
    is_concept_id_col <- str_detect(col, "_concept_id")

    vocabs_used <- NA
    n_distinct_concepts_counts <- NA
    n_concept_zero_counts <- NA
    cross_tab <- NA
    if (have_concept_table && is_concept_id_col) {
      cli::cli_progress_step("{table_name}.{col}: Forming cross-tab")

      cross_tab <- data |>
        concept_cross_tab(col, concept_table = concept_table) |>
        collect()

      cli::cli_progress_step("{table_name}.{col}: Checking vocabs")
      vocabs_used <- cross_tab |>
        distinct(vocabulary_id) |>
        pull(vocabulary_id)

      cli::cli_progress_step("{table_name}.{col}: Counting distinct concepts")
      n_distinct_concepts_counts <- nrow(cross_tab)

      cli::cli_progress_step("{table_name}.{col}: Counting concept = 0")
      n_concept_zero_counts <- cross_tab |>
        filter(.data[[col]] == 0) |>
        pull(n)

      cli::cli_progress_step("{table_name}.{col}: Checking all NA or 0")
      all_na_null_or_zero <- nrow(cross_tab) == 0 & all(cross_tab[[col]] == 0)
    } else {
      cli::cli_progress_step("{table_name}.{col}: Checking all NA or 0")
      all_na_null_or_zero <- data |>
        is_whole_column_na_null_or_zero(col, is_concept_id_col = is_concept_id_col)
    }

    list(
      table = table_name,
      row_count = row_count,
      column = col,
      all_na_null_or_zero = all_na_null_or_zero,
      vocabs_used = list(vocabs_used),
      cross_tab = list(cross_tab),
      n_distinct_concepts = n_distinct_concepts_counts,
      n_concept_zero = n_concept_zero_counts
    )
  })

  bind_rows(col_summary)
}
