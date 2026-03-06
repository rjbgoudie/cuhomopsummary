extract_queries_for_plugin <- function(plugin, name) {
  cli::cli_progress_step("Extracting queries for {name}")

  queries <- list()

  # Temporarily override collect() function to give us the query used
  rlang::local_bindings(
    collect = function(x) {
      query <- sql_render(
        x,
        sql_options = sql_options(
          cte = FALSE,
          use_star = FALSE,
          qualify_all_columns = TRUE
        )
      )
      # Add the query to the queries list
      # Note <<- assigns in the PARENT scope
      queries <<- c(queries, query)

      x |>
        dplyr::collect()
    },
    dbGetQuery = function(conn, statement, ...) {
      queries <<- c(queries, statement)
      DBI::dbGetQuery(
        conns = conns,
        statement = statement,
        ...
      )
    }
  )

  conns <- conns
  cohort <- cohort
  result <- eval(body(plugin$mapper), envir = environment())

  queries
}

extract_queries <- function(omop_plugins) {
  # This function is copied from omop_es
  enabled_by_settings <- function(plugin) {
    some_intersection <- function(x, y) length(intersect(x, y)) > 0
    source_enabled <- some_intersection(plugin$source, settings$enabled_sources)
    tags_enabled <- some_intersection(plugin$tags, settings$enabled_tags) | is.na(plugin$tags)
    source_enabled & tags_enabled
  }

  # This function is copied from omop_es
  check_type <- function(plugin) stopifnot(any(class(plugin) == "omop_plugin"))

  names(omop_plugins) |>
    map(function(table) {
      cli::cli_h1("Extracting queries for {table}")

      omop_plugins[[table]] |>
        walk(check_type) |>
        keep(enabled_by_settings) |>
        imap(extract_queries_for_plugin)
    }) |>
    setNames(names(omop_plugins))
}

extract_caboodle_tables_for_plugin <- function(plugin, name) {
  cli::cli_progress_step("Extracting tables for {name}")

  caboodle_tables <- list()

  # Temporarily override collect() function to give us the query used
  rlang::local_bindings(
    tbl = function(src, from, ...) {
      # Add the query to the queries list
      # Note <<- assigns in the PARENT scope
      caboodle_tables <<- c(caboodle_tables, from)

      x |>
        dplyr::tbl(src = src, from = from, ...)
    }
  )

  conns <- conns
  cohort <- cohort
  result <- eval(body(plugin$mapper), envir = environment())

  caboodle_tables
}

extract_caboodle_tables <- function(omop_plugins) {
  # This function is copied from omop_es
  enabled_by_settings <- function(plugin) {
    some_intersection <- function(x, y) length(intersect(x, y)) > 0
    source_enabled <- some_intersection(plugin$source, settings$enabled_sources)
    tags_enabled <- some_intersection(plugin$tags, settings$enabled_tags) | is.na(plugin$tags)
    source_enabled & tags_enabled
  }

  # This function is copied from omop_es
  check_type <- function(plugin) stopifnot(any(class(plugin) == "omop_plugin"))

  names(omop_plugins) |>
    map(function(table) {
      cli::cli_h1("Extracting tables for {table}")

      omop_plugins[[table]] |>
        walk(check_type) |>
        keep(enabled_by_settings) |>
        imap(extract_caboodle_tables_for_plugin)
    }) |>
    setNames(names(omop_plugins))
}
