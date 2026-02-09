#' Launch the cuhomop Shiny App
#'
#' @param summary_list The output from summarise_omop_database
#' @export
run_cuhomop_shiny <- function(summary_list) {
  ui <- bslib::page_navbar(
    title = 'cuhomop Explorer',
    theme = bslib::bs_theme(bootswatch = 'flatly'),
    bslib::nav_panel('Table Summary',
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput('table_sel', 'Select OMOP Table', choices = names(summary_list)),
          shiny::actionButton('next_btn', 'Next Table', class = 'btn-primary', icon = shiny::icon('arrow-right'))
        ),
        shiny::uiOutput('table_content')
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$next_btn, {
      curr <- which(names(summary_list) == input$table_sel)
      nxt <- if(curr == length(summary_list)) 1 else curr + 1
      shiny::updateSelectInput(session, 'table_sel', selected = names(summary_list)[nxt])
    })

    output$table_content <- shiny::renderUI({
      res <- summary_list[[input$table_sel]]

      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1,
          bslib::value_box(
            title = 'Total Row Count',
            value = format(res$count, big.mark = ','),
            showcase = bsicons::bs_icon('database-fill'),
            theme = 'primary'
          )
        ),
        bslib::card(
          bslib::card_header('Column & Concept Analysis'),
          DT::renderDataTable(DT::datatable(res$cols, options = list(pageLength = 10)))
        ),
        if(length(res$plots) > 0) {
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(bslib::card_header('Annual Volume'), shiny::renderPlot(res$plots$year)),
            bslib::card(bslib::card_header('Weekly Distribution'), shiny::renderPlot(res$plots$dow))
          )
        } else {
          bslib::card(shiny::p('No temporal data available for this table.', style='padding: 20px;'))
        }
      )
    })
  }

  shiny::shinyApp(ui, server)
}
