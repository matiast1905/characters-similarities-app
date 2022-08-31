library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(gt)
library(gtExtras)

chars_corr <- feather::read_feather("chars_corrs.feather")
chars_desc <- feather::read_feather("chars_desc.feather")

ui <- tagList(
  fluidPage(
    titlePanel("Most and Least Similar Characters:"),
    fluidRow(
      column(
        3,
        selectInput(
          inputId = "char_name",
          label = "Choose a Character:",
          choices = chars_desc$name,
          selected = "Harry Potter (Harry Potter)"
        )
      ),
      column(
        3,
        numericInput(
          inputId = "nrows",
          label = "Number of outputs:",
          value = 5,
          min = 1,
          max = 10
        ),
      ),
      column(
        6,
        gt_output(outputId = "char_table")
      )
    ),
    fluidRow(
      column(
        6,
        gt_output(outputId = "similar_table")
      ),
      column(
        6,
        gt_output(outputId = "least_similar_table")
      )
    ),
  ),
  tags$footer(
    "Author: @Matias_Taron | Data: #TidyTuesday Open-Source Psychometrics
        Project, courtesy of Tanya Shapiro",
    align = "right",
    style = "
            position:relative;
              bottom:2px;
            right:2%;
              height:20px;   /* Height of the footer */
              color: gray"
  )
)

server <- function(input, output) {
  correlation_character <- reactive({
    chars_desc |>
      filter(name == input$char_name) |>
      inner_join(chars_corr, by = c(id = "id1")) |>
      select(id2, correlation) |>
      inner_join(chars_desc, by = c(id2 = "id")) |>
      separate(name, c("Name", "Universe"), sep = " \\(") |>
      mutate(Universe = str_remove(Universe, "\\)"))
  })
  output$char_table <- render_gt(
    expr = {
      chars_desc |>
        filter(name == input$char_name) |>
        separate(name, c("Name", "Universe"), sep = " \\(") |>
        mutate(Universe = str_remove(Universe, "\\)")) |>
        select(Name,
          Universe,
          `Main Characteristics` = description,
          Picture = image
        ) |>
        gt() %>%
        gt_img_rows(columns = Picture, height = 100) |>
        cols_width(everything() ~ px(200)) |>
        cols_align("center") |>
        tab_header("Selected Character")
    },
    height = px(200)
  )
  output$similar_table <- render_gt({
    correlation_character() |>
      head(input$nrows) |>
      select(Name,
        Universe,
        Correlation = correlation,
        Picture = image
      ) |>
      gt() %>%
      tab_style(
        style = list(
          cell_text(
            color = "limegreen",
            weight = "bold"
          )
        ),
        locations = cells_body(
          columns = Correlation
        )
      ) |>
      gt_img_rows(columns = Picture, height = 100) |>
      fmt_number(Correlation, decimals = 3) |>
      cols_width(everything() ~ px(200)) |>
      cols_align("center") |>
      tab_header("Most similar characters")
  })
  output$least_similar_table <- render_gt({
    correlation_character() |>
      tail(input$nrows) |>
      arrange(correlation) |>
      select(Name,
        Universe,
        Correlation = correlation,
        Picture = image
      ) |>
      gt() %>%
      tab_style(
        style = list(
          cell_text(
            color = "red",
            weight = "bold"
          )
        ),
        locations = cells_body(
          columns = Correlation
        )
      ) |>
      gt_img_rows(columns = Picture, height = 100) |>
      fmt_number(Correlation, decimals = 3) |>
      cols_width(everything() ~ px(200)) |>
      cols_align("center") |>
      tab_header("Least similar characters")
  })
}

shinyApp(ui = ui, server = server)
