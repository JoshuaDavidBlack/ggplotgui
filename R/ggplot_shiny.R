#' Creating a graphical user interface for creating ggplot-graphs.
#'
#' @param dataset A dataset (optional).
#' @return A GUI for visualizing data from \code{dataset}.
#' @examples
#' #ggplot_shiny()
#' #ggplot_shiny(mpg)
#' @import ggplot2
#' @import glue
#' @import dplyr
#' @import shiny
#' @import readxl
#' @import haven
#' @import RColorBrewer
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom stringr str_replace_all
#' @importFrom readr read_delim locale
#' @importFrom DT renderDT DTOutput
#' @importFrom stats cor.test t.test wilcox.test

#' @export
ggplot_shiny <- function( dataset = NA ) {

  ui <- fluidPage(
    img(src="assets/UoC.png", style = "max-width: 100%; max-height: 75px;"),

    headerPanel("ARTS102 Data Visualisation Interface"),

    sidebarPanel(width = 3,


      # Data upload panel
      conditionalPanel(
        condition = "input.tabs=='Data upload'",
        h4("Data upload"),
        radioButtons(
          "data_input",
          "",
          choices = if (is.data.frame(dataset)) {
            list("Load sample data" = 1, # Ideally, change these from digits to meaningful names.
                 "Upload text file" = 2,
                 "Paste data" = 3,
                 "Data passed through R environment" = 4)
              } else {
              list("Load sample data" = 1,
                   "Upload file" = 2,
                   "Paste data" = 3)
              },
          selected = if (is.data.frame(dataset)) 4 else 1
        ),
        # Panel on if default dataset used.
        conditionalPanel(
          condition = "input.data_input=='1'",
          h5("Tutorial sample data loaded")
        ),

        # Panel to allow user to upload data.
        conditionalPanel(
          condition = "input.data_input=='2'",
          h5("Upload file: "),
          fileInput("upload", "", multiple = FALSE),
          selectInput("file_type", "Type of file:",
                      list("text (csv)" = "text",
                           "Excel" = "Excel"
                           # # Removing these options as distracting for students.
                           #"SPSS" = "SPSS",
                           #"Stata" = "Stata",
                           #"SAS" = "SAS"
                           ),
                      selected = "text"),
          # If csv, panel asks for details of the csv file.
          conditionalPanel(
            condition = "input.file_type=='text'",
            selectInput("upload_delim", "Delimiter:",
                        list("Semicolon" = ";",
                             "Tab" = "\t",
                             "Comma" = ",",
                             "Space" = " "),
                        selected = "Semicolon"),
            selectInput("upload_dec", "Decimal mark:",
                        list("Comma" = ",",
                             "Point" = "."),
                        selected = "Comma")
          ),
          # Button to submit the user file.
          actionButton("submit_datafile_button",
                       "Submit datafile")
          ),

        # Option for pasting data directly.
        conditionalPanel(
          condition = "input.data_input=='3'",
          h5("Paste data below:"),
          tags$textarea(id = "data_paste",
                        placeholder = "Add data here",
                        rows = 10,
                        cols = 20, ""),
          actionButton("submit_data_button", "Submit data"),
          # Format of pasted data required.
          selectInput("text_delim", "Delimiter:",
                      list("Semicolon" = ";",
                           "Tab" = "\t",
                           "Comma" = ",",
                           "Space" = " "),
                      selected = "Semicolon"),
          selectInput("text_dec", "Decimal mark:",
                      list("Comma" = ",",
                           "Point" = "."),
                      selected = "Comma")
        )
      ),


      # Plot panel
      conditionalPanel(
        condition = "input.tabs=='Plot'",
        h4("Create visualization"),
        # Select variable type.
        selectInput(inputId = "Type",
                    label = "Type of graph:",
                    choices = c(
                      "Boxplot", "Density", "Histogram",
                      "Scatter", "Violin"
                    ),
                    selected = "Violin"
        ),
        # Set y variable
        selectInput("y_var", "Y-variable", choices = ""), # Presumably choices set by a function below?

        # Working by excluding cases/ The two plots without x vars, hist and density, do not let you choose an xvar.
        conditionalPanel(
          condition = "input.Type!='Density' && input.Type!='Histogram'",
          selectInput("x_var", "X-variable", choices = "")
        ),

        # Allow selection of group/colour aesthetic.
        selectInput("group", "Group (or colour)", choices = ""),

        # Select special cases (positively) - if boxplot, violin, or dot error
        # allow for dataplots to be placed on the plot (jittered).
        conditionalPanel(
          condition = "input.Type == 'Boxplot' || input.Type == 'Violin'",
          checkboxInput(inputId = "jitter",
                        label = strong("Show data points (jittered)"),
                        value = FALSE),
          checkboxInput(inputId = "stat_out",
                        label = strong("Show statistical tests (if applicable)."),
                        value = FALSE)
        ),

        # Allow notch option for boxplots.
        conditionalPanel(
          condition = "input.Type == 'Boxplot'",
          checkboxInput(inputId = "notch",
                        label = strong("Notched box plot"),
                        value = FALSE)
        ),

        # Allow opacity control for hist and density.
        conditionalPanel(
          condition = "input.Type == 'Density' || input.Type == 'Histogram'",
          sliderInput("alpha", "Opacity:", min = 0, max = 1, value = 0.8)
        ),
        # Allow binwidth fo histogram and dotplots.
        conditionalPanel(
          condition = "input.Type == 'Histogram' || input.Type=='Dotplot'",
          numericInput("binwidth", "Binwidth:", value = 1)
        ),
        # Dotplot direction args.
        conditionalPanel(
          condition = "input.Type == 'Dotplot'",
          selectInput("dot_dir", "Direction stack:",
                      choices = c("up", "down", "center", "centerwhole"),
                      selected = "up")
        ),
        # # Density and violin get bandwith adjustment.
        # conditionalPanel(
        #   condition = "input.Type == 'Density' || input.Type == 'Violin'",
        #   sliderInput(inputId = "adj_bw",
        #               label = "Bandwidth adjustment:",
        #               min = 0.01, max = 2, value = 1, step = 0.1)
        # ),

        # Add regression line.
        conditionalPanel(
          condition = "input.Type == 'Scatter'",
          checkboxInput(inputId = "line",
                        label = strong("Show line"),
                        value = FALSE
          ),
          conditionalPanel(
            condition = "input.line == true",
            selectInput("smooth", "Smoothening function",
                        choices = c("lm", "loess")) # Need to just have lm and loess here.
          ),
          conditionalPanel(
            condition = "input.line == true",
            checkboxInput(inputId = "se",
                          label = strong("Show confidence interval"),
                          value = FALSE)
          )
        ),
      ),


      # Description of usage.
      conditionalPanel(
        condition = "input.tabs=='Info'",
        h4("Info") # Better to just have nothing here?
      )
    ),

    # # This is text which sits above the main tabs.
    # h6("For more info see the 'Info'-tab or visit",
    #    a("https://github.com/gertstulp/ggplotgui",
    #      href = "https://github.com/gertstulp/ggplotgui")),

#####################################
########### OUPUT TABS ##############
#####################################

    mainPanel(width = 6,
      tabsetPanel(
        type = "tabs",
        # Outputs a table of the data.
        tabPanel("Data upload", DT::DTOutput("out_table")),
        # The ggplot panel defined here.
        tabPanel("Plot",
                 mainPanel(
                   downloadButton("download_plot_PDF",
                                  "Download pdf of figure"),
                   plotOutput(
                     "out_ggplot",
                     height = "auto"
                    ),
                   htmlOutput(
                     "out_html",
                     container=div
                    )
                )
        ),
        # Defines all info text here. Will have to manually update.
        tabPanel("Info",
h3("App info"),
p(
  "This application was created for ARTS102 on the basis of ",
  a("ggplotgui", href = "https://github.com/gertstulp/ggplotgui"),
  " by ",
  a("Gert Stulp", href = "http://www.gertstulp.com/"),
  ". For more information see ",
  a("https://github.com/JoshuaDavidBlack/ggplotgui", href = "https://github.com/JoshuaDavidBlack/ggplotgui"),
),
p(
  "Many thanks to the School of Mathematics and Statistics at the University of ",
  "Canterbury for hosting this interactive on their ",
  a("shinyapps.io", href="https://www.shinyapps.io"),
  " account."
)

        ),
        id = "tabs"
      )
    ),

#####################################
######### AESTHETICS TAB ############
#####################################

    conditionalPanel(
      condition = "input.tabs=='Plot'",
      sidebarPanel(
        width = 3,
        h4("Change aesthetics"),
        tabsetPanel(
          tabPanel(
            "Text",
            checkboxInput(inputId = "label_axes",
                          label = strong("Change labels axes"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = "label x-axis")
            ),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_y", "Y-axis:", value = "label y-axis")
            ),
            checkboxInput(inputId = "add_title",
                          label = strong("Add title"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.add_title == true",
              textInput("title", "Title:", value = "Title")
            ),
            checkboxInput(inputId = "adj_fnt_sz",
                          label = strong("Change font size"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_ttl",
                           "Size axis titles:",
                           value = 12),
              numericInput("fnt_sz_ax",
                           "Size axis labels:",
                           value = 10)
            ),
            checkboxInput(inputId = "rot_txt",
                          label = strong("Rotate text x-axis"),
                          value = FALSE),
            checkboxInput(inputId = "adj_fnt",
                          label = strong("Change font"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt == true",
              selectInput("font", "Font",
                          choices = c("Courier",
                                      "Helvetica",
                                      "Times"),
                          selected = "Helvetica")
            )
          ),
          tabPanel(
            "Theme",
            conditionalPanel(
              condition = "input.group != '.'",
              checkboxInput(inputId = "adj_col",
                            label = strong("Change colours"),
                            value = FALSE),
              conditionalPanel(
                condition = "input.adj_col",
                selectInput(inputId = "palet",
                            label = strong("Select palette"),
                            choices = list(
                              "Qualitative" = c("Accent",
                                                "Dark2",
                                                "Paired",
                                                "Pastel1",
                                                "Pastel2",
                                                "Set1",
                                                "Set2",
                                                "Set3"),
                              "Diverging" = c("BrBG",
                                              "PiYG",
                                              "PRGn",
                                              "PuOr",
                                              "RdBu",
                                              "RdGy",
                                              "RdYlBu",
                                              "RdYlGn",
                                              "Spectral"),
                              "Sequential" = c("Blues",
                                               "BuGn",
                                               "BuPu",
                                               "GnBu",
                                               "Greens",
                                               "Greys",
                                               "Oranges",
                                               "OrRd",
                                               "PuBu",
                                               "PuBuGn",
                                               "PuRd",
                                               "Purples",
                                               "RdPu",
                                               "Reds",
                                               "YlGn",
                                               "YlGnBu",
                                               "YlOrBr",
                                               "YlOrRd")),
                            selected = "set1")
              )
            ),
            conditionalPanel(
              condition = "input.jitter",
              checkboxInput("adj_jitter",
                            strong("Change look jitter"), FALSE),
              conditionalPanel(
                condition = "input.adj_jitter",
                textInput("col_jitter", "Colour (name or RGB):",
                          value = "black"),
                numericInput("size_jitter", "Size:", value = 1),
                sliderInput("opac_jitter", "Opacity:",
                            min = 0, max = 1, value = 0.5, step = 0.01),
                sliderInput("width_jitter", "Width jitter:",
                            min = 0, max = 0.5, value = 0.25, step = 0.01)
              )
            ),
            checkboxInput("adj_grd",
                          strong("Remove gridlines"), FALSE),
            conditionalPanel(
              condition = "input.adj_grd",
              checkboxInput("grd_maj",
                            strong("Remove major gridlines"), FALSE),
              checkboxInput("grd_min",
                            strong("Remove minor gridlines"), FALSE)
            ),
            selectInput("theme", "Theme",
                        choices = c("bw" = "theme_bw",
                                    "classic" = "theme_classic",
                                    "dark" = "theme_dark",
                                    "grey" = "theme_grey",
                                    "light" = "theme_light",
                                    "line_draw" = "theme_linedraw",
                                    "minimal" = "theme_minimal"),
                        selected = "theme_bw()")
          ),
          tabPanel(
            "Legend",
            conditionalPanel(
              condition = "input.group != '.'",
              radioButtons(inputId = "adj_leg",
                           label = NULL,
                           choices = c("Keep legend as it is",
                                       "Remove legend",
                                       "Change legend"),
                           selected = "Keep legend as it is"),
              conditionalPanel(
                condition = "input.adj_leg=='Change legend'",
                textInput("leg_ttl", "Title legend:",
                          value = "title legend"),
                selectInput("pos_leg", "Position legend",
                            choices = c("right",
                                        "left",
                                        "top",
                                        "bottom"))
              )
            )
          ),
          tabPanel(
            "Size",
            checkboxInput("fig_size",
                          strong("Adjust plot size on screen"), FALSE),
            conditionalPanel(
              condition = "input.fig_size",
              numericInput("fig_height", "Plot height (# pixels): ",
                           value = 480),
              numericInput("fig_width", "Plot width (# pixels):", value = 480)
            ),
            checkboxInput("fig_size_download",
                          strong("Adjust plot size for download"), FALSE),
            conditionalPanel(
              condition = "input.fig_size_download",
              numericInput("fig_height_download",
                           "Plot height (in cm):", value = 14),
              numericInput("fig_width_download",
                           "Plot width (in cm):", value = 14)
            ),
            checkboxInput("adj_axis_lims",
                          strong("Adjust axis limits"), FALSE),
            conditionalPanel(
              condition = "input.adj_axis_lims",
              numericInput(
                "xmin",
                "Minimum x value:",
                value = -1
              ),
              numericInput(
                "xmax",
                "Maximum x value:",
                value = 1
              ),
              numericInput(
                "ymin",
                "Minimum y value:",
                value = -1
              ),
              numericInput(
                "ymax",
                "Maximum y value:",
                value = 1
              )
            )

          )
        ) # Close tabsetPanel
      ) # Close sidebarPanel
    ) # Close conditionalPanel
  ) # Close fluidPage

  server <- function(input, output, session) {

#####################################
### GET VARIABLE NAMES FOR INPUT ####
#####################################

    observe({
      nms <- names(df_shiny())
      # Make list of variables that are not factors
      nms_cont <- names(Filter(function(x) is.integer(x) ||
                                 is.numeric(x) ||
                                 is.double(x),
                               df_shiny()))

      avail_all <- c("No groups" = '.', nms)
      avail_con <-
        if (identical(nms_cont, character(0)))
          c("No continuous vars available" = '.')
        else c(nms_cont)
      avail_fact <- names(Filter(function(x) is.character(x) ||
                                   is.factor(x),
                                 df_shiny()))

      if (input$Type == 'Violin' ||
          input$Type == 'Boxplot') {
        x_choices <- c("No x-var" = " ", avail_fact)
        y_choices <- avail_con
        group_choices <- c("No groups" = '.', avail_fact)
      } else if (input$Type == "Scatter") {
        x_choices <- c("No x-var" = " ", avail_con)
        y_choices <- avail_con
        group_choices <- c("No groups" = '.', avail_fact)
      } else {
        x_choices <- c("No x-var" = " ", nms)
        y_choices <- avail_con
        group_choices <- c("No groups" = '.', avail_fact)
      }

      updateSelectInput(session, "y_var", choices = y_choices)
      updateSelectInput(session, "x_var", choices = x_choices)
      updateSelectInput(session, "group", choices = group_choices)
    })


    # sensible defaults for xlims and ylims. Note only works for numeric variables.
    observe({
      plot_data <- df_shiny() %>% # This little hack is no good.
        mutate(
          ` ` = 'x'
        )
      updateNumericInput(session, "xmin", value = min(plot_data[[input$x_var]]))
      updateNumericInput(session, "xmax", value = max(plot_data[[input$x_var]]))
      updateNumericInput(session, "ymin", value = min(plot_data[[input$y_var]]))
      updateNumericInput(session, "ymax", value = max(plot_data[[input$y_var]]))
    })


#####################################
###### READ IN / GET DATA ###########
#####################################

    df_shiny <- reactive({
      if (input$data_input == 1) {
        # Version with mpg as sample data.
        # data <- ggplot2::mpg %>%
        #   mutate( # Little hack to test t-test and wilcox test functions.
        #     year = as.factor(year)
        #   )
        data <- ggplotgui::tut_data
      } else if (input$data_input == 2) {
        file_in <- input$upload
        # Avoid error message while file is not uploaded yet
        if (is.null(input$upload)) {
          return(data.frame(x = "Select your datafile"))
        } else if (input$submit_datafile_button == 0) {
          return(data.frame(x = "Press 'submit datafile' button"))
        } else {
          # All external file import methods.
          isolate({
            if (input$file_type == "text") {
              data <- read_delim(file_in$datapath,
                                 delim = input$upload_delim,
                                 locale = locale(decimal_mark = input$upload_dec),
                                 col_names = TRUE,
                                 n_max=5000)
            } else if (input$file_type == "Excel") {
              data <- read_excel(file_in$datapath, n_max=5000)
            } else if (input$file_type == "SPSS") {
              data <- read_sav(file_in$datapath, n_max=5000)
            } else if (input$file_type == "Stata") {
              data <- read_dta(file_in$datapath, n_max=5000)
            } else if (input$file_type == "SAS") {
              data <- read_sas(file_in$datapath, n_max=5000)
            }
          })
        }
      } else if (input$data_input == 3) {
        if (input$data_paste == "") {
          data <- data.frame(x = "Copy your data into the textbox,
                             select the appropriate delimiter, and
                             press 'Submit data'")
        } else {
          if (input$submit_data_button == 0) {
            return(data.frame(x = "Press 'submit data' button"))
          } else {
            isolate({
              data <- read_delim(input$data_paste,
                                 delim = input$text_delim,
                                 locale = locale(decimal_mark = input$text_dec),
                                 col_names = TRUE,
                                 n_max=5000)
            })
          }
        }
      } else if (input$data_input == 4){
        data <- dataset
      }
      return(data)
    })

#####################################
####### CREATE GRAPH-CODE ###########
#####################################

    generate_plot <- reactive({

      # Variable used for how to deal with x/y in ggplot
      one_variable <- (input$Type == "Histogram" ||
                input$Type == "Density")
      # Variable used for how to deal with colour/fill
      fill_geom <- (input$Type == "Histogram" ||
                input$Type == "Density" ||
                input$Type == "Dotplot")

      # Deal with mapping first

      # Is it a one variable or a two variable plot. App I am forking uses
      # y_var to refer to what I would call x_var in one-variable plots.
      # TODO: fix this after recreating the original output.
      if (one_variable) {
        out_mapping <- aes(
          x = .data[[input$y_var]],
          fill = .data[[input$group]],
          colour = .data[[input$group]]
        )
      } else {
        out_mapping <- aes(
          x = .data[[input$x_var]],
          y = .data[[input$y_var]],
          fill = .data[[input$group]],
          colour = .data[[input$group]])
      }

      # Deal with colour or fill of non-grouped plots.
      # If grouped AND a fill geom, then no colour mapping.
      # If grouped AND not a fill geom, then no fill mapping.
      # If not grouped, then no fill or colour mapping.
      if (input$group != '.') {
        if (fill_geom) {
          out_mapping$colour <- NULL
        } else {
          out_mapping$fill <- NULL
        }
      } else{
        out_mapping$fill <- NULL
        out_mapping$colour <- NULL
      }

      # Obtain plot data and add column to handle cases where no x is selected.
      # Remove any data in which x or y are NA.
      plot_data <- df_shiny() %>%
        mutate(
          ` ` = 'x'
        ) %>%
        filter(
          !is.na(.data[[input$x_var]]),
          !is.na(.data[[input$y_var]])
        )

      # Remove NA values from grouping variable if groups selected.
      if (input$group != '.') {
        plot_data <- plot_data %>%
          filter(
            !is.na(.data[[input$group]])
          )
      }

      # Set up shared part of plot object
      out_plot <- ggplot(
        data = plot_data,
        mapping = out_mapping
      )

      # Add geoms as required.
      if (input$Type == "Histogram") {
        out_plot <- out_plot + geom_histogram(
          position = 'identity',
          alpha = input$alpha,
          binwidth = input$binwidth
        )
      } else if (input$Type == "Density") {
        out_plot <- out_plot + geom_density(
          position = 'identity',
          alpha = input$alpha
          # adjust = input$adj_bw
        )
      } else if (input$Type == "Boxplot") {
        out_plot <- out_plot + geom_boxplot(
          notch = input$notch
        )

        # # If two groups in x variable, output t test and wilcox test as
        # # annotation
        # if (length(unique(plot_data[[input$x_var]])) == 2) {
        #   annotation <- generate_test_annotation(
        #     plot_data,
        #     input$x_var,
        #     input$y_var
        #   )
        #
        #   out_plot <- out_plot + annotation
        # }

      } else if (input$Type == "Violin") {
        out_plot <- out_plot + geom_violin(
          # adjust = input$adj_bw
        )

        # # TODO: Refactor all annotation handling to avoid copy paste.
        # # If two groups in x variable, output t test and wilcox test as
        # # annotation
        # if (length(unique(plot_data[[input$x_var]])) == 2) {
        #   annotation <- generate_test_annotation(
        #     plot_data,
        #     input$x_var,
        #     input$y_var
        #   )
        #
        #   out_plot <- out_plot + annotation
        # }
      } else if (input$Type == "Scatter") {
        out_plot <- out_plot + geom_point()

        if (input$line) {
          # annotation <- generate_coeff_annotation(
          #   plot_data,
          #   input$x_var,
          #   input$y_var,
          #   input$smooth
          # )

          out_plot <- out_plot +
            geom_smooth(
              se = input$se,
              method = input$smooth
            ) # +
            # annotation
        }
      }

      # Prevent geom_jitter from being added if not applicable
      if (fill_geom || input$Type == "Scatter") {
        jitt <- FALSE
      } else {
        jitt <- input$jitter
      }

      # Add geom_jitter
      if (jitt) {
        out_plot <- out_plot + geom_jitter(
          size = input$size_jitter,
          alpha = input$opac_jitter,
          width = input$width_jitter,
          colour = input$col_jitter
        )
      }

      # Handle labels.
      if (input$label_axes) {
        out_plot <- out_plot +
          labs(
            x = input$lab_x,
            y = input$lab_y
          )
      }

      # if title specified
      if (input$add_title) {
        out_plot <- out_plot + labs(
          title = input$title
        )
      }

      # if legend label change specified
      if (input$adj_leg == "Change legend") {

        if (fill_geom) {
          legend_label = labs(
            fill = input$leg_ttl
          )
        } else {
          legend_label = labs(
            colour = input$leg_ttl
          )
        }

        out_plot <- out_plot + legend_label
      }

      # If colour scheme given
      if (input$adj_col) {
        if (fill_geom) {
          out_plot <- out_plot + scale_fill_brewer(
            palette = input$palet
          )
        } else{
          out_plot <- out_plot + scale_colour_brewer(
            palette = input$palet
          )
        }
      }

      # Handle themes

      # Theme selection
      out_plot <- out_plot + match.fun(input$theme)()

      # For other modifications, set up empty theme. All required modifications
      # will be added to the theme object and the resulting theme added to
      # out_plot.
      out_theme <- theme()

      # font size:
      if (input$adj_fnt_sz) {
        out_theme$axis.title <- element_text(size = input$fnt_sz_ttl)
        out_theme$axis.text = element_text(size = input$fnt_sz_ax)
      }
      if (input$adj_fnt) {
        out_theme$text = element_text(family = input$font)
      }
      if (input$rot_txt) {
        out_theme$axis.text.x = element_text(angle = 45, hjust = 1)
      }
      if (input$adj_leg == "Remove legend") {
        out_theme$legend.position = 'none'
      }
      if (input$adj_leg == "Change legend") {
        out_theme$legend.position = input$pos_leg
      }
      if (input$grd_maj) {
        out_theme$panel.grid.major = element_blank()
      }
      if (input$grd_min) {
        out_theme$panel.grid.minor = element_blank()
      }

      out_plot <- out_plot + out_theme

      # Handle changes in x and y limits.
      if (input$adj_axis_lims) {
        out_plot <- out_plot +
          coord_cartesian(
            xlim = c(input$xmin, input$xmax),
            ylim = c(input$ymin, input$ymax)
          )
      }

      out_plot

    })

    generate_annotation <- reactive({

      # Inefficient
      plot_data <- df_shiny() %>%
        mutate(
          ` ` = 'x'
        ) %>%
        filter(
          !is.na(.data[[input$x_var]]),
          !is.na(.data[[input$y_var]])
        )

      # Handle case with scatterplot.
      if (input$Type == "Scatter" && input$line) {
        # Case if no groups
        if (input$group == ".") {
          out_html <- generate_coeff_text(
            plot_data,
            input$x_var,
            input$y_var,
            input$smooth
          )
        } else {
          groups <- plot_data %>%
            # remove NAs for group.
            filter(
              !is.na(.data[[input$group]])
            ) %>%
            pull(.data[[input$group]]) %>%
            unique()

          out_html <- ""

          for (i in seq_along(groups)) {
            group_data <- plot_data %>%
              filter(
                .data[[input$group]] == groups[[i]]
              )

            group_values <- generate_coeff_text(
              group_data,
              input$x_var,
              input$y_var,
              input$smooth
            )

            out_html <- glue(
              "{out_html}",
              "<b>{input$group}: {groups[[i]]}</b>",
              "{group_values}"
            )

          }
        }
      # Handle boxplot or violin case.
      } else if ((input$Type == "Boxplot" ||
                 input$Type == "Violin") &&
                 length(unique(plot_data[[input$x_var]])) == 2 &&
                 input$stat_out) {
        if (input$group == ".") {
          out_html <- generate_test_text(
            plot_data,
            input$x_var,
            input$y_var
          )
        } else if (input$group != input$x_var) {
          groups <- plot_data %>%
            # remove NAs for group.
            filter(
              !is.na(.data[[input$group]])
            ) %>%
            pull(.data[[input$group]]) %>%
            unique()

          out_html <- ""

          for (i in seq_along(groups)) {
            group_data <- plot_data %>%
              filter(
                .data[[input$group]] == groups[[i]]
              )

            group_values <- generate_test_text(
              group_data,
              input$x_var,
              input$y_var
            )

            out_html <- glue(
              "{out_html}",
              "<b>{input$group}: {groups[[i]]}</b>",
              "{group_values}"
            )

          }
        } else {
          out_html <- "<p>Statistical tests not displayed when group variable is the same as x variable.</p>"
        }
      } else {
        out_html <- ""
      }

      out_html
    })

#####################################
###### GRAPHICAL/TABLE OUTPUT #######
#####################################

    output$out_table <- DT::renderDT(
      df_shiny()
    )

    width <- reactive ({ input$fig_width })
    height <- reactive ({ input$fig_height })
    width_download <- reactive ({ input$fig_width_download })
    height_download <- reactive ({ input$fig_height_download })

    output$out_ggplot <- renderPlot(
      width = width,
      height = height,
      {
        p <- generate_plot()
        p
      }
    )

    output$out_html <- renderUI(HTML(glue("{generate_annotation()}")))

#####################################
#### GENERATE OUTPUT PLOTS #####
#####################################

  # Also evaluates code here.
  output$download_plot_PDF <- downloadHandler(
      filename <- function() {
        paste("Figure_ggplotGUI_", Sys.time(), ".pdf", sep = "")
      },
      content <- function(file) {
        p <- generate_plot()
        ggsave(file, p, width = width_download(),
               height = height_download(), units = "cm")
      },
      contentType = "application/pdf" # MIME type of the image
  )

    # End R-session when browser closed
    session$onSessionEnded(stopApp)
  }
  shinyApp(ui, server)
}


### Helper function for adding correlation and p-values to scatter plots.
generate_coeff_annotation <- function(
  plot_data,
  x_var,
  y_var,
  smooth_method) {
  if (smooth_method == "lm") {
    method <- 'pearson'
    tag <- "Pearson's r:"
  } else {
    method <- 'spearman'
    tag <- "Spearman's rho:"
  }

  plot_cor <- cor.test(
    plot_data[[x_var]],
    plot_data[[y_var]],
    method = method
  )

  annotation <- annotate(
    "label",
    label = glue(
      "{tag} {signif(plot_cor$estimate[[1]], digits=2)}\n",
      "p-value: {signif(plot_cor$p.value, digits=2)}"
    ),
    x=Inf,
    y = Inf,
    vjust=1,
    hjust=1,
    alpha = 0.5
  )
  annotation
}

# Helper to generate annotations for t-test and wilcox test results for violin
# plot and box plots. Assumes only two unique values for x (tested before
# calling)
generate_test_annotation <- function(plot_data, x_var, y_var) {
  x_values <- unique(plot_data[[x_var]])
  vector_1 <- plot_data %>%
    filter(
      .data[[x_var]] == x_values[[1]]
    ) %>%
    pull(y_var)
  vector_2 <- plot_data %>%
    filter(
      .data[[x_var]] == x_values[[2]]
    ) %>%
    pull(y_var)

  t_result <- t.test(
    vector_1,
    vector_2,
    alternative = 'two.sided',
    paired = FALSE,
    var.equal = FALSE
  )
  wilcox_result <- wilcox.test(
    vector_1,
    vector_2,
    alternative = 'two.sided',
    paired = FALSE
  )

  annotation <- annotate(
    "label",
    label = glue(
      "Non-paired t-test: \n",
      "\t test statistic: {signif(t_result$statistic[[1]], digits=2)}\n",
      "\t p-value: {signif(t_result$p.value, digits=2)}\n",
      "Non-paired Wilcox test: \n",
      "\t test statistic: {signif(wilcox_result$statistic[[1]], digits=2)}\n",
      "\t p-value: {signif(wilcox_result$p.value, digits=2)}"
    ),
    x=Inf,
    y=Inf,
    vjust=1,
    hjust=1,
    alpha = 0.75
  )
  annotation
}


### Helper function for generating correlation and p-values text to display below scatter plots..
generate_coeff_text<- function(
  plot_data,
  x_var,
  y_var,
  smooth_method) {
  if (smooth_method == "lm") {
    method <- 'pearson'
    tag <- "Pearson's r:"
  } else {
    method <- 'spearman'
    tag <- "Spearman's rho:"
  }

  plot_cor <- cor.test(
    plot_data[[x_var]],
    plot_data[[y_var]],
    method = method
  )

  out_text <- glue(
    "<ul>",
    "<li><b>{tag}</b> {signif(plot_cor$estimate[[1]], digits=2)}</li>",
    "<li><b>p-value:</b> {signif(plot_cor$p.value, digits=2)}</li>",
    '</ul>'
  )

  out_text
}


# Helper to generate text output for t-test and wilcox test results for violin
# plot and box plots. Assumes only two unique values for x (tested before
# calling)
generate_test_text <- function(plot_data, x_var, y_var) {
  x_values <- unique(plot_data[[x_var]])
  vector_1 <- plot_data %>%
    filter(
      .data[[x_var]] == x_values[[1]]
    ) %>%
    pull(y_var)
  vector_2 <- plot_data %>%
    filter(
      .data[[x_var]] == x_values[[2]]
    ) %>%
    pull(y_var)

  t_result <- t.test(
    vector_1,
    vector_2,
    alternative = 'two.sided',
    paired = FALSE,
    var.equal = FALSE
  )
  wilcox_result <- wilcox.test(
    vector_1,
    vector_2,
    alternative = 'two.sided',
    paired = FALSE
  )

  out_text <- glue(
    "<p>t-test:</p>",
    "<ul>",
    #"<li>Test statistic: {signif(t_result$statistic[[1]], digits=2)}</li>",
    "<li>p-value: {signif(t_result$p.value, digits=2)}</li>",
    "</ul>",
    "<p>Wilcox test:</p>",
    "<ul>",
    #"<li>Test statistic: {signif(wilcox_result$statistic[[1]], digits=2)}</li>",
    "<li>p-value: {signif(wilcox_result$p.value, digits=2)}</li>",
    "</ul>"
  )

  out_text
}
