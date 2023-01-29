library(data.table)
library(shinydashboard)
library(shiny)
library(plotly)
library(tibble)
library(dplyr)
library(DT)
library(foreach)
library(stringr)
library(rsconnect)


##############
# GET DATA
##############

rsconnect::setAccountInfo(name='ancmedina', token='1310A6804C30F2A4A27214A706F5454E', secret='Atn+yfVbdCqPuCsk/+qb85tmVpiUvWp+FqWc0cly')

#Read sunburst data
sun_data <-
  fread(
    "Sunburst_data.csv",
    sep = ";",
    sep2 = ",",
    colClasses = list("character" = 1)
  )

#Read collection data
data.types <-
  fread("Expandedcollectiondata.csv",
        nrows = 1,
        header = TRUE) %>% colnames()
num.vars <- which(data.types == "Numerical")
cat.vars <- which(data.types == "Categorical")
data <-
  fread(
    "Expandedcollectiondata.csv",
    sep = ";",
    sep2 = ".",
    colClasses = list("character" = 2),
    skip = 2,
    data.table = F
  )

#Read variable descriptions
var.legend <-
  (fread(
    "Expandedcollectiondata.csv",
    nrows = 3,
    drop = c(1, 2),
    data.table = F
  ) %>% t())[, c(3, 2)]
colnames(var.legend) <- c("Variable", "Description")

#Read CNR dictionary
cnr_dict <- as.list(sun_data$Cat)
names(cnr_dict) <- sun_data$CNR

#Read super cropgroups that are not a part of the path system
super.groups <-
  (sun_data[grep(sun_data$ParentCNR, pattern = "[A-Z][a-z]"), 2] %>% unique())$ParentCNR

descripcionED <- c(
  "Necesita prueba de germinacion"	,
  "Disponible"	,
  "Duplicado"	,
  "Requiere atencion de curador"	,
  "No hay germinacion"	,
  "No disponible"	,
  "Baja germinacion"	,
  "Baja poblacion"	,
  "El inventario no existe"	,
  "Eliminado de la coleccion"	,
  "Bajo inventario	"	,
  "No hay accesion, no existe"	,
  "Eliminado de la coleccion"  ,
  "Despues de monitores, baja germinacion"	,
  "Baja germinacion y poblacion"
)


valordescripcionED <- c("1"	,
                        "2"	,
                        "3"	,
                        "4"	,
                        "5"	,
                        "6"	,
                        "7"	,
                        "8"	,
                        "9"	,
                        "10"	,
                        "11"	,
                        "12"	,
                        "13"	,
                        "14"	,
                        "15")

VrDs <- cbind(descripcionED, valordescripcionED)



###########################################
# DEFINE UI
###########################################
header <- dashboardHeader(title = "BGV COLOMBIA", titleWidth = 250)

sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                              menuItem("Explore collection", tabName = "sunburst"),
                              menuItem("Export selection", tabName = "download_page"),
                              tags$head(tags$style(
                                HTML(
                                  '
                                  .logo {
                                  background-color: #005172 !important;
                                  }
                                  .navbar {
                                  background-color: #005172 !important;
                                  }
                                  .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
                                  border-left-color: #3A84C1;
                                  }
                                  .box.box-success {
                                  border-top-color: #A59D95;
                                  }

                                  '
                                )
                              ))
                            ))

body <- dashboardBody(tabItems(
  tabItem("sunburst",
          fluidRow(
            box(
              # DISPLAY SUNBURST
              
              title = p(
                div(style = "display: inline-block;vertical-align:top; width: 175px;", "Select group of interest"),
                div(style = "display: inline-block;vertical-align:top; width: 5px;"),
                div(style = "display: inline-block;vertical-align:top; width: 50px;", actionButton("root", "", icon = icon("undo", lib = "font-awesome" , verify_fa = FALSE), class = "btn-xs", title = "Back to start"))
                       ),
              
              
              div(
                style = "display:inline-block; vertical-align:top; width: 200px;",
                selectInput(
                  "sunburst_var",
                  "Seleccionar variable:",
                  choices = colnames(sun_data)[5:(colnames(sun_data) %>% grep(pattern = ".NA", invert = TRUE) %>% max())],
                  selected = "Estado de disponibilidad"
                )
              ),
              
              div(style = "display: inline-block;vertical-align:top; width: 60px;", HTML("<br>")),
              
              
              #numero de pisos
              
              div(
                style = "display:inline-block; vertical-align:top; width: 70px;",
                numericInput(
                  "sun_depth",
                  "Plot depth:",
                  min = 1,
                  max = 4,
                  step = 1,
                  value = 2.0
                )
              ),
              plotlyOutput("sunburst_plot"),
              width = 6,
              height = "755px"
            ),
            
            box(
              # DISPLAY VARIABLE DESCRIPTIONS
              
              dataTableOutput("var_legend_table"),
              status = "success",
              title = "Descripcion de variable",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 6
            ),
            #collapsed determines whether the box is collapsed at startup
            
            box(
              # DISPLAY VARIABLE DESCRIPTIONS
              
              dataTableOutput("germination_description"),
              status = "success",
              title = "Descripcion Estado de Disponibilidad",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 6,
              height = "auto"
            ),
            
            tabBox(
              tabPanel(
                # DISPLAY HISTOGRAM
                
                title = "Histogram analysis",
                div(
                  style = "display:inline-block; vertical-align:top; width: 150px;",
                  selectInput(
                    "hist_var",
                    "Choose a variable:",
                    choices = colnames(data)[num.vars],
                    selected = "Estado de disponibilidad"
                  )
                ),
                
                div(style = "display: inline-block;vertical-align:top; width: 60px;", HTML("<br>")),
                
                div(
                  style = "display: inline-block;vertical-align:top; width: 70px;",
                  numericInput(
                    "bin_size",
                    "Bin size:",
                    min = 0.2,
                    step = 0.1,
                    value = 1.0
                  )
                ),
                textOutput("pathText1"),
                plotlyOutput("var_hist"),
                width = 6
              ),
              
              tabPanel(
                # DISPLAY GERMINATION SCATTERPLOT
                
                title = "Germination",
                textOutput("pathText2"),
                plotlyOutput("germ_scatter")
              ),
              height = "650"
            ),
            
            box(
              # DISPLAY TABLE
              
              title = p(
                div(style = "display: inline-block;vertical-align:top; width: 150px;", "Selected accessions"),
                div(style = "display: inline-block;vertical-align:top; width: 5px;"),
                div(
                  style = "display: inline-block;vertical-align:top; width: 50px;",
                  actionButton(
                    "remove",
                    "",
                    icon = icon("undo", lib = "font-awesome", verify_fa = FALSE),
                    class = "btn-xs",
                    title = "Undo selection",
                    verify_fa = FALSE
                  )
                )
              ),
              dataTableOutput("sunburst_table"),
              width = 12,
            )
            
          )),
  
  tabItem("download_page",
          fluidPage(
            box(
              title = "Export your custom selection",
              "Use the filters to find a subset of accessions. Your list can be printed, or downloaded as CSV or Excel format. Due to the size of the collection, data processing happens server-side.
                    This means only the accessions visible in the table will be exported to the print or download function. Use the 'Show' button to expand the number of accessions exported.",
              status = "success",
              width = 12
            ),
            tabBox(
              width = 12,
              tabPanel(dataTableOutput("show_selection"), title = "Current selection"),
              tabPanel(dataTableOutput("show_all"), title = "All accessions")
            )
          ))
))

ui <- dashboardPage(header, sidebar, body)

###########################################
# DEFINE SERVER
###########################################

server <- function(input, output) {
  ##############
  # TEXT
  ##############
  
  # The PathText is saved in two variables, because Shiny doesn't support multiple outputs with the same name because that would generate HTML where two elements have the same ID, which is invalid HTML.
  output$pathText2 <- output$pathText1 <- renderText({
    paste("Showing accessions in group:", cnr_dict[[sunburst$path]], sep = " ")
  })
  
  
  ##############
  # MODALS
  ##############
  
  selection_modal <- modalDialog(
    title = "You created a selection",
    "Apply selection to datatable?",
    footer =  tagList(
      actionButton("apply", label = "Apply"),
      modalButton("Cancel")
    ),
  )
  
  multiple_sel_modal <-
    modalDialog(
      title = "Another selection is active",
      "You created a selection while another selection is active. What do you want to do?",
      footer = tagList(
        actionButton("combine", label = "Combine"),
        actionButton("intersect", label = "Intersect"),
        actionButton("replace", label = "Replace"),
        modalButton("Cancel")
      )
    )
  
  
  
  ##############
  # REACTIVES
  ##############
  
  
  sunburst <-
    reactiveValues(
      ids = sun_data$CNR,
      labels = sun_data$Cat,
      parents = sun_data$ParentCNR,
      values = sun_data$Count,
      var = sun_data$`Clases de gerninacion` ,
      path = "root",
      na.count = 0
    )
  
  
  sunSelection <- reactive({
    #filters data to only include accessions in the path of the sunburst
    if (sunburst$path == "root") {
      data
    }
    else if (sunburst$path %in% super.groups) {
      paths <-
        sun_data %>% filter(ParentCNR == sunburst$path) %>% pull(CNR)
      
      foreach(i = 1:length(paths), .combine = rbind) %do% {
        data[startsWith(data$PATH, prefix = paths[i]), ]
      }
    }
    else{
      data[startsWith(data$PATH, sunburst$path), ]
    }
  })
  
  hist <- reactiveValues(layout.type = "linear", xselect = "")
  
  scatter <- reactiveValues(pointselect = "")
  
  plot <- reactiveValues(selected = "")
  
  datatable <-
    reactiveValues(
      selection = data,
      selection_counter = 0,
      persistence = F
    )
  
  
  ##############
  # BUTTON HANDLERS
  ##############
  
  observeEvent(input$apply, {
    # Applies custom selection to datatable
    datatable$selection <- plot$selected
    datatable$selection_counter <- datatable$selection_counter + 1
    removeModal()
  })
  
  observeEvent(input$combine, {
    # Combines custom selection with previous custom selection
    datatable$selection <- union(datatable$selection, plot$selected)
    removeModal()
  })
  
  observeEvent(input$intersect, {
    # Applies the intersection of two custom selections to the datatable
    datatable$selection <-
      intersect(datatable$selection, plot$selected)
    removeModal()
  })
  
  observeEvent(input$replace, {
    # Replaces the custom selection in the datatable with another
    datatable$selection <- plot$selected
    removeModal()
  })
  
  observeEvent(input$remove, {
    # Removes the custom selection
    datatable$selection <- sunSelection()
    datatable$selection_counter <- datatable$selection_counter - 1
    removeModal()
  })
  
  observeEvent(input$root, {
    # When clicked, resets the sunburst diagram to 'CGN collection' at the centre
    sunburst$centre <- "root"
  })
  
  ##############
  # OTHER OBSERVERS
  ##############
  
  # When switching variables in the sunburst diagram: ignore groups and accessions missing data and recenter plot on the group of interest
  observeEvent(input$sunburst_var, priority = 10, {
    sunburst$na.count <-
      sun_data[[paste(input$sunburst_var, "NA", sep = ".")]]
    
    sunburst$values <- sun_data$Count - sunburst$na.count
    indices <- (sunburst$values > 0) %>% which()
    sunburst$values <- sunburst$values[indices]
    
    sunburst$ids <- sun_data$CNR[indices]
    sunburst$labels <- sun_data$Cat[indices]
    sunburst$parents <- sun_data$ParentCNR[indices]
    
    sunburst$var <- sun_data[[input$sunburst_var]][indices]
    
    if (sunburst$path %in% t(sun_data$ParentCNR)) {
      sunburst$centre <- sunburst$path
    }
    else{
      # this snippet prevents you from being locked in an end node of the sunburst when you switch variables
      sunburst$centre <- str_sub(sunburst$path, 1,-2)
    }
  })
  
  #when changing plot depth in the sunburst diagram: recenter plot on the group of interest
  observeEvent(input$sun_depth, priority = 10, {
    if (sunburst$path %in% t(sun_data$ParentCNR)) {
      sunburst$centre <- sunburst$path
    }
    else{
      # this snippet prevents you from being locked in an end node of the sunburst when you switch variables
      sunburst$centre <- str_sub(sunburst$path, 1,-2)
    }
  })
  
  #Retrieve sunburst click data, set sunburst path
  observeEvent(
    event_data(
      event = "plotly_sunburstclick",
      source = "sunSource",
      priority = "event"
    ),
    {
      clickdata <- unlist(
        event_data(
          event = "plotly_sunburstclick",
          source = "sunSource",
          priority = "event"
        )
      )[["customdata"]] %>% as.character()
      if (clickdata != sunburst$path) {
        #single click: sunburst$path is click
        sunburst$path <- clickdata
      }
      else{
        #double click: sunburst$path is parent
        sunburst$path <-
          sun_data$ParentCNR[which(sun_data$CNR == sunburst$path)]
      }
      if (datatable$selection_counter == 0) {
        # if no user selection is present, automatically update the datatable with accessions from the group of interest
        datatable$selection <- sunSelection()
      }
      sunburst$centre <- sunburst$path
      
    }
  )
  
  #Retrieve selection data from histogram
  observeEvent(event_data(
    event = "plotly_selected",
    source = "histSource",
    priority = "event"
  ),
  {
    hist$xselect <- event_data(event = "plotly_selected",
                               source = "histSource",
                               priority = "event")[["x"]]
    plot$selected <-
      sunSelection()[inrange(
        sunSelection()[[input$hist_var]],
        lower = (hist$xselect - 0.5),
        upper = (hist$xselect + 0.5)
      ), ]
    if (datatable$selection_counter == 0) {
      showModal(selection_modal)
    }
    else if (datatable$selection_counter > 0) {
      showModal(multiple_sel_modal)
    }
  })
  
  #Retrieve selection data from histogram
  observeEvent(
    event_data(
      event = "plotly_selected",
      source = "scatterSource",
      priority = "event"
    ),
    {
      scatter$pointselect <-
        event_data(event = "plotly_selected",
                   source = "scatterSource",
                   priority = "event")[["pointNumber"]]
      plot$selected <-
        sunSelection()[scatter$pointselect + 1, ]
      if (datatable$selection_counter == 0) {
        showModal(selection_modal)
      }
      else if (datatable$selection_counter > 0) {
        showModal(multiple_sel_modal)
      }
    }
  )
  
  
  ##############
  # RENDER DTs
  ##############
  
  output$var_legend_table <- renderDataTable({
    #Variables and descriptions on main page
    datatable(
      var.legend,
      rownames = FALSE,
      options = list(dom = "t"),
      selection = "none"
    )
  })
  
  output$germination_description <- renderDataTable({
    #Descripcion Germinatio Class
    datatable(
      VrDs,
      rownames = FALSE,
      options = list(dom = 't', pageLength = 25),
      selection = "none"
    )
    
  })
  
  output$sunburst_table <- renderDataTable({
    #Datatable at the bottom of the main page
    datatable$selection %>%
      select(-PATH)
  },
  options = list(dom = 'tp', pageLength = 10), selection = "single",
  rownames = FALSE)
  
  
  output$show_selection <- renderDataTable(
    #Datatable on page 'Export selection': Current selection
    datatable$selection %>%
      #mutate_at(.vars = cat.vars, .funs = as.factor) %>%
      select(-2) %>%            # removes path variable
      datatable(
        filter = "top",
        extensions = 'Buttons',
        options = list(
          pageLength = 50,
          columns.width = 50,
          dom = "Btip",
          buttons = list(
            "pageLength",
            "colvis",
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            ),
            "print"
          ),
          lengthMenu = list(c(10, 50, 150, 300,-1), c('10', '50', '150', '300', 'All'))
        ),
        rownames = FALSE
      )
  )
  
  output$show_all <- renderDataTable(
    #Datatable on page 'Export selection': All accessions
    data %>%
      #mutate_at(.vars = cat.vars, .funs = as.factor) %>%
      select(-2) %>%            # removes path variable
      datatable(
        filter = "top",
        extensions = 'Buttons',
        options = list(
          pageLength = 50,
          columns.width = 50,
          dom = "Btip",
          buttons = list(
            "pageLength",
            "colvis",
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            ),
            "print"
          ),
          lengthMenu = list(c(10, 50, 150, 300,-1), c('10', '50', '150', '300', 'All'))
        ),
        rownames = FALSE
      )
  )
  
  
  
  ##############
  # RENDER PLOTS
  ##############
  
  output$sunburst_plot <- renderPlotly({
    sun_data %>%
      plot_ly(
        source = "sunSource",
        ids = sunburst$ids,
        labels = sunburst$labels,
        customdata = sunburst$ids,
        parents = sunburst$parents,
        values = sunburst$values,
        type = "sunburst",
        branchvalues = "total",
        insidetextorientation = "auto",
        maxdepth = input$sun_depth,
        height = 600,
        
        
        marker = list(
          colors = sunburst$var,
          showscale = TRUE,
          colorscale = "Reds",
          reversescale = TRUE,
          colorbar = list(
            lenmode = "fraction",
            len = 0.5,
            thicknessmode = "fraction",
            thickness = 0.02,
            outlinewidth = 0.2,
            title = input$sunburst_var,
            tickprefix = ">",
            showtickprefix = "last",
            ypad = 15,
            tickfont = list(size = 10)
          ),
          cmin = 0,
          cmax = sunburst$var
        ),
        level = sunburst$centre,
        hoverinfo = "text",
        hovertext = ~ paste(
          sunburst$labels,
          "<br>",
          "Number of accessions: ",
          sunburst$values,
          "<br>"
        )
      ) %>%
      config(
        modeBarButtonsToRemove = c("toggleHover", "hoverCompareCartesian"),
        displaylogo = FALSE
      ) %>%
      layout(# annotations = list(text = "Excluding accessions and groups without data",
        #                                              font = list(color = "lightgrey", size = 1),
        #                                              x = 0.5, y = -0.1),
        margin = list(l = 30, b = 0, t = 0))
  })
  
  
  output$germ_scatter <- renderPlotly({
    ggplotly(
      source = "scatterSource",
      height = 500,
      (
        sunSelection() %>%
          group_by(`Estado de disponibilidad`) %>%
          ggplot(
            aes(x = `Estado de disponibilidad`, y = `Fecha de ultima prueba`, text = ARN)
          ) +
          geom_jitter(
            alpha = 0.15,
            shape = 1,
            colour = "#2ca02c",
            width = 0.3
          ) +
          labs(x = "Estado de disponbilidad", y = "Fechas de ultima prueba (Anios)") +
          theme_minimal()
      ),
      tooltip = "text"
    ) %>%
      layout(dragmode = "select") %>%
      config(
        modeBarButtonsToRemove = c(
          "pan2d",
          "zoom2d",
          "zoomIn2d",
          "zoomOut2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "toggleSpikelines",
          "autoScale2d"
        ),
        displaylogo = FALSE
      )
  })
  
  
  
  output$var_hist <- renderPlotly({
    sunSelection() %>%
      plot_ly(
        source = "histSource",
        height = 450,
        x = ~ get(input$hist_var),
        type = "histogram",
        hoverinfo = "text",
        hovertemplate = paste("Accessions: %{y}<br>",
                              "Value: %{x}",
                              "<extra></extra>"),
        xbins = list(size = input$bin_size),
        marker = list(
          color = "#2ca02c",
          opacity = 0.8,
          line = list(width = 0.3, color = "white")
        )
      ) %>%
      layout(
        xaxis = list(
          range = c(-0.5 * input$bin_size, (
            max(data[[input$hist_var]], na.rm = TRUE) + 0.5 * input$bin_size
          )),
          title = input$hist_var,
          tick0 = 0,
          dtick = 2 * input$bin_size,
          type = hist$layout.type
        ),
        yaxis = list(title = "Count", fixedrange = TRUE),
        dragmode = "select"
      ) %>%
      config(
        modeBarButtonsToRemove = c(
          "pan2d",
          "lasso2d",
          "zoomIn2d",
          "zoomOut2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "toggleSpikelines",
          "autoScale2d"
        ),
        displaylogo = FALSE
      )
  })
  
  
  observeEvent(input$hist_var, {
    # Make sure that the germination class (and only the germination class) is actually treated as categorical in the histogram
    if (grep("Germination", input$hist_var, ignore.case = TRUE) %>% length() != 0) {
      hist$layout.type <- "categorical"
    }
    else {
      hist$layout.type <- "linear"
    }
  })
  
}

##############
# LAUNCH APP
##############


#Buscar informacion de cracion de vectores para colores

shinyApp(ui, server)
