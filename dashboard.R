library(shinydashboard)
library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DT)

#https://rstudio.github.io/shinydashboard/structure.html#body 
#Esta pagina contiene un tutorial de funciones del paquete.

#ValueBOX (Total de Canasta Básica)

ipc_datapanel

#Tutorial para paquetes de Shiny https://rstudio.github.io/shinydashboard/structure.html#structure-overview
#Tutorial para Dashboard Data Table https://shiny.posit.co/r/articles/build/datatables/


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Análisis de Precios"),
                    dashboardSidebar( sidebarMenu(
                      menuItem("Indice de Precios", tabName = "ipc", icon = icon("dashboard")),
                      menuItem("Productos", tabName = "tabla_detalle", icon = icon("table"))
                    )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "ipc",
                                h2(paste0("Canasta Básica en Dólares")),
                                fluidRow(
                                  valueBoxOutput("central_sum_box", width = 4),
                                  valueBoxOutput("plaza_sum_box", width = 4),
                                  valueBoxOutput("gama_sum_box", width = 4)
                                ),
                                h2(paste0("Canasta Básica en Bolívares")),
                                fluidRow(
                                  valueBoxOutput("central_sum_box_dol", width = 4),
                                  valueBoxOutput("plaza_sum_box_dol", width = 4),
                                  valueBoxOutput("gama_sum_box_dol", width = 4)
                                ),
                                fluidRow(
                                  box(
                                    title = "Evolución de Precios de la Canasta por Supermercado",
                                    status = "primary", solidHeader = TRUE, width = 12,
                                    plotOutput("precios_supermercados_ts", height = 500)
                                  )
                                )
                        ),
                        tabItem(tabName = "tabla_detalle",
                                fluidRow(
                                  box(
                                    title = "Tabla de Productos de la Canasta Básica",
                                    status = "primary", solidHeader = TRUE, width = 12,
                                    p("Aqui se encuentran los productos utilizados para la elaboracion de la canasta, en los tres supermercados"),
                                    DT::dataTableOutput("tabla_producto")
                                  )
                                )
                        ))))
                    
server <- function(input, output) {
  
  # Construcción de df_ipc dentro del server
  df_ipc <- bind_rows(df_datos, df_0, df_01, df_1, df_2, df_3, df_4) %>%
    select(categoria, marca, nombre, productID, subcategoria, precio, tienda, fecha, valor_dolar, precio_bs) %>%
    filter(productID %in% c(
      '0177943kejl','0688778optx','2641618farn','5048664dees','1024483riau','8472293qffh','4924587jfyq',
      '1740352zdgy','3766471ywnj','0911518zdav','6686895ahgv','8760443ykjc','8542675nksj','9772063bimy',
      '1766177glna','9749784sngv','0239406dlxq','4900792iskg','0847963tefn','0784532cajo','8367304bxdp',
      '6061140bgks','9989685wiow','2047677hulx','6088928jhpv','8639719lsdo','3931084tute','8700182dssr',
      '4306829tzkm','8388768ahuk','5326580nfkz','3394414nvey','0441922bxhp','9685540yucb','5391468yxyb',
      '2004318ktog','1255154xcbq','5383221flfl','7195430vlnt','6605107fwwz','3733323rpeq','8105706gvye',
      '5899485kuas','9203707wfno','2570585fktz','9888007ennt','0541816ugnt','1451804pngj','2240343zudb',
      '3733839dnep','6889212ognt','4723042vpcz','9733477wmxj'
    )) %>%
    filter(tienda %in% c('Central Madeirense', 'Gama', 'Plazas'))
  
  # Cálculo reactivo del panel agregado
  ipc_datapanel <- reactive({
    df_ipc %>%
      group_by(fecha, tienda) %>%
      reframe(ipc_dia_tienda = sum(precio), .groups = "drop")
  })
  
  # Gráfico de líneas
  output$precios_supermercados_ts <- renderPlot({
    ggplot(ipc_datapanel(), aes(x = fecha, y = ipc_dia_tienda, color = tienda)) +
      geom_line(linewidth = 1) +
      labs(
        title = "Evolución de Canasta por Supermercado",
        x = "Fecha",
        y = "Precio de Canasta Básica",
        color = "Supermercado"
      )+
      theme_minimal()
  })
  
  # ValueBoxes (esto depende de que existan las variables abajo)
  output$central_sum_box <- renderValueBox({
    valueBox(
      value = paste0("$ ", ipc_madeirense),
      "Canasta Básica: Central Madeirense",
      icon = icon("list"),
      color = "red"
    )
  })
  
  output$plaza_sum_box <- renderValueBox({
    valueBox(
      value = paste0("$ ", ipc_plazas),
      "Canasta Básica: Automercados Plaza´s",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$gama_sum_box <- renderValueBox({
    valueBox(
      value = paste0("$ ", ipc_gama),
      "Canasta Básica: GAMA",
      icon = icon("credit-card"),
      color = "orange"
    )
  })
  
  output$central_sum_box_dol <- renderValueBox({
    valueBox(
      value = paste0("Bs ", df_madeirense_bs),
      "Canasta Básica: Central Madeirense",
      icon = icon("list"),
      color = "red"
    )
  })
  
  output$plaza_sum_box_dol <- renderValueBox({
    valueBox(
      value = paste0("Bs ", df_plazas_bs),
      "Canasta Básica: Automercados Plaza´s",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$gama_sum_box_dol <- renderValueBox({
    valueBox(
      value = paste0("Bs ", df_gama_bs),
      "Canasta Básica: GAMA",
      icon = icon("credit-card"),
      color = "orange"
    )
  })
  
  # Tabla de productos 
  output$tabla_producto <- DT::renderDataTable({
    DT::datatable(df_producto, options = 
                    list(orderClasses = TRUE),
    )
  })
  
}
                    
                    shinyApp(ui, server)
                    
                    