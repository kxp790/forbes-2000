library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggthemes)
library(reshape)
library(leaflet)
library(maps)
library(rgeos)
library(rworldmap)
library(scales)
# Load data
forbes <- read.csv("data/Forbes2000.csv")

# Define UI ----
ui <- fluidPage(
  titlePanel("Forbes 2000 maailma suurimat ettevõtet aastal 2004"),
  helpText("Forbes 2000 maailma suurima ettevõtte aastal 2004 andmete visualiseerimine"),
  helpText("Õppejõud: Olga Dunajeva"),
  helpText("Autor: Killu-Smilla Palk"),
  
  tabsetPanel(
      tabPanel("Rakenduse kirjeldus",
          sidebarPanel(
              img(src = "pic.png", height = 150, width = 200),
              p("Andmed pärinevad leheküljelt ", a("Rdatasets", href = "https://vincentarelbundock.github.io/Rdatasets/doc/HSAUR/Forbes2000.html")
          )),
          mainPanel(
              h1("Rakenduse kirjeldus"),
              p("Töö visualiseerib 2004. aasta Forbes 2000 maailma suurima ettevõtte andmeid; vaatleb, millistes riikides on enim edetabelisse jõudnud riike; uurib, kas andmestikus välja toodud rahalised näitajad on selgelt mõjutanud pingerea järjestikku; annab ülevaadet enim esinenud ettevõtete tööala kategooriatest erinevates pingerea läbilõigetes. Rakendus koosneb viiest vahelehest ning vahelehed jagunevad järgnevalt:"),
              br(),
              p(HTML("<b>Andmed</b> – andmetabel")),
              p(HTML("<b>Tunnused</b> – ülevaade valitud tunnustest")),
              p(HTML("<b>Kaart</b> - populaarseimate riikide ülevaade Forbes 2000 edetabelis valitud arvu riikidega")),
              p(HTML("<b>Vara</b> - ettevõtete pingerea koha sõltuvus erinevatest rahalist seisu näitavatest tunnustest")),
              p(HTML("<b>Kategooriate edukus</b> - valitud pingerea vahemikus esinevaid kategooriaid")
          ))
      ),
      
      tabPanel("Andmed", 
          sidebarLayout(
              sidebarPanel(h4("Forbes 2000 maailma suurimat ettevõtet aastal 2004"),
                  p("2000 firmat järgneva 8 tunnusega:"),
                  p(HTML("<b>rank</b>: ettevõtte koht pingereas")),
                  p(HTML("<b>name</b>: ettevõtte nimi")),
                  p(HTML("<b>country</b>: riik, kus ettevõtte asub")),
                  p(HTML("<b>category</b>: tegur, mis kirjeldab ettevõtte toodetavaid tooteid")),
                  p(HTML("<b>sales</b>: ettevõtte müügisumma miljardites USA dollarites")),
                  p(HTML("<b>profits</b>: ettevõtte kasum miljardites USA dollarites")),
                  p(HTML("<b>assets</b>: ettevõtte varad miljardites USA dollarites")),
                  p(HTML("<b>marketvalue</b>: ettevõtte turuväärtus miljardites USA dollarites"))
          ),
          mainPanel(br(),dataTableOutput("tabel")) 
      )),         
      
      tabPanel("Tunnused",
          sidebarLayout(
               sidebarPanel(
                  h3("Tunnuste jaotumine"),
                    selectInput(
                        "parameter",
                        label = "Visualiseerimiseks saab valida tunnust:",
                        choices = names(forbes)[4:5],
                        selected = 1
                    )
               ),
               mainPanel(
                   h3(textOutput("feature_selected_parameter"), align="center"),
                   plotlyOutput("featurePlot")
               )
          )
      ),
      
      tabPanel("Kaart",
          sidebarLayout(
               sidebarPanel(
                    h3("Riikide esinemissagedus kaardil"),
                    sliderInput(inputId = "country_count", 
                                label = "TOP riikide vaatlemiseks saab piiritleda riikide arvu:", 
                                min = 3, 
                                max = 50, 
                                value = 20
                    ),
               ),
               mainPanel(
                     h3(textOutput("map_selected_parameter"), align="center"),
                     leafletOutput("map", height = 500)
               )
          )
      ),
      tabPanel("Vara",
          sidebarLayout(
              sidebarPanel(
                  h3("Summeeritud rahalised näitajad"),
                  p(strong("Müra vähendamiseks saab väärtusi peita:")),
                  checkboxInput("assets_checkbox", "Assets", TRUE),
                  checkboxInput("marketvalue_checkbox", "Turuväärtus", TRUE),
                  checkboxInput("sales_checkbox", "Müük", FALSE),
                  checkboxInput("profits_checkbox", "Kasum", FALSE),
              ),
              mainPanel(
                  h3("Rahalised parameetrid", align="center"),
                  plotlyOutput("financePlot")
              )
          )
      ),
      tabPanel("Kategooriate edukus",
           sidebarLayout(
               sidebarPanel(
                   h3("Kategooriate edukus valitud pingerea vahemikus"),
                   sliderInput("category_range", "Kategooriate täpsemaks vaatlemiseks saab valida ettevõtete pingerea vahemiku:",min = 1, max = 2000, value = c(1,10))
               ),
               mainPanel(
                 h3(textOutput("category_selected_parameter"), align="center"),
                 plotlyOutput("categoryPlot")
               )
            )
        )
    )
)

# Define server logic ----
server <- function(input, output) {
  ### ============================= ANDMED tabel ===============================
  output$tabel <- renderDataTable(forbes, options =
                                    list(searching = FALSE, ordering=F,
                                         lengthMenu = c(5, 10, 20),
                                         pageLength = 5, scrollX = TRUE))
  
  ### ============================== FEATURE ===================================
  output$feature_selected_parameter <- renderText({
    tekst <- if(input$parameter=="country") {
      "Riikide"
    } else if (input$parameter=="category"){
        "Kategooriate"
      } else if (input$parameter=="sales"){
        "Müügi"
      } else if (input$parameter=="marketvalue"){
        "Turuväärtuse"
      } 
    paste(tekst, "jaotuvus")})
  
  # make companies data frame and name columns
  forbes_filtered <- na.omit(forbes)
  countries <- as.data.frame(table(forbes_filtered$country)) 
  colnames(countries)[1] ="country"
  colnames(countries)[2] ="count"
  
  # correct country names 
  countries$country <- replace(as.character(countries$country), countries$country == "United States", "United States of America")
  countries$country <- replace(as.character(countries$country), countries$country == "Bahamas", "The Bahamas")
  countries$country <- replace(as.character(countries$country), countries$country == "Panama/ United Kingdom", "Panama")
  # countries$country <- replace(as.character(countries$country), countries$country == "Netherlands/ United Kingdom", "Netherlands")
  countries[countries$country == "South Korea", "count"] <- 
    countries[countries$country == "South Korea", "count"] + 
    countries[countries$country == "Korea", "count"]
  countries <- countries[!countries$country == "Korea", ]
  
  # add two-country fields to one of the countries (next step shows how much of what)
  countries[countries$country == "China", "count"] <- 
    countries[countries$country == "China", "count"] + 
    countries[countries$country == "Hong Kong/China", "count"] +
    countries[countries$country == "Kong/China", "count"]
  countries[countries$country == "France", "count"] <- 
    countries[countries$country == "France", "count"] + 
    countries[countries$country == "France/ United Kingdom", "count"]
  countries[countries$country == "United Kingdom", "count"] <- 
    countries[countries$country == "United Kingdom", "count"] + 
    countries[countries$country == "United Kingdom/ Australia", "count"] +
    countries[countries$country == "United Kingdom/ Netherlands", "count"] +
    countries[countries$country == "United Kingdom/ South Africa", "count"]
  countries[countries$country == "Australia", "count"] <- 
    countries[countries$country == "Australia", "count"] + 
    countries[countries$country == "Australia/ United Kingdom", "count"]
  countries[countries$country == "Netherlands", "count"] <- 
    countries[countries$country == "Netherlands", "count"] + 
    countries[countries$country == "Netherlands/ United Kingdom", "count"]

  # filter out two-country fields which have already been added to singular countries
  countries <- countries%>%
    filter(country!="Hong Kong/China")%>%               # 20 cases
    filter(country!="France/ United Kingdom")%>%        # 1 case
    filter(country!="Australia/ United Kingdom")%>%     # 2 cases
    filter(country!="Netherlands/ United Kingdom")%>%   # 2 cases
    filter(country!="Kong/China")%>%                    # 4 cases
    filter(country!="United Kingdom/ Australia")%>%     # 1 case
    filter(country!="United Kingdom/ Netherlands")%>%   # 1 case
    filter(country!="United Kingdom/ South Africa")     # 1 case
  
  # company plot by country
  countryPlot <- ggplot(data=countries, aes(x=count, y=reorder(country,count))) +
    geom_bar(stat="identity",color="black", width = 0.5)+
    xlab("Arv") +
    ylab("Riik")
  # company plot by category
  categories <- as.data.frame(table(forbes_filtered$category)) 
  categoryPlot <- ggplot(data=categories, aes(x=Freq, y=reorder(Var1,Freq))) +
    geom_bar(stat="identity",color="black", width = 0.5)+
    xlab("Arv") + 
    ylab("Kategooria")
  # render plot depending on input variable
  output$featurePlot <- renderPlotly({
    ggplotly(
      if(input$parameter =="country"){
        countryPlot 
      } else if(input$parameter =="category"){
        categoryPlot
      }
    ) %>% layout(height = 800)
  })
  
  # =================================== MAP ====================================
  output$map_selected_parameter <- renderText({paste(input$country_count, " kõige rohkem pingereas esinenud riiki")})
  output$map_selected_parameter <- renderText({
    tekst <- if(input$country_count==1) {
      "Kõige rohkem esinenud riik Forbes 2000 pingereas"
    } else {
      paste(input$country_count, " kõige rohkem esinenud riiki Forbes 2000 pingereas")
    } 
    paste(tekst)})
  
  # get world map and country coordinates
  wmap <- getMap(resolution="high")
  centroids <- gCentroid(wmap, byid=TRUE)
  latlngs <- as.data.frame(centroids)
  # move indexes to column
  latlngs <- tibble::rownames_to_column(latlngs, "country")
  # merge countries with coordinates
  countries_latlngs <- merge(x = countries, y = latlngs, by = "country")
  # add scaled count for rendering
  countries_latlngs$scaled_count <- rescale(countries_latlngs$count, to = c(5, 50)) 
  # order and filter by slider value
  countries_latlngs_ordered <- countries_latlngs[with(countries_latlngs,order(-count)),]

  Country = map("world", fill = TRUE, plot = FALSE, regions="USA", exact=TRUE)
  output$map <- renderLeaflet({
    countries_latlngs_slider_filtered <- countries_latlngs_ordered[1:input$country_count,]
    
    leaflet(Country) %>% 
      addTiles() %>%
      addCircleMarkers(lng = countries_latlngs_slider_filtered$x,
                       lat = countries_latlngs_slider_filtered$y, 
                       radius = countries_latlngs_slider_filtered$scaled_count, 
                       label = paste(countries_latlngs_slider_filtered$country, ": ", countries_latlngs_slider_filtered$count))
  })
  
  # ================================== FUNDS ===================================
  output$financePlot <- renderPlotly({
    forbes_filtered_subset_finance <- forbes_filtered[1:50,]

    ggplotly(
      ggplot(forbes_filtered_subset_finance, aes(rank), color=country) + 
        {if(input$sales_checkbox)geom_point(aes(y = sales, colour = "Müük"))} + 
        {if(input$profits_checkbox)geom_point(aes(y = profits, colour = "Kasum"))} +
        {if(input$assets_checkbox)geom_point(aes(y = assets, colour = "Vara"))} +
        {if(input$marketvalue_checkbox)geom_point(aes(y = marketvalue, colour = "Turuväärtus"))} +
        geom_line(aes(y = sales + profits + assets + marketvalue, colour = "Summeeritud rahalised näitajad")) +
        xlab("Koht pingereas") + 
        ylab("Varaline seis (miljard)")
    ) %>% layout(height = 600)
  })
  
  # ============================== CATEGORY RANK ===============================
  output$category_selected_parameter <- renderText({paste("Kategooriate esinemisarv pingereas kohtadevahemikus", input$category_range[1], " kuni ", input$category_range[2])})
  
  output$categoryPlot <- renderPlotly({
    forbes_filtered_subset_category <- forbes[input$category_range[1]:input$category_range[2],]
    categories_rank <- as.data.frame(table(forbes_filtered_subset_category$category))
    plottt <- ggplot(data=categories_rank, aes(x=Freq, y=reorder(Var1,Freq))) +
      geom_bar(stat="identity",color="green", width = 0.5) +
      ylab("Kategooria") +
      xlab("Koht pingereas") 
    
    ggplotly(
      ggplot(data=categories_rank, aes(x=Freq, y=reorder(Var1,Freq))) +
        geom_bar(stat="identity",color="#a6b5c0",fill="#4d6c81", width = 0.5) +
        ylab("Kategooria") +
        xlab("Koht pingereas") 
    ) %>% layout(height = 600)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)