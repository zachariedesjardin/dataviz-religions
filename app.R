library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)
library(shiny)
library(shinydashboard)

#Importation et traitement des données

setwd("/Documents/projet_religion")

religion <- read.csv("WRP_national.csv", header = TRUE, sep = ",")
pays <- read.csv("COW country codes.csv", header = TRUE, sep = ",")

pays$continent <- cut(pays$CCode,breaks=c(0,95,165,395,626,860,990))
levels(pays$continent) <- c("Amérique du Nord","Amérique du Sud","Europe","Afrique","Asie","Océanie")

pays <- pays %>% rename(state = CCode)
religion <- merge(pays,religion, by = "state")

religion <- religion[,1:43] %>% select(-c(state,StateAbb,sumrelig,name))
religion <- religion %>% rename(state = StateNme)
religion$state <- as.factor(religion$state)

religion[is.na(religion)] <- 0

religion_1945_2010 <- religion %>%
  pivot_longer(cols = !c(state, continent, year, pop), names_to = "religion", values_to = "value")










sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "tabDashboard",
             icon = icon("dashboard")),
    menuItem("Données", tabName = "tabDonnees",
             icon = icon("database")),  
    menuItem("Évolution mondiale", tabName = "tabGraphique1",
             icon = icon("chart-line")),
    menuItem("Histogramme par continent", tabName = "tabGraphique2",
             icon = icon("chart-bar")),
    menuItem("Évolution en France", tabName = "tabGraphique3",
             icon = icon("chart-area")),
    menuItem("Carte du monde", tabName = "tabGraphique4",
             icon = icon("globe-europe"))
  ))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tabDashboard",
            fluidRow(
                box(width = 12,
                    title = "Description",
                    p("Cette application a pour objectifs d'analyser l'évolution des principales religions entre 1945 et 2010."),
                    p("Les données proviennent du site", a("correlatesofwar.org",
                                                           href = "https://correlatesofwar.org/"),
                      ", voici les liens ci-dessous :"),
                    a("Jeu de donnée 1 : Religions", 
                           href = "https://correlatesofwar.org/data-sets/world-religion-data"),
                    br(),
                    a("Jeu de donnée 2 : Pays", 
                           href = "https://correlatesofwar.org/data-sets/cow-country-codes"),
                    br(),
                    br(),
                    p("Auteurs : Layla TAHER & Zacharie DESJARDIN - M1 SIAD Classique Groupe 2")
                         )
                ),
            fluidRow(
              box(width = 12,
                  title = "Évolution des principales religions depuis 1945",
                  solidHeader = TRUE,
                  status = "info",
                  plotOutput(outputId = "graph0")
                  )
              )
            ),
    
    tabItem(tabName = "tabDonnees",
              box(
                width = 4,
                title = "Sélection",
                
                selectInput(
                  inputId = "state",
                  label = "Pays",
                  multiple = TRUE,
                  choices = religion_1945_2010 %>%
                    select(state) %>%
                    distinct() %>% 
                    arrange(state),
                  selected = religion_1945_2010 %>% 
                    select(state) %>% 
                    distinct() %>% 
                    arrange(state)),
                
                sliderInput(
                  inputId = "year",
                  label = "Années",
                  min = 1945, max = 2010,
                  step = 5, sep = "",
                  value = 1945,
                  animate = TRUE)
              ),
              box(
                width = 8,
                title = "Données",
                tableOutput(outputId = "donnees")
                )
            ),
    
    tabItem(tabName = "tabGraphique1",
            fluidRow(
              box(title = "Évolution des principales religions depuis 1945",
                  solidHeader = TRUE,
                  width = 12,
                  status = "primary",
                  plotOutput("graph1"))
              ),
            fluidRow(
              box(width = 12,
                title = "Contrôle",
                selectInput(
                  inputId = "religion1",
                  label = "Religion :",
                  choices = c("Christianisme","Islam","Athéisme","Hindouisme","Boudhisme","Syncrétisme", "Shintoisme", "Judaisme")),
                hr(),
                helpText("Choisissez la religion à mettre en surbrillance.")
                )
              )
            ),
    
    tabItem(tabName = "tabGraphique2",
            fluidRow(
              box(title = "Répartition des principales religions par continent",
                  solidHeader = TRUE,
                  width = 12, 
                  status = "primary",
                  plotOutput("graph2")),
              
              box(title = "Contrôle",
                sliderInput(
                  inputId = "year2",
                  label = "Années",
                  min = 1945, max = 2010,
                  step = 5, sep = "",
                  value = 1945,
                  animate = TRUE),
              actionButton(inputId = "generate",
                           label = "Générer")
                )
              )
            ),
    
    tabItem(tabName = "tabGraphique3",
            fluidRow(
              box(title = "Écart du pourcentage de chrétien en France par rapport à la moyenne en Europe",
                  solidHeader = TRUE,
                  width = 12, 
                  status = "primary",
                  plotOutput("graph3")
                  ),
              )
            ),
    
    tabItem(tabName = "tabGraphique4",
            fluidRow(
              box(leafletOutput("graph4", height = 500), 
                  width = 12
                  ),
            ), 
            fluidRow(              
              box(title = "Contrôles",
                  width = 12,
                selectInput(
                  inputId = "religion4",
                  label = "Religion:",
                  choices = c("Christianisme","Islam","Athéisme","Hindouisme","Boudhisme","Syncrétisme", "Shintoisme", "Judaisme")),
                hr(),
                helpText("Choisissez une religion."),
              sliderInput(
                  inputId = "year4",
                  label = "Années",
                  min = 1945, 
                  max = 2010,
                  step = 5, 
                  sep = "",
                  value = 2010,
                  animate = TRUE),
                hr(),
                helpText("Choisissez une année."),
              
                ),
              )
            )
    )
  )

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Application SIAD"), 
  sidebar,
  body
)



server <- function(input, output) {

  

  output$graph0 <- renderPlot({  
  
ggplot(data = religion_1945_2010 %>% 
                filter(religion %in% c("chrstgen","islmgen","nonrelig","hindgen","budgen","syncgen","shntgen")) %>% 
                group_by(year,religion) %>% summarize(total = sum(value)),
              mapping = aes(x = year,
                            y = total,
                            group = religion,
                            color = religion)) +
    geom_line() + 
    labs(x = "Année",
         y = "Population (en Milliards)",
         color = "Religion") +
    scale_x_continuous(breaks = seq(from = 2010, to = 1945, by = -10)) +
    scale_y_continuous(breaks = seq(from = 0, to = 2500000000, by = 500000000), 
                       labels = c("0","0.5 Mrd", "1 Mrd", " 1.5 Mrd", "2 Mrds", " 2.5 Mrds")) + 
    scale_color_discrete(limits = c("chrstgen","islmgen","nonrelig","hindgen","budgen","syncgen"), 
                         labels = c("Christianisme","Islam","Athéisme","Hindouisme","Boudhisme","Syncrétisme"))
  
  })  
  
  output$donnees <- renderTable({
    religion_1945_2010 %>%
      filter(religion %in% c("chrstgen","islmgen","nonrelig","hindgen","budgen","syncgen","shntgen", "judgen"), 
             year == input$year,
             state %in% input$state) %>% 
      select(-c(year, continent, pop)) %>%
      mutate(religion = recode(religion, 
                               chrstgen = "Christianisme",
                               islmgen = "Islam",
                               nonrelig = "Athéisme",
                               hindgen = "Hindouisme",
                               budgen = "Boudhisme",
                               syncgen = "Syncrétisme",
                               shntgen = "Shintoisme",
                               judgen = "Judaisme"),
             value = as.integer(value)) %>% 
      rename(Pays = state, Religion = religion, "Nombre de croyants" = value) %>%
      distinct()
  })
  
  output$graph1 <- renderPlot({
    ggplot(data = religion_1945_2010 %>% select(-state) %>%
                   filter(religion %in% c("chrstgen","islmgen","nonrelig","hindgen","budgen","syncgen","shntgen")) %>% 
                   group_by(year, religion) %>% summarize(total = sum(value)),
                 mapping = aes(x = year,
                               y = total, group = religion)) +
      geom_line(color = "gray70") +
      geom_line(data = religion_1945_2010 %>% select(-state) %>%
                  mutate(religion = recode(religion, 
                                           chrstgen = "Christianisme",
                                           islmgen = "Islam",
                                           nonrelig = "Athéisme",
                                           hindgen = "Hindouisme",
                                           budgen = "Boudhisme",
                                           syncgen = "Syncrétisme",
                                           shntgen = "Shintoisme",
                                           judgen = "Judaisme")) %>%
                  filter(religion == input$religion1) %>% 
                  group_by(year, religion) %>% summarize(total = sum(value)),
                mapping = aes(x = year,
                              y = total,
                              group = religion,
                              color = religion)) +
      guides(color = FALSE) +
      labs(x = "Année",
           y = "Population (en Milliards)") +
      scale_x_continuous(breaks = seq(from = 2010, to = 1945, by = -20)) +
      scale_y_continuous(breaks = seq(from = 0, to = 2500000000, by = 500000000), 
                         labels = c("0","0.5 Mrd", "1 Mrd", " 1.5 Mrd", "2 Mrds", " 2.5 Mrds"))
    
    
    
  })
  
  output$graph2 <- renderPlot({
    input$generate
    ggplot(data = religion_1945_2010 %>% 
             distinct() %>%
             filter(religion %in% c("chrstgen","islmgen","nonrelig","hindgen","budgen","syncgen") & year == isolate(input$year2)) %>%
             group_by(continent, religion) %>% 
             summarise(total = sum(pop),
                       relig = sum(value),
                       pourcentage = relig/total),
           mapping = aes(x = religion, y = pourcentage, fill = religion)) +
      geom_col() +
      labs(x = "",
           y = "",
           fill = "Religion") +
      facet_wrap(~ continent, ncol = 3) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_discrete(limits = c("chrstgen","islmgen","nonrelig","hindgen","budgen","syncgen")) + 
      scale_fill_brewer(palette = "Spectral", 
                        limits = c("chrstgen","islmgen","nonrelig","hindgen","budgen","syncgen"), 
                        labels = c("Christianisme","Islam","Athéisme","Hindouisme","Boudhisme","Syncrétisme")) + 
      theme(legend.position = "bottom",
            axis.text.x = element_blank(),
            axis.ticks = element_blank())

    
  }) 

  output$graph3 <- renderPlot({ 
      
ggplot(data = religion_1945_2010 %>% distinct() %>% filter(religion == "chrstgen") %>%
         group_by(year) %>% summarise(total = sum(pop[continent == "Europe"]),
                                      total_fr = pop[state == "France"],
                                      chretien = sum(value[continent == "Europe"]),
                                      chretien_fr = value[state == "France"],
                                      prct_chretien = chretien/total,
                                      prct_chretien_fr = chretien_fr/total_fr,
                                      ecart = prct_chretien_fr - prct_chretien,
                                      signe = if_else(ecart >= 0, "+", "-")),
               mapping = aes(x = year,
                             y = ecart, fill = signe)) +
    geom_col() +
    geom_hline(yintercept = 0) +
    labs(x = "Année",
         y = "") +
    scale_x_continuous(breaks = seq(from = 2010, to = 1945, by = -10)) +
    guides(fill = FALSE) + 
    scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.05),labels = scales::percent_format(accuracy = 1)) + 
    geom_text(mapping = aes(label = round(ecart*100,1)),
              vjust = 1.1,
              color = "black",
              alpha = 2) 

  }) 
  
  output$graph4 <- renderLeaflet({
    
    data4 <- religion_1945_2010 %>% 
      distinct() %>%
      mutate(religion = recode(religion, 
                               chrstgen = "Christianisme",
                               islmgen = "Islam",
                               nonrelig = "Athéisme",
                               hindgen = "Hindouisme",
                               budgen = "Boudhisme",
                               syncgen = "Syncrétisme",
                               shntgen = "Shintoisme",
                               judgen = "Judaisme")) %>%
      filter(religion == input$religion4 & year == input$year4) %>%
      group_by(state) %>% 
      summarise(nombre = value) %>% 
      rename(name = state)
      
    data4$name <- as.character(data4$name)
    data4$name[data4$name=="Ivory Coast"] <- "Côte d'Ivoire"
    data4$name[data4$name=="Democratic Republic of the Congo"] <- "Dem. Rep. Congo"
    data4$name[data4$name=="Central African Republic"] <- "Central African Rep."
    data4$name[data4$name=="United States of America"] <- "United States"
    
    data4$name <- as.factor(data4$name)
    
    data4 <- data4 %>% ungroup() 
    
    
    world <- ne_countries(returnclass = "sf")
    
    world4 <- world %>%
      left_join(data4, by = "name")
    
    pal <- colorNumeric("plasma", domain = NULL)
    
    vals <- st_drop_geometry(world4["nombre"])
    vals <- vals[,"nombre"]
    
    leaflet(data = world4) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(nombre),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        smoothFactor = 0.1,
        label = ~paste(name, "-", vals, "croyants")) %>%
      addLegend(pal = pal, position = "bottomright",
                values = vals,
                title = "Nombre de croyants")

  
  })   
  
  
}

# Run the application
shinyApp(ui = ui, server = server)