# ============================================================================
# APPLICATION SHINY - Traitement JSON COBRA
# ============================================================================
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(dplyr)
# Source des fonctions
source("fonctions_cobra.R")

# ============================================================================
# UI (Interface Utilisateur)
# ============================================================================

ui <- dashboardPage(

  # ── En-tête ───────────────────────────────────────────────────────
  dashboardHeader(title = "Traitement de JSON"),
  
  # ── Menu latéral ──────────────────────────────────────────────────
  dashboardSidebar(
    sidebarMenu(
      menuItem("📤 Upload & Traitement", tabName = "upload", icon = icon("upload")),
      menuItem("📊 Résultats", tabName = "resultats", icon = icon("table")),
      menuItem("📥 Téléchargement", tabName = "download", icon = icon("download")),
      menuItem("🗺️ Carte Objects", tabName = "map", icon = icon("map")),
      menuItem("ℹ️ Info", tabName = "aide", icon = icon("question-circle"))
      
    )
  ),
  
  # ── Corps principal ───────────────────────────────────────────────
  dashboardBody(
    tabItems(
      
      # ═══ TAB 1: UPLOAD ═══════════════════════════════════════════
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "📁 Sélection des fichiers JSON",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fileInput(
              "files",
              "Choisir un ou plusieurs fichiers JSON",
              multiple = TRUE,
              accept = c(".json")
            ),
            
            hr(),
            
            actionButton(
              "process",
              "🚀 Traiter les fichiers",
              class = "btn-success btn-lg",
              width = "100%"
            ),
            
            br(), br(),
            
            verbatimTextOutput("status")
          )
        ),
        
        fluidRow(
          valueBoxOutput("nb_fichiers"),
          valueBoxOutput("nb_lignes_pilote"),
          valueBoxOutput("nb_lignes_objects")
        )
      ),
      
      # ═══ TAB 2: RÉSULTATS ════════════════════════════════════════
      tabItem(
        tabName = "resultats",
        
        fluidRow(
          box(
            title = "🗂️ Sélectionner une table",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            selectInput(
              "table_select",
              "Table à afficher:",
              choices = c(
                "Pilote" = "pilote",
                "Objects Location" = "objects_location",
                "IMS Salissures" = "ims_salissures",
                "Corbeilles" = "corbeilles"
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Données",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("table_output")
          )
        )
      ),
      
      # ═══ TAB 3: TÉLÉCHARGEMENT ═══════════════════════════════════
      tabItem(
        tabName = "download",
        
        fluidRow(
          box(
            title = "💾 Télécharger les fichiers CSV",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            p("Téléchargez les fichiers CSV générés :"),
            
            br(),
            
            downloadButton("download_pilote", "📄 Télécharger Pilote", class = "btn-primary"),
            br(), br(),
            downloadButton("download_objects", "📄 Télécharger Objects Location", class = "btn-primary"),
            br(), br(),
            downloadButton("download_ims", "📄 Télécharger IMS Salissures", class = "btn-primary"),
            br(), br(),
            downloadButton("download_corbeilles", "📄 Télécharger Corbeilles", class = "btn-primary"),
            br(), br(),
            hr(),
            downloadButton("download_all", "📦 Télécharger tout (ZIP)", class = "btn-success btn-lg")
          )
        )
      ),
# ═══ TAB 4: CARTE ═════════════════════════════════════
tabItem(
  tabName = "map",
  
  fluidRow(
    box(
      width = 4,
      title = "Catégorisation",
      status = "info",
      solidHeader = TRUE,
      
      selectInput(
        "map_cat",
        "Colorer selon :",
        choices = NULL
      )
    )
  ),
  
  fluidRow(
    box(
      width = 12,
      leafletOutput("map_objects", height = 600)
    )
  )
),

      # ═══ TAB 5: AIDE ═════════════════════════════════════════════
      tabItem(
        tabName = "aide",
        
        fluidRow(
          box(
            title = "📖 Guide d'utilisation",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            h3("Comment utiliser cette application ?"),
            
            tags$ol(
              tags$li(
                strong("Upload des fichiers:"),
                " Dans l'onglet 'Upload & Traitement', cliquez sur 'Choisir un ou plusieurs fichiers JSON' et sélectionnez vos fichiers."
              ),
              tags$li(
                strong("Traitement:"),
                " Cliquez sur le bouton '🚀 Traiter les fichiers'. Le traitement peut prendre quelques secondes par fichier."
              ),
              tags$li(
                strong("Visualisation:"),
                " Allez dans l'onglet 'Résultats' pour voir les données traitées sous forme de tableaux."
              ),
              tags$li(
                strong("Téléchargement:"),
                " Dans l'onglet 'Téléchargement', vous pouvez télécharger les fichiers CSV individuellement ou tous ensemble en ZIP."
              )
            ),
            
            hr(),
            
            h3("📊 Tables générées"),
            
            tags$ul(
              tags$li(strong("Pilote:"), " Résumé de chaque campagne (distance, IMS, durée, etc.)"),
              tags$li(strong("Objects Location:"), " Objets détectés avec leurs coordonnées GPS"),
              tags$li(strong("IMS Salissures:"), " Comptages de salissures avec IMS par type"),
              tags$li(strong("Corbeilles:"), " Données sur les corbeilles détectées")
            ),
            
            hr(),
            
            h3("⚠️ Notes importantes"),
            
            tags$ul(
              tags$li("Les champs", strong("ville"), "et", strong("rue"), "ne sont pas calculés (NA)"),
              tags$li("Le calcul de distance utilise l'ellipsoïde WGS84 (précision réelle)"),
              tags$li("L'IMS est calculé automatiquement (objets/mètre)"),
              tags$li("Les fichiers JSON doivent être au format COBRA standard")
            )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER (Logique de l'application)
# ============================================================================

server <- function(input, output, session) {
  
  # ── Variables réactives ──────────────────────────────────────────
  resultats <- reactiveVal(NULL)
  
  # ── Traitement des fichiers ─────────────────────────────────────
  observeEvent(input$process, {
    
    req(input$files)
    
    # Créer un dossier temporaire
    temp_dir <- tempdir()
    
    # Liste pour stocker tous les résultats
    tous_pilote <- list()
    tous_objects <- list()
    tous_ims <- list()
    tous_corbeilles <- list()
    
    # Barre de progression
    withProgress(message = 'Traitement en cours...', value = 0, {
      
      for (i in 1:nrow(input$files)) {
        
        incProgress(1 / nrow(input$files), detail = paste("Fichier", i, "/", nrow(input$files)))
        
        fichier <- input$files$datapath[i]
        
        # Traiter le fichier
        res <- tryCatch({
          traiter_json_cobra(fichier, exporter_csv = FALSE, verbose = FALSE)
        }, error = function(e) {
          NULL
        })
        
        if (!is.null(res)) {
          tous_pilote[[i]] <- res$pilote
          tous_objects[[i]] <- res$objects_location
          tous_ims[[i]] <- res$ims_salissures
          tous_corbeilles[[i]] <- res$corbeilles
        }
      }
    })
    
    # Combiner tous les résultats
    resultats(list(
      pilote = bind_rows(tous_pilote),
      objects_location = bind_rows(tous_objects),
      ims_salissures = bind_rows(tous_ims),
      corbeilles = bind_rows(tous_corbeilles)
    ))
    
    output$status <- renderText({
      paste(
        "✅ Traitement terminé !\n",
        "Nombre de fichiers traités:", nrow(input$files), "\n",
        "Lignes Pilote:", nrow(resultats()$pilote), "\n",
        "Lignes Objects Location:", nrow(resultats()$objects_location), "\n",
        "Lignes IMS Salissures:", nrow(resultats()$ims_salissures), "\n",
        "Lignes Corbeilles:", nrow(resultats()$corbeilles)
      )
    })
  })
  
  # ── Value boxes ─────────────────────────────────────────────────
  output$nb_fichiers <- renderValueBox({
    req(input$files)
    valueBox(
      nrow(input$files),
      "Fichiers sélectionnés",
      icon = icon("file"),
      color = "blue"
    )
  })
  
  output$nb_lignes_pilote <- renderValueBox({
    req(resultats())
    valueBox(
      nrow(resultats()$pilote),
      "Lignes Pilote",
      icon = icon("table"),
      color = "green"
    )
  })
  
  output$nb_lignes_objects <- renderValueBox({
    req(resultats())
    valueBox(
      nrow(resultats()$objects_location),
      "Lignes Objects",
      icon = icon("map-marker"),
      color = "yellow"
    )
  })
  
  # ── Affichage des tables ─────────────────────────────────────────
  output$table_output <- renderDT({
    req(resultats())
    
    table_data <- switch(
      input$table_select,
      "pilote" = resultats()$pilote,
      "objects_location" = resultats()$objects_location,
      "ims_salissures" = resultats()$ims_salissures,
      "corbeilles" = resultats()$corbeilles
    )
    
    datatable(
      table_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      filter = "top"
    )
  })
  
  # ── Téléchargement ──────────────────────────────────────────────
  output$download_pilote <- downloadHandler(
    filename = function() {
      paste0("pilote_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(resultats())
      write.csv(resultats()$pilote, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$download_objects <- downloadHandler(
    filename = function() {
      paste0("objects_location_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(resultats())
      write.csv(resultats()$objects_location, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$download_ims <- downloadHandler(
    filename = function() {
      paste0("ims_salissures_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(resultats())
      write.csv(resultats()$ims_salissures, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$download_corbeilles <- downloadHandler(
    filename = function() {
      paste0("corbeilles_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(resultats())
      write.csv(resultats()$corbeilles, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("cobra_export_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(resultats())
      
      temp_dir <- tempdir()
      
      # Créer les 4 fichiers CSV
      write.csv(resultats()$pilote, 
                file.path(temp_dir, "pilote.csv"), 
                row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(resultats()$objects_location, 
                file.path(temp_dir, "objects_location.csv"), 
                row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(resultats()$ims_salissures, 
                file.path(temp_dir, "ims_salissures.csv"), 
                row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(resultats()$corbeilles, 
                file.path(temp_dir, "corbeilles.csv"), 
                row.names = FALSE, fileEncoding = "UTF-8")
      
      # Crée le ZIP
      zip(file, files = c(
        file.path(temp_dir, "pilote.csv"),
        file.path(temp_dir, "objects_location.csv"),
        file.path(temp_dir, "ims_salissures.csv"),
        file.path(temp_dir, "corbeilles.csv")
      ), flags = "-j")
    }
  )

# ── création de la crate ──────────────────────────────────────────
 

 output$map_objects <- renderLeaflet({

  leaflet() %>%
    addProviderTiles("CartoDB.Positron")

})
observe({

  req(resultats())

  df <- resultats()$objects_location

  # on enlève latitude / longitude de la sélection
  colonnes_cat <- setdiff(
    names(df),
    c("latitude", "longitude")
  )

  updateSelectInput(
    session,
    "map_cat",
    choices = colonnes_cat,
    selected = "object_type"
  )

})
observe({

  req(resultats(), input$map_cat)

  df <- resultats()$objects_location %>%
    filter(!is.na(latitude), !is.na(longitude))

  var <- df[[input$map_cat]]

  # Palette selon type de variable
  if (is.numeric(var)) {

    pal <- colorNumeric("viridis", domain = var)

  } else {

    var <- as.factor(var)
    pal <- colorFactor("Set1", domain = var)

  }

  leafletProxy("map_objects", data = df) %>%
    clearMarkers() %>%
    clearControls() %>%

    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 5,
      color = ~pal(var),
      stroke = FALSE,
      fillOpacity = 0.8,
      popup = ~paste0(
        "<b>", input$map_cat, " :</b> ", var
      )
    ) %>%

    addLegend(
      "bottomright",
      pal = pal,
      values = var,
      title = input$map_cat
    )

})

}

# ============================================================================
# LANCEMENT DE L'APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)

