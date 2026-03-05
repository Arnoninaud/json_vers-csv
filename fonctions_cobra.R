# ============================================================================
# FONCTIONS COBRA - Traitement JSON
# Fichier de fonctions pour utilisation dans Shiny ou autres scripts
# ============================================================================

# Chargement des bibliothèques nécessaires
suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(sf)
  library(geosphere)
})

# ============================================================================
# FONCTIONS GÉOSPATIALES
# ============================================================================

#' Calcule la distance géodésique, la géométrie LineString et l'IMS
#'
#' @param df_locations DataFrame contenant les colonnes longitude, latitude, timestamp
#' @param nb_final Nombre total d'objets finaux
#' @return Liste contenant distance (en mètres), geom (WKT), et ims
#' @export
compute_distance_linestring <- function(df_locations, nb_final) {
  tryCatch({
    df <- df_locations
    
    # Vérification et tri par timestamp
    if ("timestamp" %in% names(df)) {
      df$timestamp <- as.POSIXct(df$timestamp / 1000, origin = "1970-01-01", tz = "UTC")
      df <- df %>% arrange(timestamp)
    }
    
    # Vérification nombre de points
    if (nrow(df) < 2) {
      warning("Pas assez de points pour créer une LineString.")
      return(list(distance = 0, geom = NA, ims = 0))
    }
    
    # On ignore les 2 premières coordonnées GPS (instables)
    if (nrow(df) > 4) {
      df <- df %>% slice(3:n())
    }
    
    # Création de l'objet sf (LineString)
    coords_matrix <- as.matrix(df[, c("longitude", "latitude")])
    line_raw <- st_linestring(coords_matrix)
    line_raw_sfc <- st_sfc(line_raw, crs = 4326)  # WGS84
    
    # Simplification de la trajectoire (~1.5m)
    line_simplified <- st_simplify(line_raw_sfc, dTolerance = 0.00003, preserveTopology = TRUE)
    
    # Calcul de la distance réelle (géodésique)
    distance <- round(distGeo(coords_matrix), 0)
    
    # Calcul de l'IMS
    ims <- if (distance > 0) round(nb_final / distance, 2) else 0
    
    # Conversion en WKT
    geom <- st_as_text(line_simplified[[1]])
    
    return(list(distance = distance, geom = geom, ims = ims))
    
  }, error = function(e) {
    message("Erreur dans compute_distance_linestring: ", e$message)
    return(list(distance = 0, geom = NA, ims = 0))
  })
}

# ============================================================================
# FONCTIONS DE TRAITEMENT DES DONNÉES
# ============================================================================

#' Attribue les coordonnées GPS aux objets via nearest join
#'
#' @param objects_df DataFrame des objets avec timestamp
#' @param locations_df DataFrame des locations GPS avec timestamp, latitude, longitude
#' @return DataFrame des objets avec coordonnées GPS ajoutées
#' @export
attribute_GPS_to_objects <- function(objects_df, locations_df) {
  if (nrow(objects_df) == 0) {
    return(data.frame())
  }
  
  objects_df <- objects_df %>% arrange(timestamp)
  locations_df <- locations_df %>% arrange(timestamp)
  
  # Conversion en numérique
  objects_df$timestamp <- as.numeric(objects_df$timestamp)
  locations_df$timestamp <- as.numeric(locations_df$timestamp)
  
  # Merge asof (nearest join)
  matched_df <- objects_df %>%
    rowwise() %>%
    mutate(
      idx = which.min(abs(locations_df$timestamp - timestamp)),
      latitude = locations_df$latitude[idx],
      longitude = locations_df$longitude[idx]
    ) %>%
    select(-idx) %>%
    ungroup()
  
  # Renommer classId en salissures si présent
  if ("classId" %in% names(matched_df)) {
    matched_df <- matched_df %>% rename(salissures = classId)
  }
  
  return(matched_df)
}

#' Calcule la différence entre objects_count et finalCounts_count
#'
#' @param objects_count Nombre d'objets détectés
#' @param finalCounts_count Nombre d'objets validés
#' @return Différence (positif = ajout, négatif = suppression)
#' @export
calculate_difference <- function(objects_count, finalCounts_count) {
  if (is.na(objects_count)) {
    return(as.integer(finalCounts_count))
  } else if (is.na(finalCounts_count)) {
    return(as.integer(objects_count))
  } else {
    return(as.integer(finalCounts_count - objects_count))
  }
}

#' Applique les différences de comptage aux objets
#'
#' @param df_initial DataFrame initial des objets
#' @param df_diff DataFrame contenant les différences par classId
#' @return DataFrame corrigé
#' @export
apply_diff <- function(df_initial, df_diff) {
  df_result <- df_initial
  
  for (i in 1:nrow(df_diff)) {
    cat_class <- df_diff$classId[i]
    difference <- df_diff$difference[i]
    
    # SUPPRESSION
    if (difference < 0) {
      n_remove <- abs(difference)
      idx_to_remove <- which(df_result$classId == cat_class)[1:min(n_remove, sum(df_result$classId == cat_class))]
      df_result <- df_result[-idx_to_remove, ]
    }
    
    # AJOUT
    else if (difference > 0) {
      n_add <- difference
      
      timestamps <- df_result$timestamp[df_result$classId == cat_class]
      
      if (length(timestamps) > 0) {
        ts_value <- max(timestamps)
      } else {
        ts_value <- ifelse(nrow(df_result) > 0, max(df_result$timestamp), NA)
      }
      
      new_rows <- data.frame(
        id = NA,
        classId = cat_class,
        timestamp = ts_value
      )
      new_rows <- new_rows[rep(1, n_add), ]
      
      df_result <- bind_rows(df_result, new_rows)
    }
  }
  
  return(df_result)
}

# ============================================================================
# FONCTION PRINCIPALE DE TRAITEMENT
# ============================================================================

#' Traite un fichier JSON COBRA et génère les 4 DataFrames
#'
#' @param chemin_json Chemin vers le fichier JSON
#' @param exporter_csv Booléen pour exporter en CSV
#' @param dossier_sortie Dossier de destination des CSV
#' @param verbose Afficher les messages de progression
#' @return Liste contenant les 4 DataFrames (pilote, objects_location, ims_salissures, corbeilles)
#' @export
traiter_json_cobra <- function(chemin_json, exporter_csv = TRUE, dossier_sortie = ".", verbose = TRUE) {
  
  if (verbose) {
    cat("\n========================================\n")
    cat("Traitement du fichier:", chemin_json, "\n")
    cat("========================================\n\n")
  }
  
  # ── 1. LECTURE DU JSON ──────────────────────────────────────────────
  json_data <- fromJSON(chemin_json)
  
  id_campaign <- json_data$id
  locations <- json_data$locations
  objects <- json_data$objects
  finalCounts <- json_data$finalCounts
  createdAt <- json_data$createdAt
  info <- json_data$info
  userId <- json_data$userId
  caption <- ifelse(is.null(json_data$caption), "", json_data$caption)
  version_name <- ifelse(is.null(info$versionName), NA, info$versionName)
  is_test <- ifelse(is.null(json_data$isTest), NA, json_data$isTest)
  rating <- ifelse(is.null(json_data$rating), NA, json_data$rating)
  commentaire_val <- ifelse(is.null(json_data$comment), NA, json_data$comment)
  delegate <- ifelse(is.null(info$delegateInfo), NA, info$delegateInfo)
  caption <- ifelse(is.null(json_data$caption), NA, json_data$caption)
  if (verbose) {
    cat("ID Campagne:", id_campaign, "\n")
    cat("Nombre de locations:", nrow(locations), "\n")
    cat("Nombre d'objets:", nrow(objects), "\n")
  }
  
  # ── 2. GESTION DE createdAt ─────────────────────────────────────────
  if (is.null(createdAt)) {
    if (!is.null(objects) && nrow(objects) > 0) {
      createdAt <- objects$timestamp[1]
    }
  }
  
  # ── 3. GESTION DE finalCounts ───────────────────────────────────────
  if (is.null(finalCounts) || length(finalCounts) == 0) {
    if (!is.null(objects) && nrow(objects) > 0) {
      finalCounts_list <- table(objects$classId)
      finalCounts <- as.list(finalCounts_list)
    } else {
      finalCounts <- list()
    }
  }
  
  # ── 4. CALCUL DE LA DURÉE ───────────────────────────────────────────
  if (nrow(locations) >= 2) {
    first_ts <- as.POSIXct(locations$timestamp[1] / 1000, origin = "1970-01-01", tz = "UTC")
    last_ts <- as.POSIXct(locations$timestamp[nrow(locations)] / 1000, origin = "1970-01-01", tz = "UTC")
    duree <- as.integer(difftime(last_ts, first_ts, units = "secs"))
  } else {
    if (verbose) cat("❌ Pas assez de points GPS\n")
    return(NULL)
  }
  
  # ── 5. SÉPARATION CORBEILLES / SALISSURES ──────────────────────────
  df_locations <- as.data.frame(locations)
  
  if (length(finalCounts) > 0) {
    df_finalCounts <- data.frame(
      classId = names(finalCounts),
      finalCounts_count = as.integer(unlist(finalCounts)),
      stringsAsFactors = FALSE
    )
  } else {
    df_finalCounts <- data.frame(classId = character(), finalCounts_count = integer())
  }
  
  # Liste des classes poubelles
  poubelles <- c(
    "corbeille_vide",
    "corbeille_moyenne_remplie",
    "corbeille_pleine",
    "corbeille_debordante"
  )
  
  df_finalCounts <- df_finalCounts %>% filter(!classId %in% poubelles)
  
  if (!is.null(objects) && nrow(objects) > 0) {
    df_objects <- as.data.frame(objects)
    df_poubelles <- df_objects %>% filter(classId %in% poubelles)
    df_salissures <- df_objects %>% filter(!classId %in% poubelles)
  } else {
    df_objects <- data.frame(id = character(), classId = character(), timestamp = numeric())
    df_poubelles <- data.frame(id = character(), classId = character(), timestamp = numeric())
    df_salissures <- data.frame(id = character(), classId = character(), timestamp = numeric())
  }
  
  if (verbose) {
    cat("Nombre de salissures:", nrow(df_salissures), "\n")
    cat("Nombre de poubelles:", nrow(df_poubelles), "\n")
  }
  
  # ── 6. PRÉPARATION DES DONNÉES PILOTE ───────────────────────────────
  nb_GPS <- nrow(locations)
  nb_objects <- nrow(df_salissures)
  nb_final <- sum(df_finalCounts$finalCounts_count)
  
  if (!is.null(createdAt)) {
    timestamp_dt <- as.POSIXct(createdAt / 1000, origin = "1970-01-01", tz = "UTC")
    annee <- year(timestamp_dt)
    mois <- month(timestamp_dt)
    jour <- day(timestamp_dt)
    heure <- format(timestamp_dt, "%H:%M:%S")
  } else {
    annee <- mois <- jour <- NA
    heure <- NA
    timestamp_dt <- NA
  }
  
  # ── CALCUL DISTANCE, GEOM, IMS ──────────────────────────────────────
  if (verbose) cat("\n🔄 Calcul de la distance géodésique et de la géométrie...\n")
  result_geom <- compute_distance_linestring(df_locations, nb_final)
  distance <- result_geom$distance
  geom <- result_geom$geom
  ims <- result_geom$ims
  
  if (verbose) {
    cat("✅ Distance calculée:", distance, "mètres\n")
    cat("✅ IMS calculé:", ims, "\n")
  }
  
  ville <- NA
  rue <- NA
  
  # ── 7. TABLE PILOTE ─────────────────────────────────────────────────
  df_pilote <- data.frame(
    id_campaign = id_campaign,
    userId = userId,
    commentaire = commentaire_val,
    caption = caption,
    ville = ville,
    rue = rue,
    jour = jour,
    mois = mois,
    annee = annee,
    heure = heure,
    method = delegate,
    nb_GPS = nb_GPS,
    nb_objects = nb_objects,
    nb_final = nb_final,
    distance = distance,
    geom = geom,
    ims = ims,
    timestamp = timestamp_dt,
    duree = duree,
    version = version_name,
    nom = caption,
    test = is_test,
    note = rating,
    stringsAsFactors = FALSE
  )
  
  if (verbose) cat("\n✅ Table PILOTE créée (", nrow(df_pilote), "ligne)\n")
  
  # ── 8. TABLE OBJECTS_LOCATION ───────────────────────────────────────
  if (nrow(df_salissures) > 0) {
    pivot_df_salissures <- df_salissures %>%
      group_by(classId) %>%
      summarise(objects_count = n(), .groups = "drop")
  } else {
    pivot_df_salissures <- data.frame(classId = character(), objects_count = integer())
  }
  
  df_merged <- full_join(df_finalCounts, pivot_df_salissures, by = "classId")
  
  df_merged <- df_merged %>%
    rowwise() %>%
    mutate(difference = calculate_difference(objects_count, finalCounts_count)) %>%
    ungroup()
  
  df_corrected <- apply_diff(df_salissures, df_merged)
  
  if (nrow(df_corrected) > 0) {
    df_matched <- attribute_GPS_to_objects(df_corrected, df_locations)
    
    df_matched <- df_matched %>%
      mutate(
        id_campaign = id_campaign,
        userId = userId,
        commentaire = commentaire_val,
        caption = caption,
        ville = ville,
        rue = rue,
        jour = jour,
        mois = mois,
        annee = annee,
        heure = heure,
        test = is_test,
        method = delegate,
        version = version_name,
        timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")
      ) %>%
      select(-id)
    
    if (verbose) cat("✅ Table OBJECTS_LOCATION créée (", nrow(df_matched), "lignes)\n")
  } else {
    df_matched <- data.frame()
    if (verbose) cat("⚠️  Aucune donnée pour OBJECTS_LOCATION\n")
  }
  
  # ── 9. TABLE IMS_SALISSURES ─────────────────────────────────────────
  if (nrow(df_finalCounts) > 0) {
    df_ims_salissures <- df_finalCounts %>%
      mutate(
        id_campaign = id_campaign,
        userId = userId,
        ville = ville,
        rue = rue,
        jour = jour,
        mois = mois,
        annee = annee,
        heure = heure,
        timestamp = timestamp_dt,
        ims = ifelse(!is.na(distance) & distance > 0, 
                     finalCounts_count / distance, 
                     NA),
        test = is_test
      ) %>%
      filter(finalCounts_count != 0) %>%
      rename(count = finalCounts_count, salissures = classId)
    
    if (verbose) cat("✅ Table IMS_SALISSURES créée (", nrow(df_ims_salissures), "lignes)\n")
  } else {
    df_ims_salissures <- data.frame()
    if (verbose) cat("⚠️  Aucune donnée pour IMS_SALISSURES\n")
  }
  
  # ── 10. TABLE CORBEILLES ────────────────────────────────────────────
  if (nrow(df_poubelles) > 0) {
    df_poubelles_with_gps <- attribute_GPS_to_objects(df_poubelles, df_locations)
    
    df_poubelles_with_gps <- df_poubelles_with_gps %>%
      mutate(
        id_campaign = id_campaign,
        userId = userId,
        ville = ville,
        rue = rue,
        jour = jour,
        mois = mois,
        annee = annee,
        heure = heure,
        test = is_test,
        timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")
      ) %>%
      select(-id) %>%
      rename(remplissage = salissures)
    
    if (verbose) cat("✅ Table CORBEILLES créée (", nrow(df_poubelles_with_gps), "lignes)\n")
  } else {
    df_poubelles_with_gps <- data.frame()
    if (verbose) cat("⚠️  Aucune donnée pour CORBEILLES\n")
  }
  
  # ── 11. EXPORT CSV ──────────────────────────────────────────────────
  if (exporter_csv) {
    base_name <- tools::file_path_sans_ext(basename(chemin_json))
    
    if (nrow(df_pilote) > 0) {
      fichier <- file.path(dossier_sortie, paste0(base_name, "_pilote.csv"))
      write.csv(df_pilote, fichier, row.names = FALSE, fileEncoding = "UTF-8")
      if (verbose) cat("\n📁 Exporté:", fichier, "\n")
    }
    
    if (nrow(df_matched) > 0) {
      fichier <- file.path(dossier_sortie, paste0(base_name, "_objects_location.csv"))
      write.csv(df_matched, fichier, row.names = FALSE, fileEncoding = "UTF-8")
      if (verbose) cat("📁 Exporté:", fichier, "\n")
    }
    
    if (nrow(df_ims_salissures) > 0) {
      fichier <- file.path(dossier_sortie, paste0(base_name, "_ims_salissures.csv"))
      write.csv(df_ims_salissures, fichier, row.names = FALSE, fileEncoding = "UTF-8")
      if (verbose) cat("📁 Exporté:", fichier, "\n")
    }
    
    if (nrow(df_poubelles_with_gps) > 0) {
      fichier <- file.path(dossier_sortie, paste0(base_name, "_corbeilles.csv"))
      write.csv(df_poubelles_with_gps, fichier, row.names = FALSE, fileEncoding = "UTF-8")
      if (verbose) cat("📁 Exporté:", fichier, "\n")
    }
  }
  
  if (verbose) cat("\n✅ Traitement terminé avec succès !\n\n")
  
  # ── 12. RETOUR DES RÉSULTATS ────────────────────────────────────────
  return(list(
    pilote = df_pilote,
    objects_location = df_matched,
    ims_salissures = df_ims_salissures,
    corbeilles = df_poubelles_with_gps
  ))
}

#' Traite tous les fichiers JSON d'un dossier
#'
#' @param dossier_entree Dossier contenant les fichiers JSON
#' @param dossier_sortie Dossier de destination des CSV
#' @param verbose Afficher les messages de progression
#' @return Liste contenant les 4 DataFrames combinés
#' @export
traiter_dossier_json <- function(dossier_entree, dossier_sortie = "output", verbose = TRUE) {
  
  if (!dir.exists(dossier_sortie)) {
    dir.create(dossier_sortie, recursive = TRUE)
  }
  
  fichiers_json <- list.files(dossier_entree, pattern = "\\.json$", full.names = TRUE)
  
  if (length(fichiers_json) == 0) {
    if (verbose) cat("❌ Aucun fichier JSON trouvé dans:", dossier_entree, "\n")
    return(NULL)
  }
  
  if (verbose) {
    cat("\n🔍 Trouvé", length(fichiers_json), "fichier(s) JSON\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  
  tous_pilote <- list()
  tous_objects <- list()
  tous_ims <- list()
  tous_corbeilles <- list()
  
  for (fichier in fichiers_json) {
    resultats <- tryCatch({
      traiter_json_cobra(fichier, exporter_csv = TRUE, dossier_sortie = dossier_sortie, verbose = verbose)
    }, error = function(e) {
      if (verbose) cat("\n❌ Erreur avec", basename(fichier), ":", e$message, "\n\n")
      return(NULL)
    })
    
    if (!is.null(resultats)) {
      tous_pilote[[length(tous_pilote) + 1]] <- resultats$pilote
      if (nrow(resultats$objects_location) > 0) {
        tous_objects[[length(tous_objects) + 1]] <- resultats$objects_location
      }
      if (nrow(resultats$ims_salissures) > 0) {
        tous_ims[[length(tous_ims) + 1]] <- resultats$ims_salissures
      }
      if (nrow(resultats$corbeilles) > 0) {
        tous_corbeilles[[length(tous_corbeilles) + 1]] <- resultats$corbeilles
      }
    }
  }
  
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("📊 CRÉATION DES FICHIERS COMBINÉS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  
  if (length(tous_pilote) > 0) {
    df_pilote_combine <- bind_rows(tous_pilote)
    fichier_combine <- file.path(dossier_sortie, "COMBINE_pilote.csv")
    write.csv(df_pilote_combine, fichier_combine, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) cat("📦 Fichier combiné PILOTE créé:", nrow(df_pilote_combine), "lignes\n")
  }
  
  if (length(tous_objects) > 0) {
    df_objects_combine <- bind_rows(tous_objects)
    fichier_combine <- file.path(dossier_sortie, "COMBINE_objects_location.csv")
    write.csv(df_objects_combine, fichier_combine, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) cat("📦 Fichier combiné OBJECTS_LOCATION créé:", nrow(df_objects_combine), "lignes\n")
  }
  
  if (length(tous_ims) > 0) {
    df_ims_combine <- bind_rows(tous_ims)
    fichier_combine <- file.path(dossier_sortie, "COMBINE_ims_salissures.csv")
    write.csv(df_ims_combine, fichier_combine, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) cat("📦 Fichier combiné IMS_SALISSURES créé:", nrow(df_ims_combine), "lignes\n")
  }
  
  if (length(tous_corbeilles) > 0) {
    df_corbeilles_combine <- bind_rows(tous_corbeilles)
    fichier_combine <- file.path(dossier_sortie, "COMBINE_corbeilles.csv")
    write.csv(df_corbeilles_combine, fichier_combine, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) cat("📦 Fichier combiné CORBEILLES créé:", nrow(df_corbeilles_combine), "lignes\n")
  }
  
  if (verbose) {
    cat("\n✅ Traitement terminé !\n")
    cat("📁 Tous les fichiers sont dans:", dossier_sortie, "\n\n")
  }
  
  return(list(
    pilote = if(length(tous_pilote) > 0) bind_rows(tous_pilote) else NULL,
    objects_location = if(length(tous_objects) > 0) bind_rows(tous_objects) else NULL,
    ims_salissures = if(length(tous_ims) > 0) bind_rows(tous_ims) else NULL,
    corbeilles = if(length(tous_corbeilles) > 0) bind_rows(tous_corbeilles) else NULL
  ))
}
