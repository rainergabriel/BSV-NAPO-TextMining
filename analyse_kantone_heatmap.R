##############################################################################
# Heatmap-Analyse: TF-IDF-basierte Inhaltsanalyse kantonaler und kommunaler
# Alterskonzepte
# BSV Mandat G26-02 — Nationale Alterspolitik
#
# Liest suchbegriffe.csv (trilingual, nach Cluster/Thema gegliedert),
# durchsucht corpus_kantone.jsonl und corpus_gemeinden.jsonl,
# berechnet TF-IDF pro Thema:
#   TF  = Summe aller Varianten-Treffer / Wortanzahl des Dokuments
#   IDF = log(N / df)  mit N = Korpusgrösse, df = Anzahl Dokumente mit Treffer
#   TF-IDF = TF × IDF
#
# Klassifizierung in 3 Stufen (datenbasiert):
#   0           = nicht erwähnt
#   >0 bis Q50  = am Rande erwähnt (unter dem Median der Nicht-Null-Werte)
#   >Q50        = thematisiert (über dem Median der Nicht-Null-Werte)
#
# Voraussetzungen:
#   install.packages(c("jsonlite", "ggplot2", "ggtext", "dplyr", "tidyr",
#                       "stringr"))
#
# Aufruf:
#   Rscript analyse_kantone_heatmap.R
##############################################################################

library(jsonlite)
library(ggplot2)
library(ggtext)
library(dplyr)
library(tidyr)
library(stringr)

# --- Pfade ---
script_dir <- if (interactive()) {
  getwd()
} else {
  dirname(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))] |>
    sub("--file=", "", x = _))
}
setwd(script_dir)

data_dir <- file.path(script_dir, "data")
out_dir  <- file.path(script_dir, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Stufen-Labels ---
LABEL_0 <- "nicht erwähnt"
LABEL_1 <- "am Rande erwähnt"
LABEL_2 <- "thematisiert"
STUFEN_LEVELS <- c(LABEL_0, LABEL_1, LABEL_2)
STUFEN_COLORS <- c("#D32F2F", "#FFA726", "#388E3C")
names(STUFEN_COLORS) <- STUFEN_LEVELS

# --- Lesehilfe-Texte ---
wrap_caption <- function(txt, width = 120) {
  paste(strwrap(txt, width = width), collapse = "\n")
}

LESEHILFE_KT <- wrap_caption(paste0(
  "Lesehilfe: Die Zahl in jeder Zelle zeigt den TF-IDF-Wert des Themas im ",
  "kantonalen Dokument (×1'000). ",
  "TF-IDF = (Treffer / Wörter) × log(Anzahl Kantone / Kantone mit Treffer). ",
  "Themen, die in allen Kantonen vorkommen, werden abgewichtet; ",
  "seltener behandelte Themen erhalten höhere Werte. ",
  "Stufen: rot = nicht erwähnt (0), orange = am Rande erwähnt ",
  "(unter Median), grün = thematisiert (über Median). ",
  "Suchvarianten in Klammern unter dem Thema."
))

LESEHILFE_TIER <- wrap_caption(paste0(
  "Lesehilfe: Die Zahl zeigt den Durchschnitt des TF-IDF-Werts (×1'000) ",
  "über alle Gemeinden der jeweiligen Grössenklasse. ",
  "TF-IDF = (Treffer / Wörter) × log(Anzahl Gemeinden / Gemeinden mit Treffer). ",
  "GK 1: >=20'000 Einw. | GK 2: 10'000-20'000 | GK 3: 5'000-10'000 | ",
  "GK 4: 2'000-5'000 | GK 5: <2'000. (GK = Grössenklasse Gemeinde)"
))

LESEHILFE_DETAIL <- wrap_caption(paste0(
  "Lesehilfe: Die Zahl zeigt den TF-IDF-Wert (×1'000) pro Gemeinde. ",
  "TF-IDF = (Treffer / Wörter) × log(Anzahl Gemeinden / Gemeinden mit Treffer). ",
  "Stufen: rot = nicht erwähnt (0), orange = am Rande erwähnt ",
  "(unter Median), grün = thematisiert (über Median)."
))

# --- Suchbegriffe einlesen ---
sb <- read.csv("suchbegriffe.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
cat("Suchbegriffe geladen:", nrow(sb), "Begriffe in",
    length(unique(sb$cluster)), "Clustern,",
    length(unique(sb$topic)), "Themen\n")

sb$all_terms <- paste(sb$term_de, sb$term_fr, sb$term_it, sep = ";")

# --- X-Achsen-Labels bauen: Topic + Suchvarianten in Klammern ---
sb$term_short <- str_replace(sb$term_label, " / .*", "")

build_x_labels <- function(sb_cl, max_per_line = 3) {
  topic_info <- sb_cl |>
    group_by(topic, topic_label) |>
    summarise(
      terms = list(term_short),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      variants_wrapped = {
        t <- terms
        lines <- split(t, ceiling(seq_along(t) / max_per_line))
        paste(sapply(lines, paste, collapse = ", "), collapse = ",<br>")
      },
      x_label = paste0(
        "<b>", topic_label, "</b><br>",
        "<span style='font-size:7pt; color:grey40'>(",
        variants_wrapped, ")</span>"
      )
    ) |>
    ungroup()
  topic_info
}

# --- Korpus einlesen ---
kt_path <- file.path(data_dir, "corpus_kantone.jsonl")
if (!file.exists(kt_path)) stop("corpus_kantone.jsonl nicht gefunden in data/")
kt_lines <- readLines(kt_path, encoding = "UTF-8")
corpus_kt <- bind_rows(lapply(kt_lines, fromJSON))
cat("Kantone geladen:", nrow(corpus_kt), "\n")
corpus_kt$n_words <- str_count(corpus_kt$text, "\\S+")

gm_path <- file.path(data_dir, "corpus_gemeinden.jsonl")
has_gemeinden <- file.exists(gm_path)
if (has_gemeinden) {
  gm_lines <- readLines(gm_path, encoding = "UTF-8")
  corpus_gm <- bind_rows(lapply(gm_lines, fromJSON))
  corpus_gm$n_words <- str_count(corpus_gm$text, "\\S+")
  cat("Gemeinden geladen:", nrow(corpus_gm), "\n")

  tier_path <- file.path(data_dir, "gemeinden_tier.csv")
  if (file.exists(tier_path)) {
    tier_map <- read.csv(tier_path, stringsAsFactors = FALSE, encoding = "UTF-8")
    tier_map$bfs_nr <- as.character(tier_map$bfs_nr)
    corpus_gm$bfs_nr <- as.character(corpus_gm$bfs_nr)
    corpus_gm <- corpus_gm |>
      left_join(tier_map |> select(bfs_nr, einwohner, tier), by = "bfs_nr")
    unmatched <- is.na(corpus_gm$tier)
    if (any(unmatched)) {
      cat("  Warnung:", sum(unmatched), "Gemeinden ohne Grössenklassen-Zuordnung via BFS-Nr.\n")
      for (i in which(unmatched)) {
        gm_name <- tolower(corpus_gm$gemeinde[i])
        gm_kt   <- corpus_gm$kanton[i]
        match_idx <- which(
          tolower(tier_map$gemeinde_name) == gm_name &
          tier_map$kanton == gm_kt
        )
        if (length(match_idx) == 0) {
          match_idx <- which(
            str_detect(tolower(tier_map$gemeinde_name), fixed(gm_name)) &
            tier_map$kanton == gm_kt
          )
        }
        if (length(match_idx) > 0) {
          corpus_gm$tier[i]      <- tier_map$tier[match_idx[1]]
          corpus_gm$einwohner[i] <- tier_map$einwohner[match_idx[1]]
        }
      }
      still_unmatched <- sum(is.na(corpus_gm$tier))
      if (still_unmatched > 0) {
        cat("  Noch unzugeordnet:", still_unmatched, "\n")
        corpus_gm$tier[is.na(corpus_gm$tier)] <- "Unbekannt"
      }
    }
    corpus_gm$tier <- str_replace(corpus_gm$tier, "^Tier ", "GK ")
    cat("  Grössenklassen-Verteilung:\n")
    print(table(corpus_gm$tier))
  }
} else {
  cat("Keine Gemeinde-Daten gefunden, überspringe Gemeinde-Analysen.\n")
}

# ============================================================================
# TF-IDF Suche
# ============================================================================
search_corpus_tfidf <- function(corpus, id_col, sb_subset) {
  topics <- sb_subset |>
    group_by(topic, topic_label, cluster, cluster_label) |>
    summarise(all_terms = paste(all_terms, collapse = ";"), .groups = "drop")

  N <- length(unique(corpus[[id_col]]))

  results <- expand.grid(
    entity = unique(corpus[[id_col]]),
    topic  = topics$topic,
    stringsAsFactors = FALSE
  ) |>
    left_join(topics, by = "topic")

  results$n_words   <- NA_integer_
  results$raw_count <- NA_integer_
  results$tf        <- NA_real_

  for (i in seq_len(nrow(results))) {
    idx <- which(corpus[[id_col]] == results$entity[i])
    text <- paste(corpus$text[idx], collapse = " ")
    text_lower <- str_to_lower(text)
    wc <- sum(corpus$n_words[idx])

    varianten <- unlist(str_split(results$all_terms[i], ";"))
    varianten <- str_trim(varianten)
    varianten <- varianten[varianten != ""]

    total_count <- sum(sapply(varianten, function(v) {
      str_count(text_lower, fixed(str_to_lower(v)))
    }))

    results$n_words[i]   <- wc
    results$raw_count[i] <- total_count
    results$tf[i]        <- if (wc > 0) total_count / wc else 0
  }

  # IDF per topic: log(N / df), df = number of docs with at least 1 hit
  df_per_topic <- results |>
    group_by(topic) |>
    summarise(df = sum(raw_count > 0), .groups = "drop")

  results <- results |>
    left_join(df_per_topic, by = "topic") |>
    mutate(
      idf   = log(N / pmax(df, 1)),
      tfidf = tf * idf
    )

  # Scale TF-IDF ×1000 for readability (analogous to "per 1'000 words")
  results$tfidf_scaled <- results$tfidf * 1000

  # Classify: 3 levels based on median of non-zero values
  nonzero_vals <- results$tfidf_scaled[results$tfidf_scaled > 0]
  cut_median <- if (length(nonzero_vals) > 0) median(nonzero_vals) else 0.5

  results$stufe <- factor(
    case_when(
      results$tfidf_scaled == 0       ~ LABEL_0,
      results$tfidf_scaled <= cut_median ~ LABEL_1,
      TRUE                             ~ LABEL_2
    ),
    levels = STUFEN_LEVELS
  )

  cat("    TF-IDF Schwellenwert (Median nicht-null, ×1'000):", round(cut_median, 2), "\n")
  results
}

# ============================================================================
# Heatmap: 3-Stufen (Kantone / Gemeinden Detail)
# ============================================================================
make_heatmap_3level <- function(data, x_col, y_col, title, subtitle,
                                caption, filename_base,
                                width = 12, height = 10) {
  p <- ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], fill = stufe)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(tfidf_scaled, 1)),
              size = 2.8, color = "white", fontface = "bold") +
    scale_fill_manual(values = STUFEN_COLORS, drop = FALSE, name = "") +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_markdown(angle = 45, hjust = 1, size = 9,
                                      lineheight = 1.2),
      axis.text.y = element_text(face = "bold", size = 9),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40", size = 10),
      plot.caption = element_text(color = "grey50", size = 7.5,
                                   hjust = 0, lineheight = 1.3),
      plot.caption.position = "plot",
      plot.margin = margin(10, 10, 10, 10)
    )

  pdf_path <- file.path(out_dir, paste0(filename_base, ".pdf"))
  png_path <- file.path(out_dir, paste0(filename_base, ".png"))
  ggsave(pdf_path, p, width = width, height = height, device = "pdf",
         limitsize = FALSE)
  ggsave(png_path, p, width = width, height = height, dpi = 300,
         limitsize = FALSE)
  cat("  ->", pdf_path, "\n")
  p
}

# ============================================================================
# Heatmap: Grössenklassen-Aggregation (Durchschnitt)
# ============================================================================
make_heatmap_tier <- function(tier_agg, title, subtitle, caption,
                              filename_base, width = 12, height = 7) {
  p <- ggplot(tier_agg, aes(x = topic_label, y = tier, fill = mean_tfidf)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label), size = 3, color = "white", fontface = "bold") +
    scale_fill_gradientn(
      colours = STUFEN_COLORS,
      values  = scales::rescale(c(0, median(tier_agg$mean_tfidf[tier_agg$mean_tfidf > 0], na.rm = TRUE),
                                  max(tier_agg$mean_tfidf, na.rm = TRUE))),
      limits  = c(0, NA),
      name    = "Ø TF-IDF (×1'000)"
    ) +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_markdown(angle = 45, hjust = 1, size = 9,
                                      lineheight = 1.2),
      axis.text.y = element_text(face = "bold", size = 9),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40", size = 10),
      plot.caption = element_text(color = "grey50", size = 7.5,
                                   hjust = 0, lineheight = 1.3),
      plot.caption.position = "plot",
      plot.margin = margin(10, 10, 10, 10)
    )

  pdf_path <- file.path(out_dir, paste0(filename_base, ".pdf"))
  png_path <- file.path(out_dir, paste0(filename_base, ".png"))
  ggsave(pdf_path, p, width = width, height = height, device = "pdf")
  ggsave(png_path, p, width = width, height = height, dpi = 300)
  cat("  ->", pdf_path, "\n")
  p
}

# ============================================================================
# Hauptschleife
# ============================================================================
clusters <- unique(sb$cluster)

for (cl in clusters) {
  sb_cl <- sb |> filter(cluster == cl)
  cl_label <- sb_cl$cluster_label[1]
  topic_info <- build_x_labels(sb_cl)
  n_topics <- nrow(topic_info)
  cat("\n===", cl, cl_label, "(", n_topics, "Themen ) ===\n")

  # --- Kantone ---
  cat("  Kantone (TF-IDF)...\n")
  res_kt <- search_corpus_tfidf(corpus_kt, "kanton", sb_cl)

  res_kt <- res_kt |>
    left_join(topic_info |> select(topic, x_label), by = "topic")

  kt_order <- sort(unique(res_kt$entity), decreasing = TRUE)
  res_kt$entity <- factor(res_kt$entity, levels = kt_order)
  res_kt$x_label <- factor(res_kt$x_label, levels = topic_info$x_label)

  make_heatmap_3level(
    res_kt, "x_label", "entity",
    title = paste0(cl, " ", cl_label, " — Kantone (TF-IDF)"),
    subtitle = paste0("Korpus: ", nrow(corpus_kt), " Kantone | ",
                      n_topics, " Themen | Metrik: TF-IDF (×1'000)"),
    caption = LESEHILFE_KT,
    filename_base = paste0("heatmap_", cl, "_kantone"),
    width = max(9, n_topics * 2.2),
    height = max(9, length(kt_order) * 0.45 + 1.5)
  )

  # --- Gemeinden ---
  if (has_gemeinden) {
    cat("  Gemeinden (TF-IDF, Grössenklassen)...\n")
    res_gm <- search_corpus_tfidf(corpus_gm, "gemeinde", sb_cl)
    res_gm <- res_gm |>
      left_join(corpus_gm |> select(gemeinde, tier) |> distinct(),
                by = c("entity" = "gemeinde"))

    tier_agg <- res_gm |>
      group_by(tier, topic, topic_label) |>
      summarise(
        n_total    = n(),
        mean_tfidf = mean(tfidf_scaled),
        .groups    = "drop"
      )

    tier_agg <- tier_agg |>
      left_join(topic_info |> select(topic, x_label), by = "topic")

    tier_order <- c("GK 1", "GK 2", "GK 3", "GK 4", "GK 5", "Unbekannt")
    tier_order <- intersect(tier_order, unique(tier_agg$tier))
    tier_agg$tier <- factor(tier_agg$tier, levels = rev(tier_order))
    tier_agg$topic_label <- factor(tier_agg$x_label, levels = topic_info$x_label)
    tier_agg <- tier_agg |>
      mutate(label = round(mean_tfidf, 1))

    make_heatmap_tier(
      tier_agg,
      title = paste0(cl, " ", cl_label, " — Gemeinden nach Grössenklasse (TF-IDF)"),
      subtitle = paste0("Korpus: ", nrow(corpus_gm), " Gemeinden | ",
                        n_topics, " Themen | Metrik: Ø TF-IDF (×1'000)"),
      caption = LESEHILFE_TIER,
      filename_base = paste0("heatmap_", cl, "_gemeinden_tier"),
      width = max(9, n_topics * 2.2),
      height = 7.5
    )

    # Detail: GK 1-3
    cat("  Gemeinden (TF-IDF, Detail GK 1-3)...\n")
    res_gm_detail <- res_gm |>
      filter(tier %in% c("GK 1", "GK 2", "GK 3"))

    if (nrow(res_gm_detail) > 0) {
      res_gm_detail <- res_gm_detail |>
        left_join(topic_info |> select(topic, x_label), by = "topic")

      entity_order <- res_gm_detail |>
        select(entity, tier) |>
        distinct() |>
        arrange(tier, entity) |>
        pull(entity) |>
        rev()

      res_gm_detail$entity <- factor(res_gm_detail$entity, levels = entity_order)
      res_gm_detail$x_label <- factor(res_gm_detail$x_label,
                                       levels = topic_info$x_label)

      n_entities <- length(entity_order)
      make_heatmap_3level(
        res_gm_detail, "x_label", "entity",
        title = paste0(cl, " ", cl_label, " — Gemeinden GK 1-3 (TF-IDF)"),
        subtitle = paste0(n_entities, " Gemeinden (>=5'000 Einw.) | ",
                          n_topics, " Themen | Metrik: TF-IDF (×1'000)"),
        caption = LESEHILFE_DETAIL,
        filename_base = paste0("heatmap_", cl, "_gemeinden_detail"),
        width = max(11, n_topics * 2.2),
        height = max(9, n_entities * 0.32 + 1.5)
      )
    }
  }
}

cat("\n=== Fertig. Alle Heatmaps gespeichert in:", out_dir, "===\n")
