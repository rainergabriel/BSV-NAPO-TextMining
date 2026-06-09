##############################################################################
# Heatmap-Analyse: Suchbegriffe in kantonalen Alterskonzepten
# BSV Mandat G26-02 — Nationale Alterspolitik
#
# Liest corpus_kantone.jsonl (Volltextkorpus, eine Zeile pro Kanton),
# durchsucht jeden Text nach vordefinierten Suchbegriffen (DE/FR/IT)
# und erzeugt eine binaere Heatmap (PDF + PNG).
#
# Voraussetzungen:
#   install.packages(c("jsonlite", "ggplot2", "dplyr", "tidyr", "stringr"))
#
# Aufruf:
#   Rscript analyse_kantone_heatmap.R
#
# Input:  data/corpus_kantone.jsonl
# Output: output/heatmap_kantone_begriffe.pdf
#         output/heatmap_kantone_begriffe.png
##############################################################################

library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# --- Pfade (relativ zum Skript-Verzeichnis) ---
script_dir <- if (interactive()) {
  getwd()
} else {
  dirname(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))] |>
    sub("--file=", "", x = _))
}
setwd(script_dir)

data_dir   <- file.path(script_dir, "data")
out_dir    <- file.path(script_dir, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

jsonl_path <- file.path(data_dir, "corpus_kantone.jsonl")

if (!file.exists(jsonl_path)) {
  stop(paste0(
    "ERROR: ", jsonl_path, " nicht gefunden.\n",
    "Bitte corpus_kantone.jsonl in data/ ablegen.\n",
    "Erzeugt mit: python3 01_create_jsonl_kantone.py (siehe BSV-NAPO-KorpusCrawling)"
  ))
}

# --- Korpus einlesen ---
lines <- readLines(jsonl_path, encoding = "UTF-8")
corpus <- bind_rows(lapply(lines, fromJSON))
cat("Korpus geladen:", nrow(corpus), "Kantone\n")

# --- Suchbegriffe definieren ---
# Jeder Begriff hat ein Label und Suchvarianten in DE/FR/IT.
# Case-insensitive Substring-Match.
# Neue Begriffe einfach hier ergaenzen.
suchbegriffe <- list(
  "Nichtbezug / Non-recours" = c(
    "nichtbezug", "nicht-bezug",
    "non-recours", "non recours",
    "mancato ricorso", "mancato utilizzo",
    "rinuncia alle prestazioni"
  ),
  "Armut / Pauvreté" = c(
    "armut", "armutsgefährd", "armutsbetroffen", "armutsrisiko",
    "pauvreté", "pauvre",
    "povertà", "poveri"
  ),
  "Prekarität / Précarité" = c(
    "prekarität", "prekär",
    "précarité", "précaire",
    "precarietà", "precari"
  ),
  "Erwerbsarbeit / Activité prof." = c(
    "erwerbsarbeit", "erwerbstätig", "erwerbsleben",
    "activité professionnelle", "activité lucrative",
    "attività professionale", "attività lucrativa",
    "travail rémunéré"
  ),
  "Betreuung / Accompagnement" = c(
    "betreuung",
    "accompagnement", "aide à domicile", "aide a domicile",
    "presa in carico", "assistenza"
  )
)

# --- Suche durchfuehren ---
ergebnisse <- expand.grid(
  kanton = corpus$kanton,
  begriff = names(suchbegriffe),
  stringsAsFactors = FALSE
) |>
  mutate(gefunden = NA_integer_)

for (i in seq_len(nrow(ergebnisse))) {
  kt <- ergebnisse$kanton[i]
  bg <- ergebnisse$begriff[i]

  text <- corpus$text[corpus$kanton == kt]
  text_lower <- str_to_lower(text)

  varianten <- suchbegriffe[[bg]]
  treffer <- any(sapply(varianten, function(v) str_detect(text_lower, fixed(v))))

  ergebnisse$gefunden[i] <- as.integer(treffer)
}

# --- Kantonsreihenfolge (alphabetisch, von oben nach unten) ---
kt_order <- sort(unique(ergebnisse$kanton), decreasing = TRUE)
ergebnisse$kanton <- factor(ergebnisse$kanton, levels = kt_order)
ergebnisse$begriff <- factor(ergebnisse$begriff, levels = names(suchbegriffe))

ergebnisse <- ergebnisse |>
  mutate(label = ifelse(gefunden == 1, "ja", "nein"))

# --- Heatmap ---
p <- ggplot(ergebnisse, aes(x = begriff, y = kanton, fill = factor(gefunden))) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("0" = "#D32F2F", "1" = "#388E3C"),
    labels = c("nicht erwähnt", "erwähnt"),
    name = ""
  ) +
  labs(
    title = "Erwähnung von Schlüsselbegriffen in kantonalen Alterskonzepten",
    subtitle = paste0("Korpus: ", nrow(corpus), " Kantone mit Dokument (von 26)"),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40")
  )

# --- Speichern ---
pdf_path <- file.path(out_dir, "heatmap_kantone_begriffe.pdf")
ggsave(pdf_path, p, width = 10, height = 10, device = "pdf")
cat("Gespeichert:", pdf_path, "\n")

png_path <- file.path(out_dir, "heatmap_kantone_begriffe.png")
ggsave(png_path, p, width = 10, height = 10, dpi = 300)
cat("Gespeichert:", png_path, "\n")

# --- Zusammenfassung ---
cat("\n--- Ergebnisse ---\n")
for (bg in names(suchbegriffe)) {
  treffer_kt <- ergebnisse |>
    filter(begriff == bg, gefunden == 1) |>
    pull(kanton) |>
    as.character()
  cat(sprintf("  %s: %d/%d Kantone (%s)\n",
              bg, length(treffer_kt), nrow(corpus),
              ifelse(length(treffer_kt) > 0, paste(treffer_kt, collapse = ", "), "—")))
}
