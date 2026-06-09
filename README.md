# BSV-NAPO-TextMining

Inhaltsanalyse alterspolitischer Dokumente der Schweizer Kantone und Gemeinden mittels Suchbegriff-Matching.

**Projekt:** BSV Mandat G26-02 — Nationale Alterspolitik (Postulat 24.3085)
**Konsortium:** W.I.R.E. | ZHAW | HES-SO | SUPSI

## Überblick

Dieses Repository enthält R-Skripte für die systematische Inhaltsanalyse des Dokumentenkorpus, der im Schwester-Repository [BSV-NAPO-KorpusCrawling](https://github.com/rainergabriel/BSV-NAPO-KorpusCrawling) erstellt wurde. Die Analyse prüft, ob Schlüsselbegriffe der Alterspolitik in den kantonalen und kommunalen Dokumenten vorkommen.

## Voraussetzungen

### R-Pakete

```r
install.packages(c("jsonlite", "ggplot2", "dplyr", "tidyr", "stringr"))
```

Getestet mit R 4.3+.

### Input-Daten

Die JSONL-Datei `corpus_kantone.jsonl` muss in `data/` liegen. Sie wird mit dem Skript `01_create_jsonl_kantone.py` aus dem Repository [BSV-NAPO-KorpusCrawling](https://github.com/rainergabriel/BSV-NAPO-KorpusCrawling) erzeugt.

```
BSV-NAPO-TextMining/
├── data/
│   └── corpus_kantone.jsonl   ← Input (aus BSV-NAPO-KorpusCrawling)
├── output/
│   ├── heatmap_kantone_begriffe.pdf   ← Ergebnis
│   └── heatmap_kantone_begriffe.png   ← Ergebnis
├── analyse_kantone_heatmap.R
└── README.md
```

## Ausführung

```bash
Rscript analyse_kantone_heatmap.R
```

## Was das Skript tut

1. Liest `data/corpus_kantone.jsonl` (Volltext pro Kanton, eine Zeile pro Dokument)
2. Durchsucht jeden Text nach 5 Schlüsselbegriffen (case-insensitive Substring-Match, dreisprachig DE/FR/IT):
   - **Nichtbezug / Non-recours** — Nichtbezug von Sozialleistungen
   - **Armut / Pauvreté** — Armut, Armutsrisiko
   - **Prekarität / Précarité** — prekäre Lebenssituationen
   - **Erwerbsarbeit / Activité prof.** — Erwerbstätigkeit im Alter
   - **Betreuung / Accompagnement** — Betreuungsleistungen
3. Erzeugt eine binäre Heatmap (Kanton × Begriff, ja/nein) als PDF und PNG

## Suchbegriffe erweitern

Im Skript `analyse_kantone_heatmap.R` das `suchbegriffe`-List-Objekt ergänzen:

```r
suchbegriffe[["Neuer Begriff / Nouveau terme"]] <- c(
  "deutsch_variante1", "deutsch_variante2",
  "francais_variante1",
  "italiano_variante1"
)
```

## Verwandte Repositories

- [BSV-NAPO-KorpusCrawling](https://github.com/rainergabriel/BSV-NAPO-KorpusCrawling) — Korpus-Aufbau: PDF-Sammlung, Textextraktion, Python-basierte Heatmaps
