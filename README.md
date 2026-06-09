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
├── suchbegriffe.csv               ← Suchbegriffe-Tabelle (editierbar)
├── analyse_kantone_heatmap.R
└── README.md
```

## Ausführung

```bash
Rscript analyse_kantone_heatmap.R
```

## Was das Skript tut

1. Liest `suchbegriffe.csv` — die dreisprachige Suchbegriffe-Tabelle, gegliedert nach 4 Clustern und 18 Themen
2. Liest `data/corpus_kantone.jsonl` (Volltext pro Kanton, eine Zeile pro Dokument)
3. Durchsucht jeden Text nach den Suchbegriffen (case-insensitive Substring-Match, DE/FR/IT)
4. Erzeugt pro Cluster eine binäre Heatmap (Kanton × Thema, ja/nein) als PDF und PNG

### Cluster

| Cluster | Bezeichnung | Themen |
|---------|------------|--------|
| C1 | Gesundheit und Autonomie | 4 |
| C2 | Teilhabe | 4 |
| C3 | Materielle Sicherheit | 5 |
| C4 | Schutz und Rechte | 5 |

## Suchbegriffe anpassen

Die Suchbegriffe werden in `suchbegriffe.csv` definiert (Semikolon-getrennte Varianten pro Sprache). Die Datei kann in Excel oder einem Texteditor bearbeitet werden.

Spalten: `cluster`, `cluster_label`, `topic`, `topic_label`, `term_de`, `term_fr`, `term_it`

## Verwandte Repositories

- [BSV-NAPO-KorpusCrawling](https://github.com/rainergabriel/BSV-NAPO-KorpusCrawling) — Korpus-Aufbau: PDF-Sammlung, Textextraktion, Python-basierte Heatmaps
