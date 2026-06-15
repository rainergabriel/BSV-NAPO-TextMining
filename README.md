# BSV-NAPO-TextMining

Inhaltsanalyse alterspolitischer Dokumente der Schweizer Kantone und Gemeinden mittels TF-IDF-basiertem Suchbegriff-Matching.

**Projekt:** BSV Mandat G26-02 — Nationale Alterspolitik (Postulat 24.3085)
**Konsortium:** W.I.R.E. | ZHAW | HES-SO | SUPSI

## Überblick

Dieses Repository enthält R-Skripte für die systematische Inhaltsanalyse des Dokumentenkorpus, der im Schwester-Repository [BSV-NAPO-KorpusCrawling](https://github.com/rainergabriel/BSV-NAPO-KorpusCrawling) erstellt wurde. Die Analyse misst, wie stark Schlüsselbegriffe der Alterspolitik in den kantonalen und kommunalen Dokumenten vertreten sind — gewichtet nach ihrer Unterscheidungskraft im Korpus (TF-IDF).

## Voraussetzungen

### R-Pakete

```r
install.packages(c("jsonlite", "ggplot2", "ggtext", "dplyr", "tidyr", "stringr"))
```

Getestet mit R 4.3+.

### Input-Daten

Die JSONL-Dateien müssen in `data/` liegen. Sie werden mit dem Skript `01_create_jsonl_kantone.py` aus dem Repository [BSV-NAPO-KorpusCrawling](https://github.com/rainergabriel/BSV-NAPO-KorpusCrawling) erzeugt.

```
BSV-NAPO-TextMining/
├── data/
│   ├── corpus_kantone.jsonl      ← Input (21 Kantone)
│   ├── corpus_gemeinden.jsonl    ← Input (228 Gemeinden)
│   └── gemeinden_tier.csv        ← Grössenklassen-Zuordnung
├── output/
│   ├── heatmap_C1_kantone.pdf    ← Ergebnis (je Cluster)
│   ├── heatmap_C1_gemeinden_tier.pdf
│   ├── heatmap_C1_gemeinden_detail.pdf
│   └── ...
├── suchbegriffe.csv              ← Suchbegriffe-Tabelle (editierbar)
├── analyse_kantone_heatmap.R
└── README.md
```

## Ausführung

```bash
Rscript analyse_kantone_heatmap.R
```

## Was das Skript tut

1. Liest `suchbegriffe.csv` — die dreisprachige Suchbegriffe-Tabelle, gegliedert nach 4 Clustern und 18 Themen
2. Liest `data/corpus_kantone.jsonl` und `data/corpus_gemeinden.jsonl`
3. Durchsucht jeden Text nach den Suchbegriffen (case-insensitive Substring-Match, DE/FR/IT)
4. Berechnet TF-IDF pro Dokument × Thema
5. Erzeugt pro Cluster drei Heatmaps: Kantone, Gemeinden nach Grössenklasse, und Gemeinden-Detail (GK 1–3)

### Metrik: TF-IDF

Anstelle einer einfachen Häufigkeitszählung verwendet das Skript **TF-IDF** (Term Frequency – Inverse Document Frequency), eine Standard-Metrik aus dem Information Retrieval:

- **TF (Term Frequency):** Summe aller Suchvarianten-Treffer eines Themas, geteilt durch die Wortanzahl des Dokuments
- **IDF (Inverse Document Frequency):** `log(N / df)` — Logarithmus der Korpusgrösse geteilt durch die Anzahl Dokumente mit mindestens einem Treffer
- **TF-IDF = TF × IDF** — Werte werden ×1'000 skaliert für Lesbarkeit

**Vorteil gegenüber roher Häufigkeit:** Themen, die in fast allen Dokumenten vorkommen (z.B. «Gesundheitsversorgung»), werden automatisch abgewichtet. Themen, die nur in wenigen Dokumenten erwähnt werden (z.B. «Nichtbezug»), erhalten höhere Gewichtung dort, wo sie tatsächlich behandelt werden. So werden inhaltliche Schwerpunkte besser sichtbar.

### Klassifizierung

Die TF-IDF-Werte werden in 3 Stufen eingeteilt:

| Stufe | Bedeutung | Schwellenwert |
|-------|-----------|---------------|
| 🔴 nicht erwähnt | Kein Treffer | TF-IDF = 0 |
| 🟠 am Rande erwähnt | Unter dem Median | 0 < TF-IDF ≤ Median |
| 🟢 thematisiert | Über dem Median | TF-IDF > Median |

Der Median wird pro Analyse (Kantone bzw. Gemeinden) aus allen Nicht-Null-Werten berechnet.

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
