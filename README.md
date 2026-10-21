# INNOnet Tarif-Simulator (Task 6.3)

Interaktives [Shiny](https://shiny.posit.co/)-Dashboard, das die Wirkung
zeitvariabler Netztarife auf das Lastprofil von Haushalten simuliert.
Grundlage sind die im INNOnet-Feldversuch geschätzten Tarifeffekte aus zwei
Netzgebieten zweier Netzbetreiber: **Netzbetreiber 1 (NB 1)** mit Hoch- und
Niedertariffenstern (Time-of-Use) und **Netzbetreiber 2 (NB 2)** mit
Ausnahmefenstern (Daily-Peak-Pricing).

Die App lädt ausschließlich kleine, vorberechnete Ergebnistabellen und **keine
Rohdaten**. Eine gehostete Instanz ist unter
<https://innonet-dashboard.jku.at> erreichbar.

## Was die App zeigt

Über die Seitenleiste wählt man **Region**, **Vergleichsgruppe**
(alle Haushalte, E-Auto, PV, Heizung), **Tagestyp** und die
**Tarif-/Signalstunden**. Drei Reiter:

1. **Lastprofil** – stündliche Ø Last je Haushalt, „Ohne Tarif" (Referenz)
   vs. „Mit Tarif" (Referenz + geschätzter Effekt), optional mit
   95 %-Konfidenzband, plus Balken des geschätzten Stundeneffekts.
2. **Dauerlinie** – Lastdauerlinie aus den 15-min-Werten: die Last absteigend
   sortiert über alle Intervalle (links die Spitze).
3. **Kennzahlen** – Spitzenlast, Δ Peak und Ø täglicher Netzbezug je Haushalt,
   ohne vs. mit Tarif.

## Datengrundlage

- **„Ohne Tarif"** = Kontrollgruppe ohne Tarifsignal (Post-Periode).
- **„Mit Tarif"** = Referenz + geschätzter Tarifeffekt
  (Difference-in-Differences) für die gewählten Stunden.
- **Zeitraum:** 09.08.2025 bis 26.11.2025.
- Alle Werte sind **je Haushalt** (Mittel über die Haushalte der Gruppe).

> Die Kurven zeigen die **mittlere** Last je Haushalt, nicht die aggregierte
> Netzlast. Es werden keine Preiselastizitäten modelliert.

## Voraussetzungen

- **R ≥ 4.2** und (empfohlen) RStudio Desktop
- R-Pakete für die App:

```r
install.packages(c(
  "shiny", "dplyr", "tidyr", "ggplot2", "plotly", "here", "RColorBrewer"
))
```

## Starten

Repository klonen oder als ZIP herunterladen, in RStudio die Datei
`innonet-tariff-simulator-T6.3.Rproj` öffnen und ausführen:

```r
shiny::runApp("shiny")
```

Alternativ `shiny/app.R` öffnen und auf **Run App** klicken.

## Struktur

```text
.
├── shiny/
│   ├── app.R                       # die Shiny-App
│   └── www/                        # Logos für Kopf- und Fußzeile
├── data/
│   ├── coefs_nb1_did.rds           # Tarifeffekte NB 1
│   ├── coefs_nb2_did.rds           # Tarifeffekte NB 2
│   ├── baseline_profiles.rds       # Lastprofil (Durchschnittstag)
│   ├── baseline_duration.rds       # 15-min-Werte für die Dauerlinie
│   └── sim_meta.rds                # Zeitraum, Haushaltszahlen, Signalabdeckung
├── R/
│   ├── setup.R                     # Pakete, Pfade, Auswertungszeitraum
│   ├── 01_prepare_data.R           # Rohdaten laden und bereinigen
│   └── 02_estimate_models.R        # Schätzung + Basisprofile -> data/
└── innonet-tariff-simulator-T6.3.Rproj
```

## Reproduktion der Ergebnisdateien

Die fünf `.rds`-Dateien in `data/` werden von `R/02_estimate_models.R`
erzeugt. Dafür werden die Feldversuchsdaten benötigt
(`data_simulator_nb1.rds`, `data_simulator_nb2.rds`). Diese enthalten sensible
Haushaltsdaten und sind **nicht** Teil des Repositorys. Für die Nutzung des
Dashboards ist das nicht erforderlich.

Wer Zugang zu den Rohdaten hat, legt sie in einen Ordner `data/raw/` und führt
die beiden Skripte nacheinander aus:

```r
source("R/01_prepare_data.R")     # -> data/processed/ (Zwischenstand)
source("R/02_estimate_models.R")  # -> data/
```

`data/raw/` und `data/processed/` sind per `.gitignore` ausgeschlossen und
werden nie mitveröffentlicht.
