# metrosp explorer

Data-exploration dashboard for the [metrosp](https://github.com/viniciusoike/metrosp)
R data package — passenger demand for the São Paulo metro. This is a **hosted
standalone app**: it lives in its own repository and is deployed separately from
the package (it is not shipped inside it).

Tabs: line-level demand (with KPIs and optional STL trend), per-station monthly +
daily series (with ramp-up shading), an interactive map (click a station to open
it), and dataset downloads (the package datasets verbatim, in CSV/Excel/GPKG/GeoJSON).

## Run locally

From the repository root:

```r
shiny::runApp(".")
```

## Dependencies

All dependencies are on CRAN:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "dplyr", "leaflet", "echarts4r",
  "sf", "htmltools", "htmlwidgets", "writexl", "readr",
  # the data package itself
  "metrosp",
  # optional: enables the STL trend overlay (degrades gracefully if absent)
  "trendseries"
))
```

## Deploy

The repository root is the app (`app.R`, `shared.R`, `www/`), so it deploys as a
unit. On **Posit Connect Cloud** (free tier), publish straight from this public
GitHub repo. On classic Posit Connect / shinyapps.io:

```r
rsconnect::deployApp(appName = "metrosp-explorer")
```

`rsconnect` infers the package set from the loaded namespaces. Every dependency
(`metrosp`, `trendseries`, and the rest) is on CRAN, so any deploy target
resolves them without GitHub access.
