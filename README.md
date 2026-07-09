# metrosp explorer

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Built with R](https://img.shields.io/badge/Built%20with-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-bslib-447099?logo=rstudio&logoColor=white)](https://shiny.posit.co/)
[![Deploy: Posit Connect Cloud](https://img.shields.io/badge/Deploy-Posit%20Connect%20Cloud-1A6EFF)](https://connect.posit.cloud/)
[![Data: metrosp](https://img.shields.io/badge/Data-metrosp%20(r--universe)-success)](https://viniciusoike.r-universe.dev/metrosp)
<!-- badges: end -->

An interactive dashboard for exploring passenger demand on the **São Paulo metro**,
built with [Shiny](https://shiny.posit.co/) on top of the
[metrosp](https://github.com/viniciusoike/metrosp) R data package.

This is a **hosted standalone app**: it lives in its own repository and is
deployed separately from the data package (it is not shipped inside it).

## Features

- **Line-level demand** — monthly entrance/transported series per line, with KPIs
  and an optional STL trend overlay.
- **Per-station series** — monthly weekday averages and daily counts, with
  ramp-up shading around each station's inauguration.
- **Interactive map** — four metric views: yearly demand with an animated year
  slider, year-over-year change, recovery vs. 2019, and the network by line.
  Station popups show KPIs and link straight to each station's series.
- **Dataset downloads** — the package datasets verbatim, in CSV / Excel / GPKG /
  GeoJSON.

## Run locally

From the repository root:

```r
shiny::runApp(".")
```

## Dependencies

Dependencies are managed with [renv](https://rstudio.github.io/renv/). The
lockfile (`renv.lock`) pins every package to an exact version and source.
`metrosp` is pinned to [r-universe](https://viniciusoike.r-universe.dev/metrosp)
because v1.1.1 (which adds `station_inauguration`) is ahead of CRAN.

After cloning, restore the project library with:

```r
renv::restore()
```

## Deploy

The repository root **is** the app, in Shiny's multi-file layout: `global.R`
(libraries, data prep, helpers, theme — sourced once at startup), `ui.R`,
`server.R`, `www/`, and a committed [`manifest.json`](manifest.json). It
deploys as a unit.

### Posit Connect Cloud (git-backed)

[Connect Cloud](https://connect.posit.cloud/) publishes straight from this public
GitHub repo. When it detects `renv.lock`, it calls `renv::restore()` to install
packages — including `metrosp` from r-universe. `manifest.json` is still tracked
for app-type metadata; no `rsconnect` push required.

### Classic Posit Connect / shinyapps.io

```r
rsconnect::deployApp(appName = "metrosp-explorer")
```

### Updating the lockfile

After adding or upgrading packages, re-snapshot and commit:

```r
renv::snapshot()
```

## Data source

Demand data, line/station geometries, and inauguration dates come from the
[metrosp](https://github.com/viniciusoike/metrosp) package (r-universe v1.1.1).
The STL trend overlay uses
[trendseries](https://github.com/viniciusoike/trendseries).

## License

[MIT](LICENSE) © Vinicius Oike
