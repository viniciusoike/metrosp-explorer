# metrosp explorer

<!-- badges: start -->
[![Live app](https://img.shields.io/badge/Live%20app-Posit%20Connect-1A6EFF)](https://viniciusoike-metrosp-explorer.share.connect.posit.cloud)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Built with R](https://img.shields.io/badge/Built%20with-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-bslib-447099?logo=rstudio&logoColor=white)](https://shiny.posit.co/)
[![Data: metrosp](https://img.shields.io/badge/Data-metrosp%20(r--universe)-success)](https://viniciusoike.r-universe.dev/metrosp)
<!-- badges: end -->

An interactive dashboard for exploring passenger demand on the **São Paulo metro**,
built with [Shiny](https://shiny.posit.co/) on top of the
[metrosp](https://github.com/viniciusoike/metrosp) R data package. The interface
is in Portuguese.

> **[Try the live app](https://viniciusoike-metrosp-explorer.share.connect.posit.cloud)** —
> deployed on Posit Connect Cloud.

The app is standalone: it lives in its own repository and deploys separately from
the data package.

## Features

- **Line-level demand** — monthly entrance/transported series per line, with KPIs
  and an optional STL trend overlay.
- **Per-station series** — monthly weekday averages and daily counts.
- **Interactive map** — metric views for yearly demand with an animated year
  slider, year-over-year change, recovery vs. 2019, and the network by line.
  Station popups show KPIs and link to each station's series.
- **Dataset downloads** — the package datasets verbatim, in CSV / Excel / GPKG /
  GeoJSON.

## Run locally

Start the app from the repository root.

```r
shiny::runApp(".")
```

## Dependencies

[renv](https://rstudio.github.io/renv/) manages the dependencies. The lockfile
(`renv.lock`) pins every package to an exact version and source, and it takes
`metrosp` from [r-universe](https://viniciusoike.r-universe.dev/metrosp) because
v1.1.1, which adds `station_inauguration`, is ahead of CRAN.

After cloning, restore the project library.

```r
renv::restore()
```

## Deploy

The repository root **is** the app, in Shiny's multi-file layout — `global.R`
(libraries, data prep, helpers, theme, sourced once at startup), `ui.R`,
`server.R`, `www/`, and a committed [`manifest.json`](manifest.json). It
deploys as a unit.

### Posit Connect Cloud (git-backed)

[Connect Cloud](https://connect.posit.cloud/) publishes straight from this public
GitHub repo. When it detects `renv.lock`, it calls `renv::restore()` to install
the packages, including `metrosp` from r-universe. `manifest.json` stays tracked
for app-type metadata, and no `rsconnect` push is required.

### Classic Posit Connect / shinyapps.io

```r
rsconnect::deployApp(appName = "metrosp-explorer")
```

### Updating the lockfile

After adding or upgrading packages, re-snapshot and commit the lockfile.

```r
renv::snapshot()
```

## Data source

Demand data, line/station geometries, and inauguration dates come from the
[metrosp](https://github.com/viniciusoike/metrosp) package
([documentation](https://viniciusoike.github.io/metrosp/), r-universe v1.1.1).
The STL trend overlay uses
[trendseries](https://github.com/viniciusoike/trendseries).

## License

[MIT](LICENSE) © Vinicius Oike
