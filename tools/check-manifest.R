# Structural check that manifest.json is in sync with the app ----
# Connect (Cloud) deploys the files and packages listed in manifest.json. This
# guards the two ways the manifest silently drifts: a runtime file that isn't
# listed (so it won't deploy) or a package used in code but not pinned (so the
# restore fails). It deliberately does NOT diff package *versions* — those churn
# every time a CRAN dependency updates and would make the check flaky. Refresh
# versions with rsconnect::writeManifest(); this only checks coverage.
#
# Run from the app root: Rscript tools/check-manifest.R

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to read manifest.json")
}

manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
manifest_files <- names(manifest$files)
manifest_pkgs <- names(manifest$packages)

problems <- character(0)

# Runtime files: top-level .R plus everything under www/ ----
runtime_files <- c(
  list.files(".", pattern = "\\.R$"),
  list.files("www", recursive = TRUE, full.names = TRUE) |>
    sub("^\\./", "", x = _)
)

missing_files <- setdiff(runtime_files, manifest_files)
if (length(missing_files) > 0) {
  problems <- c(
    problems,
    paste0(
      "Files used by the app but absent from manifest.json: ",
      paste(missing_files, collapse = ", "),
      "\n  -> regenerate with rsconnect::writeManifest(appDir = \".\")"
    )
  )
}

# Packages referenced in code but not pinned in the manifest ----
code <- unlist(lapply(
  c("global.R", "ui.R", "server.R"),
  readLines,
  warn = FALSE,
  encoding = "UTF-8"
))
txt <- paste(code, collapse = "\n")

grab <- function(pattern) {
  regmatches(txt, gregexpr(pattern, txt, perl = TRUE))[[1]]
}

lib_pkgs <- gsub("library\\(|\\)", "", grab("library\\([A-Za-z0-9.]+\\)"))
ns_pkgs <- gsub("::$", "", grab("[A-Za-z][A-Za-z0-9.]+::"))
rns_pkgs <- gsub(
  "requireNamespace\\(\"|\".*",
  "",
  grab("requireNamespace\\(\"[A-Za-z0-9.]+\"")
)

# Base/recommended packages ship with R and aren't pinned in the manifest.
# "pak" is excluded too: it appears only inside an install-advice string
# (pak::pak("...")) in a message(), not as a runtime dependency.
base_pkgs <- c(
  "base", "utils", "stats", "methods", "grDevices", "graphics",
  "datasets", "tools", "grid", "splines", "pak"
)

used_pkgs <- setdiff(unique(c(lib_pkgs, ns_pkgs, rns_pkgs)), base_pkgs)
missing_pkgs <- setdiff(used_pkgs, manifest_pkgs)
if (length(missing_pkgs) > 0) {
  problems <- c(
    problems,
    paste0(
      "Packages used in code but not pinned in manifest.json: ",
      paste(missing_pkgs, collapse = ", "),
      "\n  -> install them, then rsconnect::writeManifest(appDir = \".\")"
    )
  )
}

if (length(problems) > 0) {
  stop(
    "manifest.json is out of sync with the app:\n",
    paste(problems, collapse = "\n"),
    call. = FALSE
  )
}

cat(sprintf(
  "manifest.json OK: %d files, %d packages cover all app files and code references.\n",
  length(manifest_files),
  length(manifest_pkgs)
))
