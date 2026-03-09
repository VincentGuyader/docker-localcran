# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Crandore is a Docker-based tool that creates local CRAN (Comprehensive R Archive Network) repository snapshots by downloading R packages from the [Posit Public Package Manager (PPM)](https://packagemanager.posit.co/cran). The primary implementation is a single R script (`script/repos_snapshot.R`) wrapped in a Docker container.

## Build and Run

**Build the Docker image:**
```bash
docker build -t crandore .
```

**Run with specific packages:**
```bash
docker run -v ./minicran:/minicran -e CRANDORE_PACKAGES="tidyverse,data.table" crandore
```

**Run a full snapshot (all available packages):**
```bash
docker run -v ./minicran:/minicran -e CRANDORE_FULL_SNAPSHOT=true crandore
```

**Run using a packages file:**
```bash
docker run -v ./minicran:/minicran -v ./packages.list:/tmp/packages.list \
  -e CRANDORE_PACKAGES_FILE=/tmp/packages.list crandore
```

**Run the R script directly (without Docker):**
```r
source("script/repos_snapshot.R")
result <- crandore(packages = "tidyverse", cleanup = TRUE)
```

There are no formal test or lint commands configured in this project.

## Architecture

**Single-script design:** All logic lives in `script/repos_snapshot.R`. The Dockerfile installs R dependencies (`miniCRAN`, `withr`) and runs this script as its entrypoint.

**Two operating modes:**
- *Full snapshot*: Downloads all packages available from PPM (`CRANDORE_FULL_SNAPSHOT=true`)
- *Partial snapshot* (default): Downloads only specified packages plus their transitive dependencies via `miniCRAN::pkgDep()`

**Cross-platform repository generation:** Produces installable repositories for multiple Linux distros (jammy, noble, bookworm, centos7, rhel9, etc.), Windows, and architectures (x86_64, aarch64). Repository layout:
- Linux: `<local_root>/<os>/<distro>-<arch>/R-<major.minor>/src/contrib/`
- Windows: `<local_root>/windows/bin/windows/contrib/<major.minor>/` and a `src/contrib/` copy for auto-detection

**Key functions in `repos_snapshot.R`:**
- `crandore()` (line ~650): Public API — reads env vars, calls `crandore_()`
- `crandore_()` (line ~380): Core implementation — orchestrates downloads and indexing
- `available_packages()`: Queries PPM for available packages
- `download_one()`: Downloads a single package with error handling
- `cleanup_obsolete_packages()`: Removes packages no longer in the target list
- `index_needs_update()`: Cache validation by count/timestamp comparison

**Resume / caching:** By default (`CRANDORE_RESUME=true`) already-downloaded packages are skipped. Index files (PACKAGES, PACKAGES.gz, PACKAGES.rds) are only regenerated when the count or timestamp indicates a change, unless `CRANDORE_UPDATE_INDEX=force`.

**Configuration is entirely via environment variables** — all `CRANDORE_*` vars are documented in the README.
