# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Crandore builds frozen, self-hosted CRAN mirrors from the [Posit Public Package Manager (PPM)](https://packagemanager.posit.co/cran). Users point their R installation at the local mirror instead of the public CRAN.

## Build and Run

**Build the image:**
```bash
docker build -t crandore .
```

**Standard workflow (docker compose):**
```bash
cp .env.example .env                             # configure paths/ports
docker compose run --rm crandore-init            # generate stack.yml
docker compose run --rm crandore-stack           # download packages
docker compose up -d crandore-serve              # serve over HTTP
```

**Filter a build:**
```bash
CRANDORE_ONLY_DATE=2026-03-08 docker compose run --rm crandore-stack
CRANDORE_ONLY_PROFILE=linux_44 docker compose run --rm crandore-stack
```

**Run tests:**
```bash
docker compose run --rm crandore-test
```

**Single build without docker compose:**
```bash
docker run --rm -v /srv/cran-repos:/minicran \
  -e CRANDORE_PACKAGES=tidyverse crandore
```

## Architecture

### Scripts

| File | Role |
|------|------|
| `script/repos_snapshot.R` | Core engine — downloads packages and generates PACKAGES index |
| `script/stack_runner.R` | Stack mode orchestrator — reads `stack.yml`, calls `crandore()` in a loop |
| `script/stack_utils.R` | Pure function `resolve_builds()` — expands profiles × snapshots into a flat build list |
| `script/stack_init.R` | Generates a starter `stack.yml` from parameters; also the Docker entrypoint for `crandore-init` |

### Docker entrypoint logic

The Dockerfile CMD checks env vars to choose the operating mode:

1. `CRANDORE_INIT=true` → runs `stack_init.R` (generates `stack.yml`)
2. `CRANDORE_STACK_FILE` set → runs `stack_runner.R` (builds from `stack.yml`)
3. Otherwise → runs `crandore()` directly (single build)

### Path structure

Crandore creates repos at `<local_root>/<os>/<distro>-<arch>/R-<major.minor>/`:
- Linux: `.../src/contrib/*.tar.gz` + `PACKAGES`, `PACKAGES.gz`, `PACKAGES.rds`
- Windows: `.../bin/windows/contrib/<major.minor>/*.zip` + `PACKAGES*`, and a copy in `.../src/contrib/` for R auto-detection

**The date is not embedded in the path by Crandore.** To create date-based directories, set `CRANDORE_REPOS_PATH` (or the volume mount) to a date-prefixed path on the host.

### Key functions in `repos_snapshot.R`

- `crandore()` (~line 650): public API — reads env vars, calls `crandore_()`
- `crandore_()` (~line 380): core — orchestrates download loop and index generation
- `available_packages()`: queries PPM for available packages
- `download_one()`: downloads a single package (Linux via `miniCRAN::makeRepo`, Windows via `utils::download.packages`)
- `index_needs_update()`: cache check by package count and file timestamps
- `cleanup_obsolete_packages()`: removes packages no longer in the target list

### Two download modes

- **Partial** (default): resolves transitive dependencies via `miniCRAN::pkgDep()`, downloads only what's needed
- **Full snapshot** (`CRANDORE_FULL_SNAPSHOT=true`): downloads every package in the PPM snapshot

### Resume behaviour

`CRANDORE_RESUME=true` (default) skips already-downloaded packages. Safe to interrupt and restart at any point.

### Configuration files

| File | Purpose |
|------|---------|
| `.env.example` | Template — copy to `.env` and edit |
| `.env` | Local config (gitignored): `CRANDORE_REPOS_PATH`, ports, domain |
| `stack.yml` | Declares what to build: profiles (platform + R version) × snapshot dates |
| `Caddyfile` | Caddy config for the serve services; uses `{$VAR}` substitution from `.env` |

### R dependencies installed in the image

`miniCRAN`, `withr`, `yaml`, `testthat`

### Supported Linux distros (PPM)

`centos7`, `centos8`, `rhel9`, `rhel10`, `opensuse156`, `jammy`, `noble`, `bookworm`, `manylinux_2_28`
