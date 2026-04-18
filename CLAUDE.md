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

**Run all tests:**
```bash
docker compose run --rm crandore-test
```

**Run a single test file (locally, without Docker):**
```bash
Rscript -e "testthat::test_file('tests/testthat/test-stack-utils.R')"
```

**Run tests locally without Docker** (requires `miniCRAN`, `withr`, `yaml`, `testthat` installed):
```bash
Rscript -e "testthat::test_dir('tests/testthat', reporter = 'progress')"
```
The test helper (`tests/testthat/helper.R`) auto-locates `script/` relative to the working directory, so run from the repo root.

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

### Key function in `stack_init.R`

- `crandore_init()`: generates `stack.yml` from high-level parameters (packages, dates, distros, R versions, Windows/ARM flags). Called directly at Docker entrypoint when `CRANDORE_INIT=true`.

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
| `packages.list` | Example packages file usable via `CRANDORE_PACKAGES_FILE` (one package per line, `#` comments allowed) |

### `stack.yml` structure

```yaml
settings:
  local_root: /minicran      # mount point inside the container
  packages: [tidyverse]      # default package list (can be overridden per profile)
  base_url: ...              # optional: overrides default PPM URL

profiles:
  linux_44:
    os: linux
    r_version: "4.4"
    arch: x86_64
    distros: [jammy, noble]
    # packages: [shiny]      # profile-level packages override settings.packages
    # full_snapshot: true    # download all CRAN packages

snapshots:
  2026-03-08:
    profiles: [linux_44, windows_44]
```

`resolve_builds()` in `stack_utils.R` expands this into one build entry per (date × profile × distro).

### `crandore-init` env vars

These control what `stack.yml` is generated with when running `docker compose run --rm crandore-init`:

| Var | Default | Description |
|-----|---------|-------------|
| `CRANDORE_PACKAGES` | `tidyverse` | Packages to mirror |
| `CRANDORE_INIT_DATES` | today | Comma-separated snapshot dates |
| `CRANDORE_INIT_DISTROS` | `jammy,noble,centos8` | Linux distros |
| `CRANDORE_INIT_R_VERSIONS` | `4.4` | R versions for Linux |
| `CRANDORE_INIT_WINDOWS` | `true` | Include Windows profiles |
| `CRANDORE_INIT_WINDOWS_R_VERSIONS` | (same as Linux) | R versions for Windows |
| `CRANDORE_INIT_ARM` | `false` | Include aarch64 profiles |
| `CRANDORE_FULL_SNAPSHOT` | `false` | Mirror all CRAN packages |

### R dependencies installed in the image

`miniCRAN`, `withr`, `yaml`, `testthat`

### Supported Linux distros (PPM)

`centos7`, `centos8`, `rhel9`, `rhel10`, `opensuse156`, `jammy`, `noble`, `bookworm`, `manylinux_2_28`
