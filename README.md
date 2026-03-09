# Crandore

Crandore allows you to create local CRAN repository snapshots using Docker and {miniCRAN}.
The container is smart: it only downloads the necessary packages and their dependencies.
You can interrupt and resume the process without losing time.

## Prerequisites

- Docker installed on your system
- A local directory to store the snapshot (e.g., `./minicran`)

---

## Two ways to use Crandore

| Mode | When to use |
|------|-------------|
| **Stack mode** (recommended) | Build multiple repos (dates × platforms × R versions) from a single `stack.yml` |
| **Single build** | Build one repo manually via environment variables |

---

## Stack Mode — `stack.yml`

The stack file describes all the repositories you want to build. It separates
**profiles** (what platform/R version to target) from **snapshots** (which dates to build).
Share this file with a colleague and they can reproduce your entire setup with one command.

### Structure

```yaml
settings:
  local_root: /minicran          # where repos are stored (inside the container)
  packages:                      # default package list (used by all profiles unless overridden)
    - tidyverse

profiles:
  <profile_name>:
    os: linux | windows
    r_version: "4.4"
    # linux only:
    arch: x86_64 | aarch64
    distros: [jammy, noble, centos8, ...]
    # optional overrides:
    packages: [pkg1, pkg2]       # overrides settings.packages for this profile
    full_snapshot: true          # download the entire CRAN (ignores packages)

snapshots:
  YYYY-MM-DD:
    profiles: [profile_name, ...]
```

### Running

```bash
# Build everything defined in stack.yml
docker compose run --rm crandore-stack

# Only one snapshot date
CRANDORE_ONLY_DATE=2026-03-08 docker compose run --rm crandore-stack

# Only one profile (across all dates)
CRANDORE_ONLY_PROFILE=linux_44 docker compose run --rm crandore-stack

# Combine both filters
CRANDORE_ONLY_DATE=2026-03-08 CRANDORE_ONLY_PROFILE=windows_45 docker compose run --rm crandore-stack
```

Output before execution shows a summary of all builds:

```
=== Crandore Stack : 6 build(s) à exécuter ===

  [linux_44]   2026-03-08 | jammy/x86_64   | R 4.4 | tidyverse
  [linux_44]   2026-03-08 | noble/x86_64   | R 4.4 | tidyverse
  [windows_45] 2026-03-08 | windows/x86_64 | R 4.5 | tidyverse
  [linux_44]   2025-09-08 | jammy/x86_64   | R 4.4 | tidyverse
  [linux_44]   2025-09-08 | noble/x86_64   | R 4.4 | tidyverse
  [windows_45] 2025-09-08 | windows/x86_64 | R 4.5 | tidyverse
```

---

### Example 1 — Minimal: one package, one platform, one date

```yaml
settings:
  local_root: /minicran
  packages:
    - dplyr

profiles:
  linux_noble:
    os: linux
    arch: x86_64
    r_version: "4.4"
    distros: [noble]

snapshots:
  2026-03-08:
    profiles: [linux_noble]
```

Produces: `<local_root>/2026-03-08/linux/noble-x86_64/R-4.4/src/contrib/`

---

### Example 2 — Multi-platform: Linux + Windows, same snapshot

Typical for a team with mixed environments (Ubuntu servers + Windows laptops).

```yaml
settings:
  local_root: /minicran
  packages:
    - tidyverse
    - shiny

profiles:
  linux_44:
    os: linux
    arch: x86_64
    r_version: "4.4"
    distros: [jammy, noble]

  windows_44:
    os: windows
    r_version: "4.4"

  windows_45:
    os: windows
    r_version: "4.5"

snapshots:
  2026-03-08:
    profiles: [linux_44, windows_44, windows_45]
```

Use in R on Linux (Ubuntu Noble, R 4.4):
```r
options(repos = c(CRAN = "https://your-server/2026-03-08/linux/noble-x86_64/R-4.4/"))
```

Use in R on Windows (R 4.5):
```r
options(repos = c(CRAN = "https://your-server/2026-03-08/windows/windows-x86_64/R-4.5/"))
```

---

### Example 3 — Multiple snapshot dates for reproducibility

Keep several frozen snapshots so users can pin their installation to a known date.

```yaml
settings:
  local_root: /minicran
  packages:
    - tidyverse

profiles:
  linux_44:
    os: linux
    arch: x86_64
    r_version: "4.4"
    distros: [jammy, noble, centos8]

  linux_43:
    os: linux
    arch: x86_64
    r_version: "4.3"
    distros: [jammy, centos8]

  windows_45:
    os: windows
    r_version: "4.5"

  windows_44:
    os: windows
    r_version: "4.4"

  windows_43:
    os: windows
    r_version: "4.3"

snapshots:
  2026-03-08:                             # today
    profiles: [linux_44, windows_44, windows_45]

  2025-09-08:                             # 6 months ago
    profiles: [linux_44, windows_44, windows_45]

  2024-03-08:                             # 2 years ago — R 4.3 era
    profiles: [linux_43, windows_43]
```

---

### Example 4 — Per-profile package lists

Different platforms need different packages. For instance, a Shiny server needs
`shiny` + `ggplot2`, while an ETL server only needs `data.table` + `arrow`.

```yaml
settings:
  local_root: /minicran
  packages:
    - dplyr                        # fallback if a profile has no packages key

profiles:
  shiny_server:
    os: linux
    arch: x86_64
    r_version: "4.4"
    distros: [noble]
    packages:                      # overrides settings.packages
      - shiny
      - ggplot2
      - bslib
      - plotly

  etl_server:
    os: linux
    arch: x86_64
    r_version: "4.4"
    distros: [jammy, centos8]
    packages:
      - data.table
      - arrow
      - duckdb

  analyst_windows:
    os: windows
    r_version: "4.5"
    packages:
      - tidyverse
      - readxl
      - openxlsx2

snapshots:
  2026-03-08:
    profiles: [shiny_server, etl_server, analyst_windows]
```

Build only the ETL repos:
```bash
CRANDORE_ONLY_PROFILE=etl_server docker compose run --rm crandore-stack
```

---

### Example 5 — Full CRAN mirror for one distro

Use `full_snapshot: true` in a profile to download all available packages.
Useful for an internal mirror serving an entire organisation.

```yaml
settings:
  local_root: /minicran
  packages:
    - tidyverse                    # ignored by full_snapshot profiles

profiles:
  full_mirror_noble:
    os: linux
    arch: x86_64
    r_version: "4.4"
    distros: [noble]
    full_snapshot: true            # downloads the entire CRAN

  light_windows:
    os: windows
    r_version: "4.5"
    # no full_snapshot → uses settings.packages (tidyverse)

snapshots:
  2026-03-08:
    profiles: [full_mirror_noble, light_windows]
```

---

### Example 6 — aarch64 (ARM) alongside x86_64

For teams running ARM-based servers (AWS Graviton, Apple Silicon via Rosetta, etc.).

```yaml
settings:
  local_root: /minicran
  packages:
    - tidyverse

profiles:
  linux_x86:
    os: linux
    arch: x86_64
    r_version: "4.4"
    distros: [noble, jammy]

  linux_arm:
    os: linux
    arch: aarch64
    r_version: "4.4"
    distros: [noble, jammy]

  windows_45:
    os: windows
    r_version: "4.5"

snapshots:
  2026-03-08:
    profiles: [linux_x86, linux_arm, windows_45]
```

---

## Single Build Mode — environment variables

For quick one-off builds without a stack file.

```bash
# Current platform, specific packages
docker run -v ./minicran:/minicran \
  -e CRANDORE_PACKAGES="tidyverse" \
  crandore

# Ubuntu Noble, R 4.4, frozen date
docker run -v ./minicran:/minicran \
  -e CRANDORE_OS=linux \
  -e CRANDORE_DISTRO=noble \
  -e CRANDORE_SNAPSHOT_DATE=2026-03-08 \
  -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES="tidyverse" \
  crandore

# Windows binaries, R 4.5
docker run -v ./minicran:/minicran \
  -e CRANDORE_OS=windows \
  -e CRANDORE_R_VERSION=4.5.0 \
  -e CRANDORE_PACKAGES="tidyverse" \
  crandore

# Full CRAN mirror for Ubuntu Jammy
docker run -v ./minicran:/minicran \
  -e CRANDORE_OS=linux \
  -e CRANDORE_DISTRO=jammy \
  -e CRANDORE_FULL_SNAPSHOT=true \
  crandore
```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `CRANDORE_OS` | Target OS: `linux` or `windows` | Current OS |
| `CRANDORE_DISTRO` | Linux distribution: `jammy`, `noble`, `centos8`, `bookworm`, etc. | Auto-detected |
| `CRANDORE_ARCH` | Architecture: `x86_64` or `aarch64` | Current arch |
| `CRANDORE_SNAPSHOT_DATE` | CRAN snapshot date (`YYYY-MM-DD` or `latest`) | Today |
| `CRANDORE_R_VERSION` | Target R version (e.g., `4.5.0`) | Current R version |
| `CRANDORE_FULL_SNAPSHOT` | Download all available packages | `false` |
| `CRANDORE_PACKAGES` | Comma-separated package list | Empty |
| `CRANDORE_PACKAGES_FILE` | Path to packages file (one per line, takes priority) | Empty |
| `CRANDORE_CLEANUP` | Remove obsolete packages (partial mode only) | `false` |
| `CRANDORE_UPDATE_INDEX` | Index generation: `true` (smart), `false` (skip), `force` | `true` |
| `CRANDORE_RESUME` | Skip already downloaded packages | `true` |
| `CRANDORE_LOCAL_ROOT` | Root directory for repos inside the container | `./minicran` |
| `CRANDORE_BASE_URL` | PPM base URL | `https://packagemanager.posit.co/cran` |
| `CRANDORE_VERBOSE` | Verbose output | `true` |

---

## Programmatic Usage (R)

```r
source("script/repos_snapshot.R")

# Simple
result <- crandore(packages = "tidyverse", cleanup = TRUE)

# Full configuration
result <- crandore(
  os            = "linux",
  distro        = "noble",
  snapshot_date = "2026-03-08",
  r_version     = "4.4.0",
  packages      = "dplyr,ggplot2",
  local_root    = "/srv/cran-repos/2026-03-08",
  update_index  = "force",
  verbose       = TRUE
)
```

---

## Local Repository Structure

Each build populates a path of the form:

```
<local_root>/
  linux/
    <distro>-<arch>/
      R-<major.minor>/
        src/contrib/           ← .tar.gz packages + PACKAGES, PACKAGES.gz, PACKAGES.rds
  windows/
    windows-x86_64/
      R-<major.minor>/
        bin/windows/contrib/<major.minor>/   ← .zip binaries + PACKAGES*
        src/contrib/                         ← PACKAGES* copy (for R auto-detection)
```

When using `stack.yml` with `local_root: /minicran` and mounting `-v /srv/cran-repos/<date>:/minicran`,
the date becomes the top-level directory on the host, giving:

```
/srv/cran-repos/
  2026-03-08/linux/jammy-x86_64/R-4.4/src/contrib/
  2026-03-08/linux/noble-x86_64/R-4.4/src/contrib/
  2026-03-08/windows/windows-x86_64/R-4.5/bin/windows/contrib/4.5/
  2025-09-08/linux/jammy-x86_64/R-4.4/src/contrib/
  ...
```

**Windows repositories** include PACKAGES files in both `bin/windows/contrib/` and `src/contrib/`
so that R auto-detects binaries without requiring `type = "win.binary"`.

---

## Running Tests

```bash
docker compose run --rm crandore-test
```

Tests cover all pure functions (`r_major_minor`, `validate_snapshot_date`, `as_logical`,
`build_repo_url`, `read_packages_env`, `read_packages_file`, `index_needs_update`)
and the full stack resolution logic (`resolve_builds`).
