# docker-localcran

Use {miniCRAN} to locally create a CRAN repository snapshot using Docker.

This Docker container allows you to create a local CRAN snapshot using miniCRAN.
The container is smart: it only downloads the necessary packages and their dependencies.
You can interrupt and resume the process without losing time.

## Prerequisites

- Docker installed on your system (only needed to create the repos)
- A local directory to store the snapshot (e.g., `./miniCRAN`)

## Programmatic Usage

In addition to Docker usage with environment variables, you can use the R functions directly:

```r
# Load the script
source("script/repos_snapshot.R")

# Simple programmatic usage
result <- CRANDORE_sync2(packages = "tidyverse", cleanup = TRUE)

# Full configuration
result <- CRANDORE_sync2(
  os = "linux",
  distro = "noble",
  packages = "dplyr,ggplot2",
  cleanup = TRUE,
  update_index = "force",
  verbose = TRUE
)

# Uses current environment variables as defaults
result <- CRANDORE_sync2()
```

## Building the Image

Build the Docker image using one of the available Dockerfiles:

```bash
docker build -t crandore .
```

## Usage

The container uses the Posit Public Package Manager as the default repository.See: <https://packagemanager.posit.co/cran/>.

### Environment Variables

All variables are optional and have sensible defaults:

| Variable | Description | Default Value |
|----------|-------------|---------------|
| `CRANDORE_OS` | Target OS ("linux" or "windows") | Current OS |
| `CRANDORE_DISTRO` | Linux distribution (e.g., "jammy", "noble") | Auto-detected from container |
| `CRANDORE_ARCH` | Architecture ("x86_64" or "aarch64") | Current arch |
| `CRANDORE_SNAPSHOT_DATE` | CRAN snapshot date (YYYY-MM-DD or "latest") | Current date |
| `CRANDORE_R_VERSION` | Target R version (e.g., "4.5.0") | Current R version |
| `CRANDORE_FULL_SNAPSHOT` | Download all available packages | `false` |
| `CRANDORE_PACKAGES` | List of packages to download (comma-separated) | Empty |
| `CRANDORE_PACKAGES_FILE` | Path to packages list file (one per line, takes priority over CRANDORE_PACKAGES) | Empty |
| `CRANDORE_CLEANUP` | Remove obsolete packages in partial mode | `false` |
| `CRANDORE_UPDATE_INDEX` | Generate PACKAGES, PACKAGES.gz and PACKAGES.rds files (true=smart, false=skip, force=always) | `true` |
| `CRANDORE_VERBOSE` | Enable verbose output | `true` |
| `CRANDORE_RESUME` | Resume interrupted downloads | `true` |

### Basic Usage

By default, the script detects your current OS/architecture and downloads packages accordingly:

```bash
# Download specific packages for current platform
docker run -v ./miniCRAN:/miniCRAN -e CRANDORE_PACKAGES="tidyverse" crandore

# Full snapshot for current platform
docker run -v ./miniCRAN:/miniCRAN -e CRANDORE_FULL_SNAPSHOT=true crandore
```

### Cross-Platform Builds

To build repositories for different platforms:

```bash
# Linux binaries for Ubuntu Jammy
docker run -v ./miniCRAN:/miniCRAN \
  -e CRANDORE_OS=linux \
  -e CRANDORE_DISTRO=jammy \
  -e CRANDORE_PACKAGES="tidyverse" \
  crandore

# Windows binaries
docker run -v /c/wootwoot:/miniCRAN   -e CRANDORE_OS=windows   -e CRANDORE_PACKAGES="tidyverse" crandore
```

### Using a Packages File

For long lists of packages, use a file (one package per line):

Create a `packages.txt` file:
```
dplyr
data.table
ggplot2
tidyr
readr
purrr
stringr
forcats
# This is a comment
lubridate
```

Then run:
```bash
docker run -v ./miniCRAN:/miniCRAN \
  -v ./packages.txt:/packages.txt \
  -e CRANDORE_PACKAGES_FILE=/packages.txt \
  crandore
```

### Advanced Examples

- Custom snapshot date and R version:
  ```bash
  docker run -v ./miniCRAN:/miniCRAN \
    -e CRANDORE_SNAPSHOT_DATE="2024-01-01" \
    -e CRANDORE_R_VERSION="4.3.0" \
    -e CRANDORE_PACKAGES="tidyverse" \
    crandore
  ```

- Full Linux binary repository for multiple distributions:
  ```bash
  docker run -v ./miniCRAN:/miniCRAN \
    -e CRANDORE_OS=linux \
    -e CRANDORE_DISTRO=noble \
    -e CRANDORE_FULL_SNAPSHOT=true \
    crandore
  ```

- Clean up obsolete packages after updating package list:
  ```bash
  docker run -v ./miniCRAN:/miniCRAN \
    -e CRANDORE_PACKAGES="tidyverse" \
    -e CRANDORE_CLEANUP=true \
    crandore
  ```

- Download packages only (skip PACKAGES index generation):
  ```bash
  docker run -v ./miniCRAN:/miniCRAN \
    -e CRANDORE_PACKAGES="tidyverse" \
    -e CRANDORE_UPDATE_INDEX=false \
    crandore
  ```

- Force PACKAGES index regeneration (even if up to date):
  ```bash
  docker run -v ./miniCRAN:/miniCRAN \
    -e CRANDORE_PACKAGES="tidyverse" \
    -e CRANDORE_UPDATE_INDEX=force \
    crandore
  ```

## Local Repository Structure

After execution, your `miniCRAN` directory will contain:
- `linux/` or `windows/` (based on target OS)
  - `{distro}-{arch}/` or `windows-x86_64/` (target platform)
    - `R-{major.minor}/` (R version)
      - `src/contrib/` (Linux: binary packages as .tar.gz)
      - `bin/windows/contrib/{major.minor}/` (Windows: .zip binaries)
      - `PACKAGES`, `PACKAGES.gz`, `PACKAGES.rds` (repository metadata)
