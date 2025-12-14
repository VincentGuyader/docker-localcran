# ============================================================
# Local CRAN repository with binary management
#
# Support:
# - Linux PPM (binaries served via src/contrib => type="source")
#   URL: https://packagemanager.posit.co/cran/__linux__/<distro>/<YYYY-MM-DD|latest>
# - Windows PPM (zip binaries => type="win.binary")
#   URL: https://packagemanager.posit.co/cran/<YYYY-MM-DD|latest>
#   contrib: /bin/windows/contrib/<Rmajor.minor>
#
# Env vars (all optional):
# - CRANDORE_OS            : "linux" (default = current OS) or "windows"
# - CRANDORE_DISTRO        : e.g. "noble", "jammy", "bookworm"... (linux only, default = auto-detection from container)
# - CRANDORE_ARCH          : "x86_64" (default = current arch) or "aarch64" (linux only)
# - CRANDORE_SNAPSHOT_DATE : "YYYY-MM-DD" or "latest" (default: Sys.Date())
# - CRANDORE_R_VERSION     : target R version e.g. "4.5.0" (default: R.Version()$major.minor)
# - CRANDORE_FULL_SNAPSHOT : TRUE/FALSE (default FALSE)
# - CRANDORE_PACKAGES      : CSV list "dplyr,data.table" (default empty)
# - CRANDORE_PACKAGES_FILE : path to packages list file (1 per line, takes priority over CRANDORE_PACKAGES)
# - CRANDORE_CLEANUP       : TRUE/FALSE (default FALSE) removes obsolete packages in partial mode
# - CRANDORE_UPDATE_INDEX  : TRUE/FALSE/"force" (default TRUE) generates PACKAGES, PACKAGES.gz, PACKAGES.rds files ("force" bypasses smart detection)
# - CRANDORE_BASE_URL      : default "https://packagemanager.posit.co/cran"
# - CRANDORE_LOCAL_ROOT    : default "./minicran"
# - CRANDORE_VERBOSE       : TRUE/FALSE (default TRUE)
# - CRANDORE_RESUME        : TRUE/FALSE (default TRUE) skips already present packages
#
# Result:
# - local repository created in <CRANDORE_LOCAL_ROOT>/<CRANDORE_OS>/<target>/<Rmajor.minor>
#   target = distro-arch (linux) or "windows-x86_64"
# - PACKAGES / PACKAGES.gz / PACKAGES.rds updated
# ============================================================

linux_distros <- c(
  "centos7", "centos8", "rhel9", "rhel10",
  "opensuse156", "jammy", "noble", "bookworm", "manylinux_2_28"
)

getenv <- function(key, default = "") {
  v <- Sys.getenv(key, unset = "")
  if (identical(v, "")) default else v
}

as_logical <- function(x, default = FALSE) {
  if (length(x) != 1) return(default)
  if (is.logical(x)) return(isTRUE(x))
  x <- tolower(trimws(as.character(x)))
  if (x %in% c("1","true","t","yes","y","oui")) return(TRUE)
  if (x %in% c("0","false","f","no","n","non","")) return(FALSE)
  default
}

validate_snapshot_date <- function(d) {
  if (identical(d, "")) d <- as.character(Sys.Date())
  if (identical(d, "latest")) return(d)
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", d)) {
    stop("CRANDORE_SNAPSHOT_DATE must be 'latest' or in YYYY-MM-DD format")
  }
  date_val <- as.Date(d, format = "%Y-%m-%d")
  if (is.na(date_val)) {
    stop("CRANDORE_SNAPSHOT_DATE is not a valid date")
  }
  if (date_val > Sys.Date()) {
    stop("CRANDORE_SNAPSHOT_DATE cannot be in the future")
  }
  d
}

as_pkg_version <- function(x) {
  if (inherits(x, "package_version")) return(x)
  if (is.numeric(x)) x <- as.character(x)
  if (!is.character(x) || length(x) != 1 || !grepl("^\\d+\\.\\d+(\\.\\d+)?$", x)) {
    stop("CRANDORE_R_VERSION must be a version like '4.5.0' or '4.5'")
  }
  package_version(x)
}

r_major_minor <- function(r_version) {
  v <- as_pkg_version(r_version)
  parts <- strsplit(as.character(v), "\\.")[[1]]
  if (length(parts) < 2) stop("CRANDORE_R_VERSION must have at least major.minor (e.g., '4.5' or '4.5.0')")
  paste(parts[1], parts[2], sep = ".")
}

pick_triplet <- function(os = c("linux","windows"), arch = c("x86_64","aarch64")) {
  os <- match.arg(os)
  arch <- match.arg(arch)
  
  if (os == "linux") {
    if (arch == "x86_64") return(list(platform = "x86_64-pc-linux-gnu", arch = "x86_64", os = "linux-gnu"))
    return(list(platform = "aarch64-pc-linux-gnu", arch = "aarch64", os = "linux-gnu"))
  }
  
  # windows
  list(platform = "x86_64-w64-mingw32", arch = "x86_64", os = "mingw32")
}

build_http_user_agent <- function(target, r_version_target) {
  rv <- as_pkg_version(r_version_target)
  rv_str <- as.character(rv)
  # Uses target triplet (adapted to specified or detected OS/arch)
  sprintf("R/%s R (%s)", rv_str, paste(rv_str, target$platform, target$arch, target$os))
}

build_repo_url <- function(os, distro, snapshot_date, base_url) {
  if (os == "linux") {
    sprintf("%s/__linux__/%s/%s", base_url, distro, snapshot_date)
  } else {
    sprintf("%s/%s", base_url, snapshot_date)
  }
}

contriburl_windows <- function(repo_url, r_version_target) {
  mm <- r_major_minor(r_version_target)
  sprintf("%s/bin/windows/contrib/%s", repo_url, mm)
}

repo_type <- function(os) {
  if (os == "windows") "win.binary" else "source"
}

local_root_default <- function() {
  # if (.Platform$OS.type == "windows") "miniCRAN" else "/miniCRAN"
  file.path(".","minicran")
}

display_info <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message(...)
}

read_packages_env <- function(x) {
  x <- trimws(x)
  if (identical(x, "")) return(character())
  unique(Filter(nzchar, trimws(strsplit(x, ",")[[1]])))
}

read_packages_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(sprintf("CRANDORE_PACKAGES_FILE not found: '%s'", file_path))
  }
  lines <- readLines(file_path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != "" & !grepl("^#", lines)]  # Ignore empty lines and comments
  unique(lines)
}

detect_container_distro <- function() {
  os_release <- "/etc/os-release"
  if (!file.exists(os_release)) return("jammy")  # fallback

  lines <- readLines(os_release, warn = FALSE)

  # Extract ID and VERSION_CODENAME
  id_line <- grep("^ID=", lines, value = TRUE)
  codename_line <- grep("^VERSION_CODENAME=", lines, value = TRUE)

  id <- if (length(id_line) > 0) sub("^ID=", "", id_line[1]) else ""
  codename <- if (length(codename_line) > 0) sub("^VERSION_CODENAME=", "", codename_line[1]) else ""

  # Clean quotes
  id <- gsub('"', '', id)
  codename <- gsub('"', '', codename)

  # Mapping to supported PPM distributions
  if (id == "ubuntu") {
    # For Ubuntu, use codename if available and supported
    if (codename %in% c("jammy", "noble", "focal", "bionic")) return(codename)
    return("jammy")  # fallback
  }

  if (id == "debian") {
    if (codename %in% c("bookworm", "bullseye", "buster")) return(codename)
    return("bookworm")
  }

  if (id == "centos") return("centos8")
  if (id == "rhel") return("rhel9")
  if (id == "opensuse") return("opensuse156")

  return("jammy")  # default fallback
}

list_downloaded_names <- function(local_repo, os) {
  if (!dir.exists(local_repo)) return(character())
  
  if (os == "windows") {
    zips <- list.files(local_repo, pattern = "\\.zip$", recursive = TRUE, full.names = FALSE)
    # package_1.2.3.zip => "package"
    pkgs <- sub("_.*$", "", basename(zips))
    unique(pkgs)
  } else {
    tars <- list.files(local_repo, pattern = "\\.tar\\.gz$", recursive = TRUE, full.names = FALSE)
    pkgs <- sub("_.*$", "", basename(tars))
    unique(pkgs)
  }
}

write_packages_index <- function(local_repo, os, r_version_target, verbose = TRUE) {
  if (os == "windows") {
    mm <- r_major_minor(r_version_target)
    dir_bin <- file.path(local_repo, "bin", "windows", "contrib", mm)
    if (!dir.exists(dir_bin)) dir.create(dir_bin, recursive = TRUE, showWarnings = FALSE)
    tools::write_PACKAGES(dir = dir_bin, type = "win.binary", verbose = verbose)

    # Copy PACKAGES files to src/contrib/ to allow automatic detection by R
    dir_src <- file.path(local_repo, "src", "contrib")
    if (!dir.exists(dir_src)) dir.create(dir_src, recursive = TRUE, showWarnings = FALSE)

    packages_files <- c("PACKAGES", "PACKAGES.gz", "PACKAGES.rds")
    for (pkg_file in packages_files) {
      src_file <- file.path(dir_bin, pkg_file)
      dst_file <- file.path(dir_src, pkg_file)
      if (file.exists(src_file)) {
        file.copy(from = src_file, to = dst_file, overwrite = TRUE)
      }
    }
    invisible(dir_bin)
  } else {
    dir_src <- file.path(local_repo, "src", "contrib")
    if (!dir.exists(dir_src)) dir.create(dir_src, recursive = TRUE, showWarnings = FALSE)
    tools::write_PACKAGES(dir = dir_src, type = "source", verbose = verbose)
    invisible(dir_src)
  }
}

# Robust single download + resume + verbose
download_one <- function(pkg, os, repos, local_repo, r_version_target, verbose = TRUE) {
  if (os == "windows") {
    mm <- r_major_minor(r_version_target)
    destdir <- file.path(local_repo, "bin", "windows", "contrib", mm)
    dir.create(destdir, recursive = TRUE, showWarnings = FALSE)

    cu <- contriburl_windows(repos[[1]], r_version_target)

    utils::download.packages(
      pkgs = pkg,
      destdir = destdir,
      repos = repos,
      type = "win.binary",
      contriburl = cu,
      quiet = !isTRUE(verbose)
    )
  } else {
    # miniCRAN downloads to src/contrib/*.tar.gz when type="source"
    miniCRAN::makeRepo(
      pkg = pkg,
      repos = repos,
      path = local_repo,
      type = "source",
      writePACKAGES = FALSE,
      quiet = !isTRUE(verbose)
    )
  }
  invisible(TRUE)
}

available_packages <- function(os, repos, r_version_target) {
  if (os == "windows") {
    cu <- contriburl_windows(repos[[1]], r_version_target)
    available.packages(repos = repos, type = "win.binary", contriburl = cu)
  } else {
    available.packages(repos = repos, type = "source")
  }
}

index_needs_update <- function(local_repo, os, r_version_target) {
  # For Windows, check both PACKAGES files (bin/ and src/)
  if (os == "windows") {
    mm <- r_major_minor(r_version_target)
    packages_files <- c(
      file.path(local_repo, "bin", "windows", "contrib", mm, "PACKAGES"),
      file.path(local_repo, "src", "contrib", "PACKAGES")
    )
  } else {
    packages_files <- file.path(local_repo, "src", "contrib", "PACKAGES")
  }

  # Check if at least one PACKAGES file exists
  existing_files <- packages_files[file.exists(packages_files)]
  if (length(existing_files) == 0) {
    return(TRUE)  # No PACKAGES exists
  }

  # Use the most recent PACKAGES file for verification
  packages_file <- existing_files[which.max(file.mtime(existing_files))]

  # Count present package files
  pkg_count <- length(list_downloaded_names(local_repo, os))

  # Read the number of entries in PACKAGES
  packages_content <- tryCatch(
    readLines(packages_file, warn = FALSE),
    error = function(e) return(character())
  )

  # Count PACKAGES entries (lines starting with Package:)
  packages_entries <- sum(grepl("^Package:", packages_content))

  # If count doesn't match, update needed
  if (pkg_count != packages_entries) {
    return(TRUE)
  }

  # Check timestamps: PACKAGES must be newer than all package files
  pkg_dir <- if (os == "windows") {
    mm <- r_major_minor(r_version_target)
    file.path(local_repo, "bin", "windows", "contrib", mm)
  } else {
    file.path(local_repo, "src", "contrib")
  }

  if (dir.exists(pkg_dir)) {
    pattern <- if (os == "windows") "\\.zip$" else "\\.tar\\.gz$"
    pkg_files <- list.files(pkg_dir, pattern = pattern, full.names = TRUE)

    if (length(pkg_files) > 0) {
      # Timestamp of the most recent PACKAGES file
      packages_mtime <- file.mtime(packages_file)

      # Check if PACKAGES is older than any package file
      pkg_mtimes <- file.mtime(pkg_files)
      if (any(pkg_mtimes > packages_mtime)) {
        return(TRUE)  # PACKAGES is outdated
      }
    }
  }

  return(FALSE)  # Index is up to date
}

cleanup_obsolete_packages <- function(local_repo, os, r_version_target, pkgs_target, verbose = TRUE) {
  # List all packages present in the repository
  all_downloaded <- list_downloaded_names(local_repo, os = os)

  # Identify obsolete packages (present but not in target)
  obsolete_pkgs <- setdiff(all_downloaded, pkgs_target)

  if (length(obsolete_pkgs) == 0) {
    display_info("No obsolete packages to clean up", verbose = verbose)
    return(0)
  }

  display_info(paste0("Found ", length(obsolete_pkgs), " obsolete packages to clean up: ",
                          paste(obsolete_pkgs, collapse = ", ")), verbose = verbose)

  # Remove obsolete files
  removed_count <- 0
  if (os == "windows") {
    mm <- r_major_minor(r_version_target)
    pkg_dir <- file.path(local_repo, "bin", "windows", "contrib", mm)
    for (pkg in obsolete_pkgs) {
      # Find corresponding .zip files
      zip_pattern <- paste0("^", pkg, "_.*\\.zip$")
      zip_files <- list.files(pkg_dir, pattern = zip_pattern, full.names = TRUE)
      for (zip_file in zip_files) {
        if (file.remove(zip_file)) {
          removed_count <- removed_count + 1
          display_info(paste0("Removed obsolete package: ", basename(zip_file)), verbose = verbose)
        }
      }
    }
  } else {
    pkg_dir <- file.path(local_repo, "src", "contrib")
    for (pkg in obsolete_pkgs) {
      # Find corresponding .tar.gz files
      tar_pattern <- paste0("^", pkg, "_.*\\.tar\\.gz$")
      tar_files <- list.files(pkg_dir, pattern = tar_pattern, full.names = TRUE)
      for (tar_file in tar_files) {
        if (file.remove(tar_file)) {
          removed_count <- removed_count + 1
          display_info(paste0("Removed obsolete package: ", basename(tar_file)), verbose = verbose)
        }
      }
    }
  }

  display_info(paste0("Cleanup completed: ", removed_count, " files removed"), verbose = verbose)
  invisible(removed_count)
}

crandore_ <- function() {
  # 1) Read config
  verbose <- as_logical(getenv("CRANDORE_VERBOSE", "true"), default = TRUE)

  # Detect current OS/arch as default values
  current_os_type <- tolower(.Platform$OS.type)
  current_os <- if (current_os_type == "unix") "linux" else current_os_type
  current_arch <- tolower(R.Version()$arch)

  os <- tolower(getenv("CRANDORE_OS", current_os))
  if (!os %in% c("linux","windows")) stop("CRANDORE_OS must be 'linux' or 'windows'")

  distro <- tolower(getenv("CRANDORE_DISTRO", detect_container_distro()))
  arch <- tolower(getenv("CRANDORE_ARCH", current_arch))
  if (!arch %in% c("x86_64","aarch64")) {
    stop("CRANDORE_ARCH must be 'x86_64' (windows or linux) or 'aarch64' (linux only)")
  }
  if (os == "linux") {
    distro <- tolower(getenv("CRANDORE_DISTRO", detect_container_distro()))
    if (!distro %in% linux_distros) {
      stop(sprintf("CRANDORE_DISTRO unknown: '%s'. Values: %s", distro, paste(linux_distros, collapse = ", ")))
    }
  } else {
    distro <- NULL
  }
  arch <- tolower(getenv("CRANDORE_ARCH", current_arch))
  if (!arch %in% c("x86_64","aarch64")) stop("CRANDORE_ARCH must be 'x86_64' or 'aarch64' (linux only)")
  snapshot_date <- validate_snapshot_date(getenv("CRANDORE_SNAPSHOT_DATE", ""))
  base_url <- getenv("CRANDORE_BASE_URL", "https://packagemanager.posit.co/cran")

  r_version_target <- getenv("CRANDORE_R_VERSION", "")
  if (identical(r_version_target, "")) {
    r_version_target <- paste0(R.Version()$major, ".", R.Version()$minor)
  }
  r_version_target <- as_pkg_version(r_version_target)

  full_snapshot <- as_logical(getenv("CRANDORE_FULL_SNAPSHOT", "false"), default = FALSE)
  resume <- as_logical(getenv("CRANDORE_RESUME", "true"), default = TRUE)
  cleanup <- as_logical(getenv("CRANDORE_CLEANUP", "false"), default = FALSE)
  update_index_raw <- tolower(getenv("CRANDORE_UPDATE_INDEX", "true"))

  # Read package list (file takes priority over env var)
  packages_file <- getenv("CRANDORE_PACKAGES_FILE", "")
  packages_seed <- if (identical(packages_file, "")) {
    # Use CRANDORE_PACKAGES
    packages_csv <- getenv("CRANDORE_PACKAGES", "")
    read_packages_env(packages_csv)
  } else {
    # Use file
    read_packages_file(packages_file)
  }

  local_root <- getenv("CRANDORE_LOCAL_ROOT", local_root_default())

  # 2) Configure options(repos, HTTPUserAgent)
  repo_url <- build_repo_url(os = os, distro = distro, snapshot_date = snapshot_date, base_url = base_url)
  repos <- c(CRAN = repo_url)

  triplet <- pick_triplet(os = os, arch = arch)
  ua <- build_http_user_agent(triplet, r_version_target = r_version_target)

  options(repos = repos, HTTPUserAgent = ua)

  # 3) Local path distinct per OS/target/Rmajor.minor
  r_mm <- r_major_minor(r_version_target)
  target_id <- if (os == "linux") paste0(distro, "-", arch) else paste0("windows-", arch)
  local_repo <- file.path(local_root, os, target_id, paste0("R-", r_mm))
  dir.create(local_repo, recursive = TRUE, showWarnings = FALSE)

  display_info(paste0("OS = ", os), verbose = verbose)
  if (os == "linux") {
    display_info(paste0("Distro = ", distro), verbose = verbose)
    display_info(paste0("Arch = ", arch), verbose = verbose)
  }
  display_info(paste0("Target R version = ", as.character(r_version_target), " (", r_mm, ")"), verbose = verbose)
  display_info(paste0("Snapshot = ", snapshot_date), verbose = verbose)
  display_info(paste0("PPM repo = ", repo_url), verbose = verbose)
  if (os == "windows") {
    display_info(paste0("Windows contriburl = ", contriburl_windows(repo_url, r_version_target)), verbose = verbose)
  }
  display_info(paste0("Local repo = ", normalizePath(local_repo, winslash = "/", mustWork = FALSE)), verbose = verbose)

  # 4) Load miniCRAN if necessary
  suppressPackageStartupMessages(library(miniCRAN))

  # 5) List available packages
  ap <- available_packages(os = os, repos = repos, r_version_target = r_version_target)
  available_pkgs <- rownames(ap)
  display_info(paste0("Number of available packages: ", length(available_pkgs)), verbose = verbose)

  # 6) Determine target (full or dependencies)
  if (isTRUE(full_snapshot)) {
    display_info("Mode: full snapshot", verbose = verbose)
    pkgs_target <- available_pkgs
  } else {
    display_info("Mode: partial snapshot", verbose = verbose)
    if (length(packages_seed) == 0) {
      pkgs_target <- character()
    } else {
      # pkgDep() queries available.packages() already configured via repos/options
      pkgs_target <- unique(miniCRAN::pkgDep(packages_seed))
    }
  }

  display_info(paste0("Number of requested packages: ", length(pkgs_target)), verbose = verbose)

  # 7) Resume: remove already downloaded
  deja_dl <- list_downloaded_names(local_repo, os = os)
  deja_dl_current <- intersect(deja_dl, pkgs_target)
  pkgs_todo <- if (isTRUE(resume)) setdiff(pkgs_target, deja_dl) else pkgs_target

  display_info(paste0("Already downloaded (in target): ", length(deja_dl_current)), verbose = verbose)
  display_info(paste0("Already downloaded (total): ", length(deja_dl)), verbose = verbose)
  display_info(paste0("To download: ", length(pkgs_todo)), verbose = verbose)

  # 8) Download with progress
  total_target <- max(1L, length(pkgs_target))
  done_already <- length(deja_dl_current)
  failed_downloads <- character()

  for (i in seq_along(pkgs_todo)) {
    pct <- round((done_already + i - 1) / total_target * 100, 2)
    display_info(paste0(pkgs_todo[i], " - ", pct, " %"), verbose = verbose)
    tryCatch(
      {
        download_one(
          pkg = pkgs_todo[i],
          os = os,
          repos = repos,
          local_repo = local_repo,
          r_version_target = r_version_target,
          verbose = verbose
        )
      },
      error = function(e) {
        display_info(paste0("ERROR downloading ", pkgs_todo[i], ": ", conditionMessage(e)), verbose = verbose)
        failed_downloads <<- c(failed_downloads, pkgs_todo[i])
      }
    )
  }

  display_info("DL done", verbose = verbose)
  if (length(failed_downloads) > 0) {
    max_display <- 10
    failed_list <- if (length(failed_downloads) > max_display) {
      paste0(paste(head(failed_downloads, max_display), collapse = ", "), ", ... (", length(failed_downloads) - max_display, " more)")
    } else {
      paste(failed_downloads, collapse = ", ")
    }
    display_info(paste0("WARNING: ", length(failed_downloads), " package(s) failed to download: ", failed_list), verbose = verbose)
  }

  # 9) Cleanup (only in partial mode) - BEFORE PACKAGES update
  if (isTRUE(cleanup) && !isTRUE(full_snapshot)) {
    display_info("Starting cleanup of obsolete packages", verbose = verbose)
    cleanup_result <- cleanup_obsolete_packages(local_repo, os, r_version_target, pkgs_target, verbose)
    display_info(paste0("Cleanup: ", cleanup_result, " obsolete files removed"), verbose = verbose)
  }

  # 10) Update PACKAGES (ONLY ONCE with final list)
  if (update_index_raw == "force") {
    display_info("force update PACKAGES, PACKAGES.gz and PACKAGES.rds", verbose = verbose)
    idx_dir <- write_packages_index(local_repo, os = os, r_version_target = r_version_target, verbose = verbose)
    display_info("done", verbose = verbose)
  } else if (as_logical(update_index_raw, default = TRUE)) {
    if (index_needs_update(local_repo, os, r_version_target)) {
      display_info("update PACKAGES, PACKAGES.gz and PACKAGES.rds", verbose = verbose)
      idx_dir <- write_packages_index(local_repo, os = os, r_version_target = r_version_target, verbose = verbose)
      display_info("done", verbose = verbose)
    } else {
      display_info("PACKAGES index is up to date, skipping update", verbose = verbose)
    }
  } else {
    display_info("Skipping PACKAGES index update (CRANDORE_UPDATE_INDEX=false)", verbose = verbose)
  }

  # 11) Control
  deja_dl2 <- list_downloaded_names(local_repo, os = os)
  missing <- setdiff(pkgs_target, deja_dl2)

  if (length(missing) > 0) {
    display_info(paste0("missing packages: ", paste(missing, collapse = ", ")), verbose = verbose)
  } else {
    display_info("All packages downloaded", verbose = verbose)
  }

  invisible(list(
    os = os,
    distro = if (os == "linux") distro else NA_character_,
    arch = if (os == "linux") arch else NA_character_,
    snapshot_date = snapshot_date,
    r_version_target = as.character(r_version_target),
    r_major_minor = r_mm,
    repo_url = repo_url,
    repo_type = repo_type(os),
    local_repo = normalizePath(local_repo, winslash = "/", mustWork = FALSE),
    packages_target_n = length(pkgs_target),
    packages_todo_n = length(pkgs_todo),
    missing_n = length(missing)
  ))
}

#' Create Local CRAN Repository Snapshot
#'
#' Downloads packages and their dependencies from Posit Public Package Manager (PPM)
#' to create a local CRAN repository snapshot. Supports both full snapshots (all available packages)
#' and partial snapshots (specified packages + dependencies).
#'
#' @param os Character string. Target operating system: "linux" or "windows".
#'   Defaults to current OS or CRANDORE_OS environment variable.
#' @param distro Character string. Linux distribution for binary packages.
#'   Supported: "jammy", "noble", "bookworm", etc. Auto-detected if not specified.
#' @param arch Character string. Architecture: "x86_64" or "aarch64" (Linux only).
#'   Defaults to current architecture.
#' @param snapshot_date Character string. CRAN snapshot date in "YYYY-MM-DD" format
#'   or "latest". Defaults to current date.
#' @param r_version Character string. Target R version (e.g., "4.5.0").
#'   Defaults to current R version.
#' @param full_snapshot Logical. If TRUE, downloads all available packages.
#'   If FALSE, downloads only specified packages and dependencies.
#' @param packages Character string. Comma-separated list of package names to download.
#' @param packages_file Character string. Path to file containing package names
#'   (one per line). Takes priority over packages parameter.
#' @param cleanup Logical. If TRUE, removes obsolete packages in partial mode.
#' @param update_index Character string. Controls PACKAGES index generation:
#'   "true" (smart update), "false" (skip), "force" (always regenerate).
#' @param base_url Character string. Base URL for package repository.
#'   Defaults to Posit Public Package Manager.
#' @param local_root Character string. Root directory for local repository.
#'   Defaults to "./minicran".
#' @param verbose Logical. If TRUE, displays progress messages.
#' @param resume Logical. If TRUE, skips already downloaded packages.
#'
#' @return Invisible list containing repository information:
#'   \item{os}{Target operating system}
#'   \item{distro}{Linux distribution (NA for Windows)}
#'   \item{arch}{Architecture (NA for Windows)}
#'   \item{snapshot_date}{Snapshot date used}
#'   \item{r_version_target}{Target R version}
#'   \item{r_major_minor}{Major.minor version}
#'   \item{repo_url}{Repository URL}
#'   \item{repo_type}{Repository type ("source" or "win.binary")}
#'   \item{local_repo}{Local repository path}
#'   \item{packages_target_n}{Number of target packages}
#'   \item{packages_todo_n}{Number of packages to download}
#'   \item{missing_n}{Number of missing packages}
#'
#' @importFrom withr with_envvar
#' @importFrom miniCRAN makeRepo
#' @importFrom miniCRAN pkgDep
#' @importFrom utils download.packages
#' @importFrom utils available.packages
#' @importFrom tools write_PACKAGES
#'
#' @examples
#' \dontrun{
#' # Download specific packages for current platform
#' crandore(packages = "tidyverse,dplyr,ggplot2")
#'
#' # Create Windows repository
#' crandore(os = "windows", packages = "shiny")
#'
#' # Full snapshot for Ubuntu Jammy
#' crandore(os = "linux", distro = "jammy", full_snapshot = TRUE)
#'
#' # Clean up obsolete packages
#' crandore(packages = "tidyverse", cleanup = TRUE)
#' }
#'
#' @export
crandore <- function(
  os = getenv("CRANDORE_OS", if (tolower(.Platform$OS.type) == "unix") "linux" else tolower(.Platform$OS.type)),
  distro = getenv("CRANDORE_DISTRO", detect_container_distro()),
  arch = getenv("CRANDORE_ARCH", tolower(R.Version()$arch)),
  snapshot_date = getenv("CRANDORE_SNAPSHOT_DATE", ""),
  r_version = getenv("CRANDORE_R_VERSION", ""),
  full_snapshot = as_logical(getenv("CRANDORE_FULL_SNAPSHOT", "false")),
  packages = getenv("CRANDORE_PACKAGES", ""),
  packages_file = getenv("CRANDORE_PACKAGES_FILE", ""),
  cleanup = as_logical(getenv("CRANDORE_CLEANUP", "false")),
  update_index = getenv("CRANDORE_UPDATE_INDEX", "true"),
  base_url = getenv("CRANDORE_BASE_URL", "https://packagemanager.posit.co/cran"),
  local_root = getenv("CRANDORE_LOCAL_ROOT", local_root_default()),
  verbose = as_logical(getenv("CRANDORE_VERBOSE", "true")),
  resume = as_logical(getenv("CRANDORE_RESUME", "true"))
) {
  # Use withr::with_envvar for clean environment variable management

  result <- withr::with_envvar(
    c(
      CRANDORE_OS = os,
      CRANDORE_DISTRO = distro,
      CRANDORE_ARCH = arch,
      CRANDORE_SNAPSHOT_DATE = snapshot_date,
      CRANDORE_R_VERSION = r_version,
      CRANDORE_FULL_SNAPSHOT = as.character(full_snapshot),
      CRANDORE_PACKAGES = packages,
      CRANDORE_PACKAGES_FILE = packages_file,
      CRANDORE_CLEANUP = as.character(cleanup),
      CRANDORE_UPDATE_INDEX = update_index,
      CRANDORE_BASE_URL = base_url,
      CRANDORE_LOCAL_ROOT = local_root,
      CRANDORE_VERBOSE = as.character(verbose),
      CRANDORE_RESUME = as.character(resume)
    ),
    crandore_()
  )

  return(invisible(result))
}
