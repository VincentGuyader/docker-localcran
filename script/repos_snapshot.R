# ============================================================
# Mini-CRAN sync via Posit Package Manager (PPM)
# approche "env vars + reprise + verbose + progression"
#
# Support:
# - Linux PPM (binaires servis via src/contrib => type="source")
#   URL: https://packagemanager.posit.co/cran/__linux__/<distro>/<YYYY-MM-DD|latest>
# - Windows PPM (binaires zip => type="win.binary")
#   URL: https://packagemanager.posit.co/cran/<YYYY-MM-DD|latest>
#   contrib: /bin/windows/contrib/<Rmajor.minor>
#
# Env vars (toutes optionnelles):
# - CRANDORE_OS            : "linux" (defaut = OS courant) ou "windows"
# - CRANDORE_DISTRO        : ex "noble", "jammy", "bookworm"... (linux uniquement, defaut = detection auto du conteneur)
# - CRANDORE_ARCH          : "x86_64" (defaut = arch courante) ou "aarch64" (linux uniquement)
# - CRANDORE_SNAPSHOT_DATE : "YYYY-MM-DD" ou "latest" (defaut: Sys.Date())
# - CRANDORE_R_VERSION     : version de R cible ex "4.5.0" (defaut: R.Version()$major.minor)
# - CRANDORE_FULL_SNAPSHOT : TRUE/FALSE (defaut FALSE)
# - CRANDORE_PACKAGES      : liste CSV "dplyr,data.table" (defaut vide)
# - CRANDORE_PACKAGES_FILE : chemin vers fichier liste packages (1 par ligne, prioritaire sur CRANDORE_PACKAGES)
# - CRANDORE_CLEANUP       : TRUE/FALSE (defaut FALSE) supprime les packages obsolètes en mode partiel
# - CRANDORE_UPDATE_INDEX  : TRUE/FALSE/"force" (defaut TRUE) genere les fichiers PACKAGES, PACKAGES.gz, PACKAGES.rds ("force" bypass la detection intelligente)
# - CRANDORE_BASE_URL      : defaut "https://packagemanager.posit.co/cran"
# - CRANDORE_LOCAL_ROOT    : defaut "/miniCRAN" (ou "miniCRAN" si Windows)
# - CRANDORE_VERBOSE       : TRUE/FALSE (defaut TRUE)
# - CRANDORE_RESUME        : TRUE/FALSE (defaut TRUE) saute les paquets deja presents
#
# Resultat:
# - depot local cree dans <CRANDORE_LOCAL_ROOT>/<CRANDORE_OS>/<target>/<Rmajor.minor>
#   target = distro-arch (linux) ou "windows-x86_64"
# - PACKAGES / PACKAGES.gz / PACKAGES.rds mis a jour
# ============================================================

CRANDORE_linux_distros <- c(
  "centos7", "centos8", "rhel9", "rhel10",
  "opensuse156", "jammy", "noble", "bookworm", "manylinux_2_28"
)

CRANDORE_getenv <- function(key, default = "") {
  v <- Sys.getenv(key, unset = "")
  if (identical(v, "")) default else v
}

CRANDORE_as_logical <- function(x, default = FALSE) {
  if (length(x) != 1) return(default)
  if (is.logical(x)) return(isTRUE(x))
  x <- tolower(trimws(as.character(x)))
  if (x %in% c("1","true","t","yes","y","oui")) return(TRUE)
  if (x %in% c("0","false","f","no","n","non","")) return(FALSE)
  default
}

CRANDORE_validate_snapshot_date <- function(d) {
  if (identical(d, "")) d <- as.character(Sys.Date())
  if (!(identical(d, "latest") || grepl("^\\d{4}-\\d{2}-\\d{2}$", d))) {
    stop("CRANDORE_SNAPSHOT_DATE doit etre 'latest' ou au format YYYY-MM-DD")
  }
  d
}

CRANDORE_as_pkg_version <- function(x) {
  if (inherits(x, "package_version")) return(x)
  if (is.numeric(x)) x <- as.character(x)
  if (!is.character(x) || length(x) != 1 || !grepl("^\\d+\\.\\d+(\\.\\d+)?$", x)) {
    stop("CRANDORE_R_VERSION doit etre une version du type '4.5.0' ou '4.5'")
  }
  package_version(x)
}

CRANDORE_r_major_minor <- function(r_version) {
  v <- CRANDORE_as_pkg_version(r_version)
  parts <- strsplit(as.character(v), "\\.")[[1]]
  if (length(parts) < 2) stop("CRANDORE_R_VERSION doit avoir au moins major.minor (ex: '4.5' ou '4.5.0')")
  paste(parts[1], parts[2], sep = ".")
}

CRANDORE_pick_triplet <- function(os = c("linux","windows"), arch = c("x86_64","aarch64")) {
  os <- match.arg(os)
  arch <- match.arg(arch)
  
  if (os == "linux") {
    if (arch == "x86_64") return(list(platform = "x86_64-pc-linux-gnu", arch = "x86_64", os = "linux-gnu"))
    return(list(platform = "aarch64-pc-linux-gnu", arch = "aarch64", os = "linux-gnu"))
  }
  
  # windows
  list(platform = "x86_64-w64-mingw32", arch = "x86_64", os = "mingw32")
}

CRANDORE_build_http_user_agent <- function(target, r_version_target) {
  rv <- CRANDORE_as_pkg_version(r_version_target)
  rv_str <- as.character(rv)
  # Utilise le triplet cible (adapté à l'OS/arch spécifiés ou détectés)
  sprintf("R/%s R (%s)", rv_str, paste(rv_str, target$platform, target$arch, target$os))
}

CRANDORE_build_repo_url <- function(os, distro, snapshot_date, base_url) {
  if (os == "linux") {
    sprintf("%s/__linux__/%s/%s", base_url, distro, snapshot_date)
  } else {
    sprintf("%s/%s", base_url, snapshot_date)
  }
}

CRANDORE_contriburl_windows <- function(repo_url, r_version_target) {
  mm <- CRANDORE_r_major_minor(r_version_target)
  sprintf("%s/bin/windows/contrib/%s", repo_url, mm)
}

CRANDORE_repo_type <- function(os) {
  if (os == "windows") "win.binary" else "source"
}

CRANDORE_local_root_default <- function() {
  if (.Platform$OS.type == "windows") "miniCRAN" else "/miniCRAN"
}

CRANDORE_message <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message(...)
}

CRANDORE_read_packages_env <- function(x) {
  x <- trimws(x)
  if (identical(x, "")) return(character())
  unique(Filter(nzchar, trimws(strsplit(x, ",")[[1]])))
}

CRANDORE_read_packages_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(sprintf("CRANDORE_PACKAGES_FILE not found: '%s'", file_path))
  }
  lines <- readLines(file_path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != "" & !grepl("^#", lines)]  # Ignore empty lines and comments
  unique(lines)
}

CRANDORE_detect_container_distro <- function() {
  os_release <- "/etc/os-release"
  if (!file.exists(os_release)) return("jammy")  # fallback

  lines <- readLines(os_release, warn = FALSE)

  # Extraire ID et VERSION_CODENAME
  id_line <- grep("^ID=", lines, value = TRUE)
  codename_line <- grep("^VERSION_CODENAME=", lines, value = TRUE)

  id <- if (length(id_line) > 0) sub("^ID=", "", id_line[1]) else ""
  codename <- if (length(codename_line) > 0) sub("^VERSION_CODENAME=", "", codename_line[1]) else ""

  # Nettoyer les guillemets
  id <- gsub('"', '', id)
  codename <- gsub('"', '', codename)

  # Mapping vers distributions PPM supportées
  if (id == "ubuntu") {
    # Pour Ubuntu, utiliser le codename si disponible et supporté
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

  return("jammy")  # fallback par défaut
}

CRANDORE_list_downloaded_names <- function(local_repo, os) {
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

CRANDORE_write_packages_index <- function(local_repo, os, r_version_target, verbose = TRUE) {
  if (os == "windows") {
    mm <- CRANDORE_r_major_minor(r_version_target)
    dir_bin <- file.path(local_repo, "bin", "windows", "contrib", mm)
    if (!dir.exists(dir_bin)) dir.create(dir_bin, recursive = TRUE, showWarnings = FALSE)
    tools::write_PACKAGES(dir = dir_bin, type = "win.binary", verbose = verbose)
    invisible(dir_bin)
  } else {
    dir_src <- file.path(local_repo, "src", "contrib")
    if (!dir.exists(dir_src)) dir.create(dir_src, recursive = TRUE, showWarnings = FALSE)
    tools::write_PACKAGES(dir = dir_src, type = "source", verbose = verbose)
    invisible(dir_src)
  }
}

# Telechargement unitaire "robuste" + resume + verbose
CRANDORE_download_one <- function(pkg, os, repos, local_repo, r_version_target, verbose = TRUE) {
  if (os == "windows") {
    mm <- CRANDORE_r_major_minor(r_version_target)
    destdir <- file.path(local_repo, "bin", "windows", "contrib", mm)
    dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
    
    cu <- CRANDORE_contriburl_windows(repos[[1]], r_version_target)
    
    utils::download.packages(
      pkgs = pkg,
      destdir = destdir,
      repos = repos,
      type = "win.binary",
      contriburl = cu,
      quiet = !isTRUE(verbose)
    )
  } else {
    # miniCRAN telecharge en src/contrib/*.tar.gz quand type="source"
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

CRANDORE_available_packages <- function(os, repos, r_version_target) {
  if (os == "windows") {
    cu <- CRANDORE_contriburl_windows(repos[[1]], r_version_target)
    available.packages(repos = repos, type = "win.binary", contriburl = cu)
  } else {
    available.packages(repos = repos, type = "source")
  }
}

CRANDORE_index_needs_update <- function(local_repo, os, r_version_target) {
  # Vérifier si PACKAGES existe
  packages_file <- if (os == "windows") {
    mm <- CRANDORE_r_major_minor(r_version_target)
    file.path(local_repo, "bin", "windows", "contrib", mm, "PACKAGES")
  } else {
    file.path(local_repo, "src", "contrib", "PACKAGES")
  }

  if (!file.exists(packages_file)) {
    return(TRUE)  # PACKAGES n'existe pas
  }

  # Compter les fichiers de packages présents
  pkg_count <- length(CRANDORE_list_downloaded_names(local_repo, os))

  # Lire le nombre d'entrées dans PACKAGES
  packages_content <- tryCatch(
    readLines(packages_file, warn = FALSE),
    error = function(e) return(character())
  )

  # Compter les entrées PACKAGES (lignes non vides commençant par Package:)
  packages_entries <- sum(grepl("^Package:", packages_content))

  # Si le nombre ne correspond pas, update nécessaire
  if (pkg_count != packages_entries) {
    return(TRUE)
  }

  # Vérifier timestamps : PACKAGES doit être plus récent que tous les fichiers packages
  pkg_dir <- if (os == "windows") {
    mm <- CRANDORE_r_major_minor(r_version_target)
    file.path(local_repo, "bin", "windows", "contrib", mm)
  } else {
    file.path(local_repo, "src", "contrib")
  }

  if (dir.exists(pkg_dir)) {
    pattern <- if (os == "windows") "\\.zip$" else "\\.tar\\.gz$"
    pkg_files <- list.files(pkg_dir, pattern = pattern, full.names = TRUE)

    if (length(pkg_files) > 0) {
      # Timestamp du fichier PACKAGES le plus ancien
      packages_mtime <- file.mtime(packages_file)

      # Vérifier si PACKAGES est plus ancien que n'importe quel fichier package
      pkg_mtimes <- file.mtime(pkg_files)
      if (any(pkg_mtimes > packages_mtime)) {
        return(TRUE)  # PACKAGES est obsolète
      }
    }
  }

  return(FALSE)  # Index à jour
}

CRANDORE_cleanup_obsolete_packages <- function(local_repo, os, r_version_target, pkgs_target, verbose = TRUE) {
  # Lister tous les packages présents dans le dépôt
  all_downloaded <- CRANDORE_list_downloaded_names(local_repo, os = os)

  # Identifier les packages obsolètes (présents mais pas dans la cible)
  obsolete_pkgs <- setdiff(all_downloaded, pkgs_target)

  if (length(obsolete_pkgs) == 0) {
    CRANDORE_message("No obsolete packages to clean up", verbose = verbose)
    return(0)
  }

  CRANDORE_message(paste0("Found ", length(obsolete_pkgs), " obsolete packages to clean up: ",
                          paste(obsolete_pkgs, collapse = ", ")), verbose = verbose)

  # Supprimer les fichiers obsolètes
  removed_count <- 0
  if (os == "windows") {
    mm <- CRANDORE_r_major_minor(r_version_target)
    pkg_dir <- file.path(local_repo, "bin", "windows", "contrib", mm)
    for (pkg in obsolete_pkgs) {
      # Chercher les fichiers .zip correspondants
      zip_pattern <- paste0("^", pkg, "_.*\\.zip$")
      zip_files <- list.files(pkg_dir, pattern = zip_pattern, full.names = TRUE)
      for (zip_file in zip_files) {
        if (file.remove(zip_file)) {
          removed_count <- removed_count + 1
          CRANDORE_message(paste0("Removed obsolete package: ", basename(zip_file)), verbose = verbose)
        }
      }
    }
  } else {
    pkg_dir <- file.path(local_repo, "src", "contrib")
    for (pkg in obsolete_pkgs) {
      # Chercher les fichiers .tar.gz correspondants
      tar_pattern <- paste0("^", pkg, "_.*\\.tar\\.gz$")
      tar_files <- list.files(pkg_dir, pattern = tar_pattern, full.names = TRUE)
      for (tar_file in tar_files) {
        if (file.remove(tar_file)) {
          removed_count <- removed_count + 1
          CRANDORE_message(paste0("Removed obsolete package: ", basename(tar_file)), verbose = verbose)
        }
      }
    }
  }

  CRANDORE_message(paste0("Cleanup completed: ", removed_count, " files removed"), verbose = verbose)
  invisible(removed_count)
}

CRANDORE_sync <- function() {
  # 1) Lire la config
  verbose <- CRANDORE_as_logical(CRANDORE_getenv("CRANDORE_VERBOSE", "true"), default = TRUE)

  # Detecter OS/arch courants comme valeurs par defaut
  current_os_type <- tolower(.Platform$OS.type)
  current_os <- if (current_os_type == "unix") "linux" else current_os_type
  current_arch <- tolower(R.Version()$arch)

  os <- tolower(CRANDORE_getenv("CRANDORE_OS", current_os))
  if (!os %in% c("linux","windows")) stop("CRANDORE_OS doit etre 'linux' ou 'windows'")

  distro <- tolower(CRANDORE_getenv("CRANDORE_DISTRO", CRANDORE_detect_container_distro()))
  arch <- tolower(CRANDORE_getenv("CRANDORE_ARCH", current_arch))
  if (!arch %in% c("x86_64","aarch64")) stop("CRANDORE_ARCH doit etre 'x86_64' ou 'aarch64' (linux)")
  if (os == "linux" && !distro %in% CRANDORE_linux_distros) {
    stop(sprintf("CRANDORE_DISTRO inconnue: '%s'. Valeurs: %s", distro, paste(CRANDORE_linux_distros, collapse = ", ")))
  }
  
  snapshot_date <- CRANDORE_validate_snapshot_date(CRANDORE_getenv("CRANDORE_SNAPSHOT_DATE", ""))
  base_url <- CRANDORE_getenv("CRANDORE_BASE_URL", "https://packagemanager.posit.co/cran")
  
  r_version_target <- CRANDORE_getenv("CRANDORE_R_VERSION", "")
  if (identical(r_version_target, "")) {
    r_version_target <- paste0(R.Version()$major, ".", R.Version()$minor)
  }
  r_version_target <- CRANDORE_as_pkg_version(r_version_target)
  
  full_snapshot <- CRANDORE_as_logical(CRANDORE_getenv("CRANDORE_FULL_SNAPSHOT", "false"), default = FALSE)
  resume <- CRANDORE_as_logical(CRANDORE_getenv("CRANDORE_RESUME", "true"), default = TRUE)
  cleanup <- CRANDORE_as_logical(CRANDORE_getenv("CRANDORE_CLEANUP", "false"), default = FALSE)
  update_index_raw <- tolower(CRANDORE_getenv("CRANDORE_UPDATE_INDEX", "true"))
  
  # Lire la liste des packages (fichier prioritaire sur env var)
  packages_file <- CRANDORE_getenv("CRANDORE_PACKAGES_FILE", "")
  packages_seed <- if (identical(packages_file, "")) {
    # Utiliser CRANDORE_PACKAGES
    packages_csv <- CRANDORE_getenv("CRANDORE_PACKAGES", "")
    CRANDORE_read_packages_env(packages_csv)
  } else {
    # Utiliser le fichier
    CRANDORE_read_packages_file(packages_file)
  }
  
  local_root <- CRANDORE_getenv("CRANDORE_LOCAL_ROOT", CRANDORE_local_root_default())
  
  # 2) Config options(repos, HTTPUserAgent)
  repo_url <- CRANDORE_build_repo_url(os = os, distro = distro, snapshot_date = snapshot_date, base_url = base_url)
  repos <- c(CRAN = repo_url)
  
  triplet <- CRANDORE_pick_triplet(os = os, arch = arch)
  ua <- CRANDORE_build_http_user_agent(triplet, r_version_target = r_version_target)
  
  options(repos = repos, HTTPUserAgent = ua)
  
  # 3) Local path distinct par OS/target/Rmajor.minor
  r_mm <- CRANDORE_r_major_minor(r_version_target)
  target_id <- if (os == "linux") paste0(distro, "-", arch) else "windows-x86_64"
  local_repo <- file.path(local_root, os, target_id, paste0("R-", r_mm))
  dir.create(local_repo, recursive = TRUE, showWarnings = FALSE)
  
  CRANDORE_message(paste0("OS = ", os), verbose = verbose)
  if (os == "linux") {
    CRANDORE_message(paste0("Distro = ", distro), verbose = verbose)
    CRANDORE_message(paste0("Arch = ", arch), verbose = verbose)
  }
  CRANDORE_message(paste0("R version cible = ", as.character(r_version_target), " (", r_mm, ")"), verbose = verbose)
  CRANDORE_message(paste0("Snapshot = ", snapshot_date), verbose = verbose)
  CRANDORE_message(paste0("Repo PPM = ", repo_url), verbose = verbose)
  if (os == "windows") {
    CRANDORE_message(paste0("contriburl Windows = ", CRANDORE_contriburl_windows(repo_url, r_version_target)), verbose = verbose)
  }
  CRANDORE_message(paste0("Depot local = ", normalizePath(local_repo, winslash = "/", mustWork = FALSE)), verbose = verbose)
  
  # 4) Charger miniCRAN si necessaire
  if (!requireNamespace("miniCRAN", quietly = TRUE)) install.packages("miniCRAN")
  suppressPackageStartupMessages(library(miniCRAN))
  
  # 5) Lister paquets disponibles
  ap <- CRANDORE_available_packages(os = os, repos = repos, r_version_target = r_version_target)
  available_pkgs <- rownames(ap)
  CRANDORE_message(paste0("Nombre de packages disponibles: ", length(available_pkgs)), verbose = verbose)
  
  # 6) Determiner la cible (full ou dependances)
  if (isTRUE(full_snapshot)) {
    CRANDORE_message("Mode: full snapshot", verbose = verbose)
    pkgs_target <- available_pkgs
  } else {
    CRANDORE_message("Mode: partial snapshot", verbose = verbose)
    if (length(packages_seed) == 0) {
      pkgs_target <- character()
    } else {
      # pkgDep() interroge available.packages() deja configure via repos/options
      pkgs_target <- unique(miniCRAN::pkgDep(packages_seed))
    }
  }
  
  CRANDORE_message(paste0("Nombre de packages demandes: ", length(pkgs_target)), verbose = verbose)
  
  # 7) Resume: retirer deja telecharges
  deja_dl <- CRANDORE_list_downloaded_names(local_repo, os = os)
  deja_dl_current <- intersect(deja_dl, pkgs_target)
  pkgs_todo <- if (isTRUE(resume)) setdiff(pkgs_target, deja_dl) else pkgs_target
  
  CRANDORE_message(paste0("Deja telecharges (dans la cible): ", length(deja_dl_current)), verbose = verbose)
  CRANDORE_message(paste0("Deja telecharges (total): ", length(deja_dl)), verbose = verbose)
  CRANDORE_message(paste0("A telecharger: ", length(pkgs_todo)), verbose = verbose)
  
  # 8) Download avec progression
  total_target <- max(1L, length(pkgs_target))
  done_already <- length(deja_dl_current)
  
  for (i in seq_along(pkgs_todo)) {
    pct <- round((done_already + i - 1) / total_target * 100, 2)
    CRANDORE_message(paste0(pkgs_todo[i], " - ", pct, " %"), verbose = verbose)
    CRANDORE_download_one(
      pkg = pkgs_todo[i],
      os = os,
      repos = repos,
      local_repo = local_repo,
      r_version_target = r_version_target,
      verbose = verbose
    )
  }
  
  CRANDORE_message("DL done", verbose = verbose)

  # 9) Cleanup (only in partial mode) - AVANT la mise à jour PACKAGES
  if (isTRUE(cleanup) && !isTRUE(full_snapshot)) {
    CRANDORE_message("Starting cleanup of obsolete packages", verbose = verbose)
    cleanup_result <- CRANDORE_cleanup_obsolete_packages(local_repo, os, r_version_target, pkgs_target, verbose)
    CRANDORE_message(paste0("Cleanup: ", cleanup_result, " obsolete files removed"), verbose = verbose)
  }

  # 10) Update PACKAGES (UNE SEULE FOIS avec la liste finale)
  if (update_index_raw == "force") {
    CRANDORE_message("force update PACKAGES, PACKAGES.gz and PACKAGES.rds", verbose = verbose)
    idx_dir <- CRANDORE_write_packages_index(local_repo, os = os, r_version_target = r_version_target, verbose = verbose)
    CRANDORE_message("done", verbose = verbose)
  } else if (CRANDORE_as_logical(update_index_raw, default = TRUE)) {
    if (CRANDORE_index_needs_update(local_repo, os, r_version_target)) {
      CRANDORE_message("update PACKAGES, PACKAGES.gz and PACKAGES.rds", verbose = verbose)
      idx_dir <- CRANDORE_write_packages_index(local_repo, os = os, r_version_target = r_version_target, verbose = verbose)
      CRANDORE_message("done", verbose = verbose)
    } else {
      CRANDORE_message("PACKAGES index is up to date, skipping update", verbose = verbose)
    }
  } else {
    CRANDORE_message("Skipping PACKAGES index update (CRANDORE_UPDATE_INDEX=false)", verbose = verbose)
  }

  # 11) Control
  deja_dl2 <- CRANDORE_list_downloaded_names(local_repo, os = os)
  missing <- setdiff(pkgs_target, deja_dl2)
  
  if (length(missing) > 0) {
    CRANDORE_message(paste0("missing packages: ", paste(missing, collapse = ", ")), verbose = verbose)
  } else {
    CRANDORE_message("All packages downloaded", verbose = verbose)
  }
  
  invisible(list(
    os = os,
    distro = if (os == "linux") distro else NA_character_,
    arch = if (os == "linux") arch else NA_character_,
    snapshot_date = snapshot_date,
    r_version_target = as.character(r_version_target),
    r_major_minor = r_mm,
    repo_url = repo_url,
    repo_type = CRANDORE_repo_type(os),
    local_repo = normalizePath(local_repo, winslash = "/", mustWork = FALSE),
    packages_target_n = length(pkgs_target),
    packages_todo_n = length(pkgs_todo),
    missing_n = length(missing)
  ))
}

# Version avec paramètres explicites (utilise variables d'environnement comme valeurs par défaut)
CRANDORE_sync2 <- function(
  os = CRANDORE_getenv("CRANDORE_OS", if (tolower(.Platform$OS.type) == "unix") "linux" else tolower(.Platform$OS.type)),
  distro = CRANDORE_getenv("CRANDORE_DISTRO", CRANDORE_detect_container_distro()),
  arch = CRANDORE_getenv("CRANDORE_ARCH", tolower(R.Version()$arch)),
  snapshot_date = CRANDORE_getenv("CRANDORE_SNAPSHOT_DATE", ""),
  r_version = CRANDORE_getenv("CRANDORE_R_VERSION", ""),
  full_snapshot = CRANDORE_as_logical(CRANDORE_getenv("CRANDORE_FULL_SNAPSHOT", "false")),
  packages = CRANDORE_getenv("CRANDORE_PACKAGES", ""),
  packages_file = CRANDORE_getenv("CRANDORE_PACKAGES_FILE", ""),
  cleanup = CRANDORE_as_logical(CRANDORE_getenv("CRANDORE_CLEANUP", "false")),
  update_index = CRANDORE_getenv("CRANDORE_UPDATE_INDEX", "true"),
  base_url = CRANDORE_getenv("CRANDORE_BASE_URL", "https://packagemanager.posit.co/cran"),
  local_root = CRANDORE_getenv("CRANDORE_LOCAL_ROOT", CRANDORE_local_root_default()),
  verbose = CRANDORE_as_logical(CRANDORE_getenv("CRANDORE_VERBOSE", "true")),
  resume = CRANDORE_as_logical(CRANDORE_getenv("CRANDORE_RESUME", "true"))
) {
  # Sauvegarder les valeurs actuelles des variables d'environnement
  env_vars <- c(
    "CRANDORE_OS", "CRANDORE_DISTRO", "CRANDORE_ARCH", "CRANDORE_SNAPSHOT_DATE",
    "CRANDORE_R_VERSION", "CRANDORE_FULL_SNAPSHOT", "CRANDORE_PACKAGES",
    "CRANDORE_PACKAGES_FILE", "CRANDORE_CLEANUP", "CRANDORE_UPDATE_INDEX",
    "CRANDORE_BASE_URL", "CRANDORE_LOCAL_ROOT", "CRANDORE_VERBOSE", "CRANDORE_RESUME"
  )

  old_env <- Sys.getenv(env_vars, unset = NA)

  # Définir les nouvelles valeurs via variables d'environnement
  Sys.setenv(
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
  )

  # Appeler CRANDORE_sync avec gestion d'erreurs et restauration
  result <- tryCatch({
    CRANDORE_sync()
  }, finally = {
    # Restaurer les anciennes valeurs des variables d'environnement
    for (i in seq_along(env_vars)) {
      var_name <- env_vars[i]
      old_value <- old_env[i]
      if (!is.na(old_value)) {
        Sys.setenv(var_name, old_value)
      } else {
        Sys.unsetenv(var_name)
      }
    }
  })

  return(result)
}

CRANDORE_sync()
# ============================================================
# EXEMPLES
# ============================================================

# Exemple 1: Linux noble x86_64, snapshot 2025-12-08, R 4.5.0, partial, data.table
# Sys.setenv(
#   CRANDORE_OS = "linux",
#   CRANDORE_DISTRO = "noble",
#   CRANDORE_ARCH = "x86_64",
#   CRANDORE_SNAPSHOT_DATE = "2025-12-08",
#   CRANDORE_R_VERSION = "4.5.0",
#   CRANDORE_FULL_SNAPSHOT = "false",
#   CRANDORE_PACKAGES = "data.table",
#   CRANDORE_VERBOSE = "true",
#   CRANDORE_RESUME = "true"
# )
# res_linux <- CRANDORE_sync()
# res_linux
# 
# # Exemple 2: Windows, snapshot 2025-12-08, R 4.4.2, partial, data.table
# Sys.setenv(
#   CRANDORE_OS = "windows",
#   CRANDORE_SNAPSHOT_DATE = "2025-12-08",
#   CRANDORE_R_VERSION = "4.4.2",
#   CRANDORE_FULL_SNAPSHOT = "false",
#   CRANDORE_PACKAGES = "data.table",
#   CRANDORE_VERBOSE = "true",
#   CRANDORE_RESUME = "true"
# )
# res_win <- CRANDORE_sync()
# res_win

# Exemples d'utilisation de CRANDORE_sync2 avec paramètres explicites :
#
# # Utilisation programmatique simple
# result <- CRANDORE_sync2(packages = "tidyverse", cleanup = TRUE)
#
# # Configuration complète
# result <- CRANDORE_sync2(
#   os = "linux",
#   distro = "noble",
#   packages = "dplyr,ggplot2",
#   cleanup = TRUE,
#   update_index = "force",
#   verbose = TRUE
# )
#
# # Utilise les variables d'environnement actuelles (même comportement que CRANDORE_sync())
# result <- CRANDORE_sync2()
