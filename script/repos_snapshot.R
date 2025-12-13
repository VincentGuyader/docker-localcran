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
  if (!(identical(d, "latest") || grepl("^\\d{4}-\\d{2}-\\d{2}$", d))) {
    stop("CRANDORE_SNAPSHOT_DATE doit etre 'latest' ou au format YYYY-MM-DD")
  }
  d
}

as_pkg_version <- function(x) {
  if (inherits(x, "package_version")) return(x)
  if (is.numeric(x)) x <- as.character(x)
  if (!is.character(x) || length(x) != 1 || !grepl("^\\d+\\.\\d+(\\.\\d+)?$", x)) {
    stop("CRANDORE_R_VERSION doit etre une version du type '4.5.0' ou '4.5'")
  }
  package_version(x)
}

r_major_minor <- function(r_version) {
  v <- as_pkg_version(r_version)
  parts <- strsplit(as.character(v), "\\.")[[1]]
  if (length(parts) < 2) stop("CRANDORE_R_VERSION doit avoir au moins major.minor (ex: '4.5' ou '4.5.0')")
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
  # Utilise le triplet cible (adapté à l'OS/arch spécifiés ou détectés)
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

display_info<- function(..., verbose = TRUE) {
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

    # Copier les fichiers PACKAGES vers src/contrib/ pour permettre la détection automatique par R
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

# Telechargement unitaire "robuste" + resume + verbose
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

available_packages <- function(os, repos, r_version_target) {
  if (os == "windows") {
    cu <- contriburl_windows(repos[[1]], r_version_target)
    available.packages(repos = repos, type = "win.binary", contriburl = cu)
  } else {
    available.packages(repos = repos, type = "source")
  }
}

index_needs_update <- function(local_repo, os, r_version_target) {
  # Pour Windows, vérifier les deux fichiers PACKAGES (bin/ et src/)
  if (os == "windows") {
    mm <- r_major_minor(r_version_target)
    packages_files <- c(
      file.path(local_repo, "bin", "windows", "contrib", mm, "PACKAGES"),
      file.path(local_repo, "src", "contrib", "PACKAGES")
    )
  } else {
    packages_files <- file.path(local_repo, "src", "contrib", "PACKAGES")
  }

  # Vérifier si au moins un fichier PACKAGES existe
  existing_files <- packages_files[file.exists(packages_files)]
  if (length(existing_files) == 0) {
    return(TRUE)  # Aucun PACKAGES n'existe
  }

  # Utiliser le fichier PACKAGES le plus récent pour la vérification
  packages_file <- existing_files[which.max(file.mtime(existing_files))]

  # Compter les fichiers de packages présents
  pkg_count <- length(list_downloaded_names(local_repo, os))

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
    mm <- r_major_minor(r_version_target)
    file.path(local_repo, "bin", "windows", "contrib", mm)
  } else {
    file.path(local_repo, "src", "contrib")
  }

  if (dir.exists(pkg_dir)) {
    pattern <- if (os == "windows") "\\.zip$" else "\\.tar\\.gz$"
    pkg_files <- list.files(pkg_dir, pattern = pattern, full.names = TRUE)

    if (length(pkg_files) > 0) {
      # Timestamp du fichier PACKAGES le plus récent
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

cleanup_obsolete_packages <- function(local_repo, os, r_version_target, pkgs_target, verbose = TRUE) {
  # Lister tous les packages présents dans le dépôt
  all_downloaded <- list_downloaded_names(local_repo, os = os)

  # Identifier les packages obsolètes (présents mais pas dans la cible)
  obsolete_pkgs <- setdiff(all_downloaded, pkgs_target)

  if (length(obsolete_pkgs) == 0) {
    display_info("No obsolete packages to clean up", verbose = verbose)
    return(0)
  }

  display_info(paste0("Found ", length(obsolete_pkgs), " obsolete packages to clean up: ",
                          paste(obsolete_pkgs, collapse = ", ")), verbose = verbose)

  # Supprimer les fichiers obsolètes
  removed_count <- 0
  if (os == "windows") {
    mm <- r_major_minor(r_version_target)
    pkg_dir <- file.path(local_repo, "bin", "windows", "contrib", mm)
    for (pkg in obsolete_pkgs) {
      # Chercher les fichiers .zip correspondants
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
      # Chercher les fichiers .tar.gz correspondants
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
  # 1) Lire la config
  verbose <- as_logical(getenv("CRANDORE_VERBOSE", "true"), default = TRUE)

  # Detecter OS/arch courants comme valeurs par defaut
  current_os_type <- tolower(.Platform$OS.type)
  current_os <- if (current_os_type == "unix") "linux" else current_os_type
  current_arch <- tolower(R.Version()$arch)

  os <- tolower(getenv("CRANDORE_OS", current_os))
  if (!os %in% c("linux","windows")) stop("CRANDORE_OS doit etre 'linux' ou 'windows'")

  distro <- tolower(getenv("CRANDORE_DISTRO", detect_container_distro()))
  arch <- tolower(getenv("CRANDORE_ARCH", current_arch))
  if (!arch %in% c("x86_64","aarch64")) stop("CRANDORE_ARCH doit etre 'x86_64' ou 'aarch64' (linux)")
  if (os == "linux" && !distro %in% linux_distros) {
    stop(sprintf("CRANDORE_DISTRO inconnue: '%s'. Valeurs: %s", distro, paste(linux_distros, collapse = ", ")))
  }
  
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
  
  # Lire la liste des packages (fichier prioritaire sur env var)
  packages_file <- getenv("CRANDORE_PACKAGES_FILE", "")
  packages_seed <- if (identical(packages_file, "")) {
    # Utiliser CRANDORE_PACKAGES
    packages_csv <- getenv("CRANDORE_PACKAGES", "")
    read_packages_env(packages_csv)
  } else {
    # Utiliser le fichier
    read_packages_file(packages_file)
  }
  
  local_root <- getenv("CRANDORE_LOCAL_ROOT", local_root_default())
  
  # 2) Config options(repos, HTTPUserAgent)
  repo_url <- build_repo_url(os = os, distro = distro, snapshot_date = snapshot_date, base_url = base_url)
  repos <- c(CRAN = repo_url)
  
  triplet <- pick_triplet(os = os, arch = arch)
  ua <- build_http_user_agent(triplet, r_version_target = r_version_target)
  
  options(repos = repos, HTTPUserAgent = ua)
  
  # 3) Local path distinct par OS/target/Rmajor.minor
  r_mm <- r_major_minor(r_version_target)
  target_id <- if (os == "linux") paste0(distro, "-", arch) else "windows-x86_64"
  local_repo <- file.path(local_root, os, target_id, paste0("R-", r_mm))
  dir.create(local_repo, recursive = TRUE, showWarnings = FALSE)
  
  display_info(paste0("OS = ", os), verbose = verbose)
  if (os == "linux") {
    display_info(paste0("Distro = ", distro), verbose = verbose)
    display_info(paste0("Arch = ", arch), verbose = verbose)
  }
  display_info(paste0("R version cible = ", as.character(r_version_target), " (", r_mm, ")"), verbose = verbose)
  display_info(paste0("Snapshot = ", snapshot_date), verbose = verbose)
  display_info(paste0("Repo PPM = ", repo_url), verbose = verbose)
  if (os == "windows") {
    display_info(paste0("contriburl Windows = ", contriburl_windows(repo_url, r_version_target)), verbose = verbose)
  }
  display_info(paste0("Depot local = ", normalizePath(local_repo, winslash = "/", mustWork = FALSE)), verbose = verbose)
  
  # 4) Charger miniCRAN si necessaire
  suppressPackageStartupMessages(library(miniCRAN))
  
  # 5) Lister paquets disponibles
  ap <- available_packages(os = os, repos = repos, r_version_target = r_version_target)
  available_pkgs <- rownames(ap)
  display_info(paste0("Nombre de packages disponibles: ", length(available_pkgs)), verbose = verbose)
  
  # 6) Determiner la cible (full ou dependances)
  if (isTRUE(full_snapshot)) {
    display_info("Mode: full snapshot", verbose = verbose)
    pkgs_target <- available_pkgs
  } else {
    display_info("Mode: partial snapshot", verbose = verbose)
    if (length(packages_seed) == 0) {
      pkgs_target <- character()
    } else {
      # pkgDep() interroge available.packages() deja configure via repos/options
      pkgs_target <- unique(miniCRAN::pkgDep(packages_seed))
    }
  }
  
  display_info(paste0("Nombre de packages demandes: ", length(pkgs_target)), verbose = verbose)
  
  # 7) Resume: retirer deja telecharges
  deja_dl <- list_downloaded_names(local_repo, os = os)
  deja_dl_current <- intersect(deja_dl, pkgs_target)
  pkgs_todo <- if (isTRUE(resume)) setdiff(pkgs_target, deja_dl) else pkgs_target
  
  display_info(paste0("Deja telecharges (dans la cible): ", length(deja_dl_current)), verbose = verbose)
  display_info(paste0("Deja telecharges (total): ", length(deja_dl)), verbose = verbose)
  display_info(paste0("A telecharger: ", length(pkgs_todo)), verbose = verbose)
  
  # 8) Download avec progression
  total_target <- max(1L, length(pkgs_target))
  done_already <- length(deja_dl_current)
  
  for (i in seq_along(pkgs_todo)) {
    pct <- round((done_already + i - 1) / total_target * 100, 2)
    display_info(paste0(pkgs_todo[i], " - ", pct, " %"), verbose = verbose)
    download_one(
      pkg = pkgs_todo[i],
      os = os,
      repos = repos,
      local_repo = local_repo,
      r_version_target = r_version_target,
      verbose = verbose
    )
  }
  
  display_info("DL done", verbose = verbose)

  # 9) Cleanup (only in partial mode) - AVANT la mise à jour PACKAGES
  if (isTRUE(cleanup) && !isTRUE(full_snapshot)) {
    display_info("Starting cleanup of obsolete packages", verbose = verbose)
    cleanup_result <- cleanup_obsolete_packages(local_repo, os, r_version_target, pkgs_target, verbose)
    display_info(paste0("Cleanup: ", cleanup_result, " obsolete files removed"), verbose = verbose)
  }

  # 10) Update PACKAGES (UNE SEULE FOIS avec la liste finale)
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

# Version avec paramètres explicites (utilise variables d'environnement comme valeurs par défaut)
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
  # Utiliser withr::with_envvar pour une gestion propre des variables d'environnement

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
