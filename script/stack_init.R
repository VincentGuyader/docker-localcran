# ============================================================
# Crandore Stack Init
#
# Génère un fichier stack.yml prêt à l'emploi à partir de
# paramètres de haut niveau.
#
# Usage R :
#   source("script/stack_init.R")
#   crandore_init("stack.yml", packages = "tidyverse", dates = "2026-03-08")
#
# Usage Docker :
#   docker compose run --rm crandore-init
# ============================================================

#' Génère un fichier stack.yml pour Crandore
#'
#' @param output         Chemin du fichier à écrire (défaut : "stack.yml")
#' @param packages       Vecteur ou chaîne CSV de paquets R à inclure
#' @param dates          Date(s) de snapshot YYYY-MM-DD (défaut : aujourd'hui)
#' @param distros        Distributions Linux à cibler
#' @param r_versions     Version(s) R pour Linux (ex: c("4.4", "4.5"))
#' @param windows        Inclure des profils Windows ? (défaut : TRUE)
#' @param windows_r_versions Versions R pour Windows (défaut = r_versions)
#' @param arm            Inclure des profils aarch64 ? (défaut : FALSE)
#' @param full_snapshot  Télécharger tout le CRAN ? (défaut : FALSE)
#' @param local_root     Racine locale des repos dans le conteneur
#' @param overwrite      Écraser le fichier s'il existe déjà ? (défaut : FALSE)
#'
#' @return Le chemin du fichier généré (invisible)
crandore_init <- function(
  output              = "stack.yml",
  packages            = "tidyverse",
  dates               = NULL,   # NULL ou vide = aujourd'hui
  distros             = c("jammy", "noble", "centos8"),
  r_versions          = "4.4",
  windows             = TRUE,
  windows_r_versions  = NULL,
  arm                 = FALSE,
  full_snapshot       = FALSE,
  local_root          = "/minicran",
  overwrite           = FALSE
) {

  # --- Validation ---------------------------------------------------

  if (file.exists(output) && !isTRUE(overwrite)) {
    stop(sprintf(
      "'%s' existe déjà. Utilisez overwrite = TRUE pour l'écraser.", output
    ))
  }

  # NULL ou vecteur vide → aujourd'hui ; nettoyer les espaces parasites
  if (is.null(dates) || length(dates) == 0 || all(!nzchar(trimws(as.character(dates))))) {
    dates <- as.character(Sys.Date())
  }
  dates <- trimws(as.character(dates))
  dates <- dates[nzchar(dates)]

  for (d in dates) {
    tryCatch(
      validate_snapshot_date(d),
      error = function(e) stop(sprintf("Date invalide '%s' : %s", d, conditionMessage(e)))
    )
  }

  if (is.character(packages) && length(packages) == 1 && grepl(",", packages)) {
    packages <- trimws(strsplit(packages, ",")[[1]])
  }
  packages <- unique(packages[nzchar(packages)])
  if (length(packages) == 0 && !isTRUE(full_snapshot)) {
    stop("Spécifiez au moins un paquet dans 'packages', ou passez full_snapshot = TRUE.")
  }

  r_versions <- as.character(r_versions)
  if (is.null(windows_r_versions)) windows_r_versions <- r_versions
  windows_r_versions <- as.character(windows_r_versions)

  supported_distros <- c(
    "jammy", "noble", "bookworm", "focal",
    "centos7", "centos8", "rhel9", "rhel10",
    "opensuse156", "manylinux_2_28"
  )
  unknown <- setdiff(distros, supported_distros)
  if (length(unknown) > 0) {
    warning(sprintf(
      "Distro(s) inconnue(s) : %s. Valeurs supportées : %s",
      paste(unknown, collapse = ", "),
      paste(supported_distros, collapse = ", ")
    ))
  }

  # Avertir si noble est demandé pour une date < 2024-04
  noble_dates <- dates[dates < "2024-04-01"]
  if ("noble" %in% distros && length(noble_dates) > 0) {
    warning(sprintf(
      "Ubuntu Noble (24.04) n'existait pas avant avril 2024. Dates concernées : %s",
      paste(noble_dates, collapse = ", ")
    ))
  }

  # --- Construction des profils -------------------------------------

  profiles  <- list()
  snap_profiles <- character()  # noms des profils à utiliser dans tous les snapshots

  # Profils Linux x86_64
  for (rv in r_versions) {
    rv_tag    <- gsub("\\.", "", rv)
    prof_name <- sprintf("linux_%s", rv_tag)
    snap_profiles <- c(snap_profiles, prof_name)

    profile_body <- list(
      os        = "linux",
      arch      = "x86_64",
      r_version = rv,
      distros   = as.list(distros)
    )
    if (isTRUE(full_snapshot)) profile_body$full_snapshot <- TRUE
    if (!isTRUE(full_snapshot) && length(packages) > 0)
      profile_body$packages <- as.list(packages)

    profiles[[prof_name]] <- profile_body
  }

  # Profils Linux aarch64
  if (isTRUE(arm)) {
    for (rv in r_versions) {
      rv_tag    <- gsub("\\.", "", rv)
      prof_name <- sprintf("linux_arm_%s", rv_tag)
      snap_profiles <- c(snap_profiles, prof_name)

      profile_body <- list(
        os        = "linux",
        arch      = "aarch64",
        r_version = rv,
        distros   = as.list(distros)
      )
      if (isTRUE(full_snapshot)) profile_body$full_snapshot <- TRUE
      if (!isTRUE(full_snapshot) && length(packages) > 0)
        profile_body$packages <- as.list(packages)

      profiles[[prof_name]] <- profile_body
    }
  }

  # Profils Windows
  if (isTRUE(windows)) {
    for (rv in windows_r_versions) {
      rv_tag    <- gsub("\\.", "", rv)
      prof_name <- sprintf("windows_%s", rv_tag)
      snap_profiles <- c(snap_profiles, prof_name)

      profile_body <- list(os = "windows", r_version = rv)
      if (!isTRUE(full_snapshot) && length(packages) > 0)
        profile_body$packages <- as.list(packages)

      profiles[[prof_name]] <- profile_body
    }
  }

  # --- Construction des snapshots -----------------------------------

  snapshots <- setNames(
    lapply(dates, function(d) list(profiles = as.list(snap_profiles))),
    dates
  )

  # --- Sérialisation YAML manuelle ----------------------------------
  # On écrit à la main pour contrôler l'indentation et les commentaires.

  pkg_lines <- if (isTRUE(full_snapshot)) {
    "    # ignoré (full_snapshot: true dans les profils)"
  } else {
    paste0("    - ", packages, collapse = "\n")
  }

  lines <- c(
    sprintf("# Généré par crandore_init() le %s", Sys.Date()),
    "# Modifiez ce fichier selon vos besoins et lancez :",
    "#   docker compose run --rm crandore-stack",
    "",
    "settings:",
    sprintf("  local_root: %s", local_root),
    "  packages:",
    pkg_lines,
    "",
    "profiles:"
  )

  for (pname in names(profiles)) {
    p     <- profiles[[pname]]
    lines <- c(lines, sprintf("  %s:", pname))
    lines <- c(lines, sprintf("    os: %s", p$os))
    lines <- c(lines, sprintf("    r_version: \"%s\"", p$r_version))

    if (!is.null(p$arch)) {
      lines <- c(lines, sprintf("    arch: %s", p$arch))
    }
    if (!is.null(p$distros)) {
      lines <- c(lines,
        sprintf("    distros: [%s]", paste(unlist(p$distros), collapse = ", "))
      )
    }
    if (isTRUE(p$full_snapshot)) {
      lines <- c(lines, "    full_snapshot: true")
    }
    if (!is.null(p$packages) && !isTRUE(p$full_snapshot)) {
      lines <- c(lines,
        "    packages:",
        paste0("      - ", unlist(p$packages), collapse = "\n")
      )
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, "snapshots:")
  for (d in names(snapshots)) {
    snap  <- snapshots[[d]]
    lines <- c(lines, sprintf("  %s:", d))
    lines <- c(lines, "    profiles:")
    for (pn in unlist(snap$profiles)) {
      lines <- c(lines, sprintf("      - %s", pn))
    }
  }

  yaml_text <- paste(lines, collapse = "\n")

  # --- Écriture -----------------------------------------------------

  writeLines(yaml_text, output)

  # --- Résumé -------------------------------------------------------

  n_builds <- length(snap_profiles) * length(dates)
  message(sprintf("stack.yml généré : '%s'", normalizePath(output, mustWork = FALSE)))
  message(sprintf(
    "  %d profil(s) x %d snapshot(s) = %d build(s) total",
    length(snap_profiles), length(dates), n_builds
  ))
  message("  Profils :")
  for (pname in names(profiles)) {
    p <- profiles[[pname]]
    mode <- if (isTRUE(p$full_snapshot)) "FULL CRAN" else paste(unlist(p$packages), collapse = ", ")
    target <- if (p$os == "linux") {
      sprintf("linux/%s [%s]", p$arch, paste(unlist(p$distros), collapse = ", "))
    } else {
      "windows/x86_64"
    }
    message(sprintf("    %-25s R %s | %s | %s", pname, p$r_version, target, mode))
  }
  message("  Dates :", paste(dates, collapse = ", "))
  message("\nLancez : docker compose run --rm crandore-stack")

  invisible(output)
}

# --- Point d'entrée Docker ----------------------------------------
# Activé via CRANDORE_INIT=true dans l'environnement du conteneur.

if (identical(Sys.getenv("CRANDORE_INIT"), "true")) {
  output_path <- Sys.getenv("CRANDORE_INIT_OUTPUT", "/output/stack.yml")

  getenv_or <- function(key, default) {
    v <- trimws(Sys.getenv(key, ""))
    if (nzchar(v)) v else default
  }
  getenv_list <- function(key, default) {
    strsplit(getenv_or(key, default), ",")[[1]]
  }

  crandore_init(
    output             = output_path,
    packages           = getenv_list("CRANDORE_PACKAGES", "tidyverse"),
    dates              = {
      v <- trimws(Sys.getenv("CRANDORE_INIT_DATES", ""))
      if (nzchar(v)) strsplit(v, ",")[[1]] else NULL
    },
    distros            = getenv_list("CRANDORE_INIT_DISTROS", "jammy,noble,centos8"),
    r_versions         = getenv_list("CRANDORE_INIT_R_VERSIONS", "4.4"),
    windows            = as_logical(getenv_or("CRANDORE_INIT_WINDOWS", "true")),
    windows_r_versions = {
      v <- Sys.getenv("CRANDORE_INIT_WINDOWS_R_VERSIONS", "")
      if (nzchar(v)) strsplit(v, ",")[[1]] else NULL
    },
    arm                = as_logical(getenv_or("CRANDORE_INIT_ARM", "false")),
    full_snapshot      = as_logical(getenv_or("CRANDORE_FULL_SNAPSHOT", "false")),
    local_root         = getenv_or("CRANDORE_LOCAL_ROOT", "/minicran"),
    overwrite          = TRUE
  )
}
