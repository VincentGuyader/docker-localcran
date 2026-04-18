# ============================================================
# Crandore Stack Utilities
#
# Fonctions pures pour la résolution profils × snapshots.
# Séparé de stack_runner.R pour permettre les tests unitaires.
# ============================================================

#' Résout la liste des builds à partir d'un stack parsé
#'
#' @param stack        Liste parsée depuis stack.yml
#' @param only_date    Filtre sur une date (chaîne vide = pas de filtre)
#' @param only_profile Filtre sur un profil (chaîne vide = pas de filtre)
#' @param default_packages Packages par défaut (depuis settings) si le profil
#'                         n'en définit pas
#' @return Liste de listes, chaque élément décrit un build
resolve_builds <- function(stack, only_date, only_profile, default_packages) {
  builds <- list()

  for (date in names(stack$snapshots)) {
    if (nzchar(only_date) && date != only_date) next

    snapshot <- stack$snapshots[[date]]

    for (profile_name in snapshot$profiles) {
      if (nzchar(only_profile) && profile_name != only_profile) next

      if (!profile_name %in% names(stack$profiles)) {
        stop(sprintf("Profil '%s' référencé dans snapshot '%s' mais non défini",
                     profile_name, date))
      }

      p <- stack$profiles[[profile_name]]

      # Résoudre packages/full_snapshot : profil > settings
      build_full_snapshot <- isTRUE(p$full_snapshot)
      build_packages <- if (!is.null(p$packages)) {
        paste(p$packages, collapse = ",")
      } else {
        default_packages
      }

      if (p$os == "linux") {
        for (distro in p$distros) {
          builds <- c(builds, list(list(
            date          = date,
            profile       = profile_name,
            os            = "linux",
            distro        = distro,
            arch          = p$arch,
            r_version     = as.character(p$r_version),
            full_snapshot = build_full_snapshot,
            packages      = build_packages
          )))
        }
      } else if (p$os == "windows") {
        builds <- c(builds, list(list(
          date          = date,
          profile       = profile_name,
          os            = "windows",
          distro        = "",
          arch          = "x86_64",
          r_version     = as.character(p$r_version),
          full_snapshot = build_full_snapshot,
          packages      = build_packages
        )))
      } else if (p$os == "source") {
        # Source repos are platform- and R-version-independent : no distro
        # iteration, no arch, no r_version key required.
        builds <- c(builds, list(list(
          date          = date,
          profile       = profile_name,
          os            = "source",
          distro        = "",
          arch          = "",
          r_version     = "",
          full_snapshot = build_full_snapshot,
          packages      = build_packages
        )))
      } else {
        stop(sprintf("os '%s' inconnu dans le profil '%s'", p$os, profile_name))
      }
    }
  }

  builds
}
