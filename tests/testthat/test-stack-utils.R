# Fixture : stack minimal réutilisable dans tous les tests
make_stack <- function() {
  list(
    settings = list(
      local_root = "/minicran",
      packages   = list("tidyverse")
    ),
    profiles = list(
      linux_44 = list(
        os      = "linux",
        arch    = "x86_64",
        r_version = "4.4",
        distros = list("jammy", "noble")
      ),
      linux_43 = list(
        os      = "linux",
        arch    = "x86_64",
        r_version = "4.3",
        distros = list("jammy")
      ),
      windows_44 = list(
        os        = "windows",
        r_version = "4.4"
      ),
      linux_full = list(
        os            = "linux",
        arch          = "x86_64",
        r_version     = "4.4",
        distros       = list("jammy"),
        full_snapshot = TRUE
      ),
      linux_custom = list(
        os        = "linux",
        arch      = "x86_64",
        r_version = "4.4",
        distros   = list("noble"),
        packages  = list("shiny", "data.table")
      ),
      source_universal = list(
        os = "source"
      )
    ),
    snapshots = list(
      "2026-03-08" = list(profiles = list("linux_44", "windows_44")),
      "2025-09-08" = list(profiles = list("linux_44")),
      "2024-03-08" = list(profiles = list("linux_43"))
    )
  )
}

# --- Nombre de builds -------------------------------------------------

test_that("resolve_builds retourne le bon nombre de builds (sans filtre)", {
  builds <- resolve_builds(make_stack(), "", "", "tidyverse")
  # 2026 : linux_44 (jammy+noble) + windows_44 = 3
  # 2025 : linux_44 (jammy+noble) = 2
  # 2024 : linux_43 (jammy) = 1
  expect_equal(length(builds), 6)
})

# --- Filtres ----------------------------------------------------------

test_that("ONLY_DATE filtre sur une seule date", {
  builds <- resolve_builds(make_stack(), "2026-03-08", "", "tidyverse")
  expect_equal(length(builds), 3)
  expect_true(all(sapply(builds, function(b) b$date == "2026-03-08")))
})

test_that("ONLY_DATE sur une date inexistante retourne une liste vide", {
  builds <- resolve_builds(make_stack(), "1999-01-01", "", "tidyverse")
  expect_equal(length(builds), 0)
})

test_that("ONLY_PROFILE filtre sur un seul profil (toutes les dates)", {
  builds <- resolve_builds(make_stack(), "", "windows_44", "tidyverse")
  expect_equal(length(builds), 1)
  expect_equal(builds[[1]]$os, "windows")
})

test_that("ONLY_DATE + ONLY_PROFILE combinés filtrent correctement", {
  builds <- resolve_builds(make_stack(), "2026-03-08", "linux_44", "tidyverse")
  expect_equal(length(builds), 2)  # jammy + noble
  expect_true(all(sapply(builds, function(b) b$os == "linux")))
})

# --- Contenu des builds -----------------------------------------------

test_that("un build linux a les bons champs", {
  builds <- resolve_builds(make_stack(), "2024-03-08", "linux_43", "tidyverse")
  b <- builds[[1]]
  expect_equal(b$os,        "linux")
  expect_equal(b$distro,    "jammy")
  expect_equal(b$arch,      "x86_64")
  expect_equal(b$r_version, "4.3")
  expect_equal(b$date,      "2024-03-08")
  expect_equal(b$profile,   "linux_43")
})

test_that("un build windows a les bons champs", {
  builds <- resolve_builds(make_stack(), "2026-03-08", "windows_44", "tidyverse")
  b <- builds[[1]]
  expect_equal(b$os,    "windows")
  expect_equal(b$distro, "")
  expect_equal(b$arch,  "x86_64")
})

test_that("un profil source ne nécessite ni distro ni arch ni r_version", {
  s <- make_stack()
  s$snapshots[["2026-03-08"]] <- list(profiles = list("source_universal"))
  builds <- resolve_builds(s, "2026-03-08", "", "tidyverse")
  expect_equal(length(builds), 1)
  b <- builds[[1]]
  expect_equal(b$os,        "source")
  expect_equal(b$distro,    "")
  expect_equal(b$arch,      "")
  expect_equal(b$r_version, "")
  expect_equal(b$packages,  "tidyverse")
})

test_that("linux_44 avec 2 distros génère 2 builds distincts", {
  builds <- resolve_builds(make_stack(), "2026-03-08", "linux_44", "tidyverse")
  distros <- sapply(builds, function(b) b$distro)
  expect_setequal(distros, c("jammy", "noble"))
})

# --- Packages et full_snapshot ----------------------------------------

test_that("packages par défaut (settings) utilisés quand le profil n'en définit pas", {
  builds <- resolve_builds(make_stack(), "2026-03-08", "windows_44", "tidyverse")
  expect_equal(builds[[1]]$packages, "tidyverse")
  expect_false(builds[[1]]$full_snapshot)
})

test_that("full_snapshot = TRUE quand défini dans le profil", {
  s <- make_stack()
  s$snapshots[["2026-03-08"]] <- list(profiles = list("linux_full"))
  builds <- resolve_builds(s, "2026-03-08", "", "tidyverse")
  expect_equal(length(builds), 1)
  expect_true(builds[[1]]$full_snapshot)
})

test_that("packages au niveau profil remplace le défaut", {
  s <- make_stack()
  s$snapshots[["2026-03-08"]] <- list(profiles = list("linux_custom"))
  builds <- resolve_builds(s, "2026-03-08", "", "tidyverse")
  expect_equal(builds[[1]]$packages, "shiny,data.table")
  expect_false(builds[[1]]$full_snapshot)
})

test_that("default_packages est propagé correctement", {
  builds <- resolve_builds(make_stack(), "2026-03-08", "linux_44", "dplyr,ggplot2")
  expect_true(all(sapply(builds, function(b) b$packages == "dplyr,ggplot2")))
})

# --- Erreurs ----------------------------------------------------------

test_that("profil inexistant lève une erreur explicite", {
  s <- make_stack()
  s$snapshots[["2026-03-08"]] <- list(profiles = list("profil_inexistant"))
  expect_error(
    resolve_builds(s, "2026-03-08", "", "tidyverse"),
    "non défini"
  )
})

test_that("os inconnu dans un profil lève une erreur", {
  s <- make_stack()
  s$profiles$bad_os <- list(os = "solaris", r_version = "4.4")
  s$snapshots[["2026-03-08"]] <- list(profiles = list("bad_os"))
  expect_error(
    resolve_builds(s, "2026-03-08", "", "tidyverse"),
    "inconnu"
  )
})
