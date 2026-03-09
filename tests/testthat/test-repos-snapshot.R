test_that("r_major_minor extrait correctement major.minor", {
  expect_equal(r_major_minor("4.4.0"), "4.4")
  expect_equal(r_major_minor("4.3.1"), "4.3")
  expect_equal(r_major_minor("4.4"),   "4.4")
  expect_equal(r_major_minor("3.6.3"), "3.6")
})

test_that("r_major_minor rejette les entrées invalides", {
  expect_error(r_major_minor("invalid"))
  expect_error(r_major_minor("4"))
  expect_error(r_major_minor(""))
})

test_that("validate_snapshot_date accepte les valeurs correctes", {
  expect_equal(validate_snapshot_date("2024-01-15"), "2024-01-15")
  expect_equal(validate_snapshot_date("2020-06-01"), "2020-06-01")
  expect_equal(validate_snapshot_date("latest"),     "latest")
})

test_that("validate_snapshot_date rejette les valeurs incorrectes", {
  expect_error(validate_snapshot_date("not-a-date"))
  expect_error(validate_snapshot_date("01-15-2024"))  # format US
  expect_error(validate_snapshot_date("2024-13-01"))  # mois invalide
})

test_that("validate_snapshot_date rejette les dates futures", {
  future <- format(Sys.Date() + 10, "%Y-%m-%d")
  expect_error(validate_snapshot_date(future))
})

test_that("as_logical convertit TRUE correctement", {
  for (v in c("true", "TRUE", "True", "1", "yes", "oui", "y", "t")) {
    expect_true(as_logical(v), info = paste("valeur:", v))
  }
})

test_that("as_logical convertit FALSE correctement", {
  for (v in c("false", "FALSE", "0", "no", "non", "n", "f", "")) {
    expect_false(as_logical(v), info = paste("valeur:", v))
  }
})

test_that("as_logical retourne le défaut pour les valeurs inconnues", {
  expect_true(as_logical("???", default = TRUE))
  expect_false(as_logical("???", default = FALSE))
})

test_that("build_repo_url construit les URLs linux correctement", {
  url <- build_repo_url("linux", "jammy", "2024-01-01",
                        "https://packagemanager.posit.co/cran")
  expect_equal(url, "https://packagemanager.posit.co/cran/__linux__/jammy/2024-01-01")
})

test_that("build_repo_url construit les URLs windows correctement", {
  url <- build_repo_url("windows", NULL, "2024-01-01",
                        "https://packagemanager.posit.co/cran")
  expect_equal(url, "https://packagemanager.posit.co/cran/2024-01-01")
})

test_that("build_repo_url fonctionne avec latest", {
  url <- build_repo_url("linux", "noble", "latest",
                        "https://packagemanager.posit.co/cran")
  expect_equal(url, "https://packagemanager.posit.co/cran/__linux__/noble/latest")
})

test_that("read_packages_env parse un CSV correctement", {
  expect_equal(read_packages_env("dplyr,ggplot2"),        c("dplyr", "ggplot2"))
  expect_equal(read_packages_env("dplyr , ggplot2"),      c("dplyr", "ggplot2"))  # espaces
  expect_equal(read_packages_env("dplyr"),                "dplyr")
  expect_equal(read_packages_env(""),                     character())
})

test_that("read_packages_env déduplique les entrées", {
  expect_equal(read_packages_env("dplyr,dplyr,ggplot2"), c("dplyr", "ggplot2"))
})

test_that("read_packages_file lit un fichier correctement", {
  tmp <- tempfile()
  writeLines(c("dplyr", "# un commentaire", "ggplot2", "", "  tidyr  "), tmp)
  on.exit(unlink(tmp))

  result <- read_packages_file(tmp)
  expect_equal(result, c("dplyr", "ggplot2", "tidyr"))
})

test_that("read_packages_file déduplique les entrées", {
  tmp <- tempfile()
  writeLines(c("dplyr", "ggplot2", "dplyr"), tmp)
  on.exit(unlink(tmp))

  expect_equal(read_packages_file(tmp), c("dplyr", "ggplot2"))
})

test_that("read_packages_file errore si le fichier n'existe pas", {
  expect_error(read_packages_file("/nonexistent/packages.list"))
})

test_that("index_needs_update retourne TRUE si PACKAGES absent", {
  tmp <- tempdir()
  expect_true(index_needs_update(tmp, "linux", "4.4.0"))
})

test_that("index_needs_update retourne TRUE si le nombre de paquets diffère", {
  tmp <- file.path(tempdir(), paste0("crandore_test_", as.integer(Sys.time())))
  dir.create(file.path(tmp, "src", "contrib"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))

  # 2 fichiers .tar.gz
  file.create(file.path(tmp, "src", "contrib", "pkg1_1.0.0.tar.gz"))
  file.create(file.path(tmp, "src", "contrib", "pkg2_2.0.0.tar.gz"))

  # PACKAGES avec seulement 1 entrée (désaccord)
  writeLines(c("Package: pkg1", "Version: 1.0.0", ""),
             file.path(tmp, "src", "contrib", "PACKAGES"))

  expect_true(index_needs_update(tmp, "linux", "4.4.0"))
})

test_that("index_needs_update retourne FALSE quand tout est cohérent", {
  tmp <- file.path(tempdir(), paste0("crandore_test2_", as.integer(Sys.time())))
  dir.create(file.path(tmp, "src", "contrib"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))

  # Créer les .tar.gz d'abord
  file.create(file.path(tmp, "src", "contrib", "pkg1_1.0.0.tar.gz"))
  file.create(file.path(tmp, "src", "contrib", "pkg2_2.0.0.tar.gz"))

  # Attendre que le timestamp soit différent (1 seconde)
  Sys.sleep(1)

  # PACKAGES avec 2 entrées, plus récent que les .tar.gz
  writeLines(c("Package: pkg1", "Version: 1.0.0", "",
               "Package: pkg2", "Version: 2.0.0", ""),
             file.path(tmp, "src", "contrib", "PACKAGES"))

  expect_false(index_needs_update(tmp, "linux", "4.4.0"))
})
