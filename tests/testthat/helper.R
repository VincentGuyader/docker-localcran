# Helper commun à tous les fichiers de tests.
# Résout le chemin des scripts selon le contexte (Docker vs local).

script_dir <- Sys.getenv("CRANDORE_SCRIPT_DIR", {
  # En local : remonter depuis tests/testthat/ jusqu'à la racine du repo
  candidates <- c(
    file.path(getwd(), "script"),                        # si lancé depuis la racine
    file.path(dirname(dirname(getwd())), "script"),      # si lancé depuis tests/testthat
    "/script"                                            # Docker
  )
  found <- Filter(function(p) file.exists(file.path(p, "repos_snapshot.R")), candidates)
  if (length(found) == 0) stop("Impossible de localiser le dossier script/")
  found[[1]]
})

source(file.path(script_dir, "repos_snapshot.R"))
source(file.path(script_dir, "stack_utils.R"))
