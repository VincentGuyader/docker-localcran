# ============================================================
# Crandore Stack Runner
#
# Lit un fichier stack.yml, résout les profils × snapshots,
# et appelle crandore() pour chaque build.
#
# Env vars :
#   CRANDORE_STACK_FILE  : chemin vers le fichier stack.yml (requis)
#   CRANDORE_ONLY_DATE   : si renseigné, ne traite que ce snapshot (YYYY-MM-DD)
#   CRANDORE_ONLY_PROFILE: si renseigné, ne traite que ce profil
# ============================================================

library(yaml)

stack_file   <- Sys.getenv("CRANDORE_STACK_FILE", "")
only_date    <- Sys.getenv("CRANDORE_ONLY_DATE", "")
only_profile <- Sys.getenv("CRANDORE_ONLY_PROFILE", "")

if (!nzchar(stack_file) || !file.exists(stack_file)) {
  stop(sprintf("CRANDORE_STACK_FILE introuvable : '%s'", stack_file))
}

stack <- yaml::read_yaml(stack_file)

# --- Validation minimale -----------------------------------------------

required_keys <- c("settings", "profiles", "snapshots")
missing <- setdiff(required_keys, names(stack))
if (length(missing) > 0) {
  stop(sprintf("stack.yml : clés manquantes : %s", paste(missing, collapse = ", ")))
}

# --- Récupération des paramètres globaux (avant resolve_builds) --------

packages   <- paste(stack$settings$packages, collapse = ",")
local_root <- stack$settings$local_root
base_url   <- if (!is.null(stack$settings$base_url)) stack$settings$base_url else ""

# --- Résolution profils × snapshots ------------------------------------

source(file.path(Sys.getenv("CRANDORE_SCRIPT_DIR", "/script"), "stack_utils.R"))

builds <- resolve_builds(stack, only_date, only_profile, packages)

if (length(builds) == 0) {
  message("Aucun build à exécuter (vérifier CRANDORE_ONLY_DATE / CRANDORE_ONLY_PROFILE).")
  quit(status = 0)
}

# --- Résumé avant exécution --------------------------------------------

message(sprintf("\n=== Crandore Stack : %d build(s) à exécuter ===\n", length(builds)))
for (b in builds) {
  target     <- if (b$os == "linux") sprintf("%s/%s", b$distro, b$arch) else "windows/x86_64"
  mode_label <- if (b$full_snapshot) "FULL CRAN" else b$packages
  message(sprintf("  [%s] %s | %s | R %s | %s",
                  b$profile, b$date, target, b$r_version, mode_label))
}
message("")

# --- Exécution ---------------------------------------------------------

failed <- character()

for (i in seq_along(builds)) {
  b      <- builds[[i]]
  target <- if (b$os == "linux") sprintf("%s/%s", b$distro, b$arch) else "windows/x86_64"

  mode_label <- if (b$full_snapshot) "FULL" else sprintf("packages: %s", b$packages)
  message(sprintf("\n[%d/%d] %s | %s | R %s | %s",
                  i, length(builds), b$date, target, b$r_version, mode_label))

  tryCatch({
    crandore(
      os            = b$os,
      distro        = b$distro,
      arch          = b$arch,
      snapshot_date = b$date,
      r_version     = b$r_version,
      packages      = if (b$full_snapshot) "" else b$packages,
      full_snapshot = b$full_snapshot,
      local_root    = local_root,
      base_url      = if (nzchar(base_url)) base_url else "https://packagemanager.posit.co/cran"
    )
  }, error = function(e) {
    message(sprintf("ERREUR sur le build [%s | %s | R %s] : %s",
                    b$date, target, b$r_version, conditionMessage(e)))
    failed <<- c(failed, sprintf("%s|%s|R%s", b$date, target, b$r_version))
  })
}

# --- Bilan -------------------------------------------------------------

message(sprintf("\n=== Stack terminée : %d/%d builds réussis ===",
                length(builds) - length(failed), length(builds)))

if (length(failed) > 0) {
  message("Builds en échec :")
  for (f in failed) message(sprintf("  - %s", f))
  quit(status = 1)
}
