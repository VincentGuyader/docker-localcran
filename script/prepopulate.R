# ============================================================
# Crandore — pre-populate a target snapshot from existing local
# packages, using MD5 lookup against PACKAGES indexes.
#
# For each package listed in the *remote* PACKAGES of the target
# date/os, look up its MD5sum in the local PACKAGES indexes already
# under <repos_root>. If a match is found, hardlink the local file
# into the target dir — no download, no extra disk.
#
# Crandore is then run in RESUME mode and only downloads the
# remaining (truly new) packages.
#
# Env vars:
#   PREPOPULATE_DATE     YYYY-MM-DD (required)
#   PREPOPULATE_OS       "source" (default) | "linux" | "windows"
#   PREPOPULATE_DISTRO   linux only, e.g. "jammy" / "noble"
#   PREPOPULATE_RVER     "4.5" — required when OS != "source"
#   PREPOPULATE_ARCH     "x86_64" (default)
#   REPOS_ROOT           default "/srv/cran-repos"
#   PPM_BASE_URL         default "https://packagemanager.posit.co/cran"
# ============================================================

suppressPackageStartupMessages({
  library(tools)
})

getenv <- function(k, default = NULL) {
  v <- Sys.getenv(k, unset = "")
  if (identical(v, "")) default else v
}

date_target  <- getenv("PREPOPULATE_DATE")
if (is.null(date_target)) stop("PREPOPULATE_DATE is required (YYYY-MM-DD)")
os_target    <- getenv("PREPOPULATE_OS",    "source")
distro       <- getenv("PREPOPULATE_DISTRO")
rver         <- getenv("PREPOPULATE_RVER")
arch         <- getenv("PREPOPULATE_ARCH",  "x86_64")
repos_root   <- getenv("REPOS_ROOT",        "/srv/cran-repos")
ppm_base_url <- getenv("PPM_BASE_URL",      "https://packagemanager.posit.co/cran")

stopifnot(os_target %in% c("source", "linux", "windows"))
if (os_target != "source" && (is.null(rver) || nchar(rver) == 0)) {
  stop("PREPOPULATE_RVER required when OS != 'source'")
}
if (os_target == "linux" && (is.null(distro) || nchar(distro) == 0)) {
  stop("PREPOPULATE_DISTRO required for linux")
}

# --- Resolve remote contrib URL + local target dir + filename pattern -----
if (os_target == "source") {
  remote_contrib <- sprintf("%s/%s/src/contrib", ppm_base_url, date_target)
  local_target   <- file.path(repos_root, date_target, "src", "contrib")
  ext            <- "tar.gz"
} else if (os_target == "linux") {
  remote_contrib <- sprintf("%s/__linux__/%s/%s/src/contrib", ppm_base_url, distro, date_target)
  local_target   <- file.path(repos_root, date_target, "linux",
                              sprintf("%s-%s", distro, arch),
                              sprintf("R-%s", rver), "src", "contrib")
  ext            <- "tar.gz"
} else {
  remote_contrib <- sprintf("%s/%s/bin/windows/contrib/%s", ppm_base_url, date_target, rver)
  local_target   <- file.path(repos_root, date_target, "windows",
                              sprintf("windows-%s", arch),
                              sprintf("R-%s", rver), "bin", "windows", "contrib", rver)
  ext            <- "zip"
}

dir.create(local_target, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("[prepopulate] target  = %s\n", local_target))
cat(sprintf("[prepopulate] remote  = %s\n", remote_contrib))

# --- Load REMOTE PACKAGES (Package + Version only — PPM does not serve MD5)
# CRAN invariant: a (Package, Version) pair maps to a single canonical
# tarball. PPM re-serves it as-is, never re-packages. So matching by
# (Package, Version) against any local copy is safe.
remote_url <- paste0(remote_contrib, "/PACKAGES")
cat(sprintf("[prepopulate] reading %s\n", remote_url))
remote <- as.data.frame(read.dcf(url(remote_url)), stringsAsFactors = FALSE)
remote <- remote[, c("Package", "Version")]
remote <- remote[!is.na(remote$Package) & !is.na(remote$Version), , drop = FALSE]
cat(sprintf("[prepopulate] remote packages: %d\n", nrow(remote)))

# --- Build LOCAL index : "Package_Version" -> path ------------------------
# Walk every <Package>_<Version>.{tar.gz,zip} under repos_root and remember
# the first match per (Package, Version) tuple.
cat("[prepopulate] indexing local archives…\n")
local_idx <- new.env(hash = TRUE, parent = emptyenv())

# Match files of the form pkg_ver.<ext>. This is faster than read.dcf on
# every PACKAGES file and works even where PACKAGES is missing.
pattern <- if (ext == "zip") "\\.(tar\\.gz|zip)$" else "\\.(tar\\.gz|zip)$"
all_archives <- list.files(repos_root, pattern = pattern,
                           recursive = TRUE, full.names = TRUE)

# Skip the target dir itself so we never match a half-DL'd file as source.
all_archives <- all_archives[!startsWith(all_archives, paste0(local_target, "/"))]

re <- "^(.+)_([^/_]+)\\.(?:tar\\.gz|zip)$"
n_indexed <- 0
for (p in all_archives) {
  bn <- basename(p)
  m <- regmatches(bn, regexec(re, bn, perl = TRUE))[[1]]
  if (length(m) < 3) next
  pkg <- m[2]; ver <- m[3]
  key <- paste0(pkg, "_", ver)
  if (is.null(local_idx[[key]])) {
    local_idx[[key]] <- p
    n_indexed <- n_indexed + 1
  }
}
cat(sprintf("[prepopulate] local index: %d unique (Package,Version) across %d archives\n",
            n_indexed, length(all_archives)))

# --- Match + hardlink ------------------------------------------------------
n_hits  <- 0
n_skipped <- 0
n_already <- 0
n_link_err <- 0

for (i in seq_len(nrow(remote))) {
  key <- paste0(remote$Package[i], "_", remote$Version[i])
  src <- local_idx[[key]]
  if (is.null(src)) {
    n_skipped <- n_skipped + 1
    next
  }
  dst <- file.path(local_target,
                   sprintf("%s_%s.%s", remote$Package[i], remote$Version[i], ext))
  if (file.exists(dst)) {
    n_already <- n_already + 1
    next
  }
  # Only link archives of the matching extension family.
  src_ext <- if (grepl("\\.zip$", src)) "zip" else "tar.gz"
  if (src_ext != ext) {
    n_skipped <- n_skipped + 1
    next
  }
  ok <- tryCatch(file.link(src, dst), error = function(e) FALSE,
                 warning = function(w) FALSE)
  if (isTRUE(ok)) {
    n_hits <- n_hits + 1
  } else {
    n_link_err <- n_link_err + 1
  }
}

cat(sprintf("\n[prepopulate] result\n"))
cat(sprintf("  remote packages         : %d\n", nrow(remote)))
cat(sprintf("  hardlinked from local   : %d\n", n_hits))
cat(sprintf("  already in target       : %d\n", n_already))
cat(sprintf("  not found (will DL)     : %d\n", n_skipped))
cat(sprintf("  link errors             : %d\n", n_link_err))
