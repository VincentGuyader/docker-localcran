#!/usr/bin/env bash
# ============================================================
# Rolling full-CRAN mirror — daily incremental refresh.
#
# Produces THREE persistent rolling repos under $REPOS_ROOT/rolling/ :
#   - source       : full CRAN sources, platform-independent
#                    $REPOS_ROOT/rolling/src/contrib/...
#   - linux/jammy  : full CRAN binaries for Ubuntu 22.04, R 4.5
#                    $REPOS_ROOT/rolling/linux/jammy-x86_64/R-4.5/...
#   - linux/noble  : full CRAN binaries for Ubuntu 24.04, R 4.5
#                    $REPOS_ROOT/rolling/linux/noble-x86_64/R-4.5/...
#
# Behaviour:
#   - Uses CRANDORE_RESUME=true so only new/changed packages are downloaded.
#     Previously-downloaded packages stay on disk between runs.
#   - $REPOS_ROOT/latest is a symlink to rolling/, so the hub indexes the
#     three layouts as aliases (is_alias=1) under /latest/ URL paths.
#   - Pinned snapshots (declared in $STACK_FILE) are protected forever.
#     Empty/orphan dated dirs older than RETAIN_DAYS are purged.
#   - flock prevents overlapping runs (a full-CRAN refresh can take longer
#     than the cron interval the first time).
#
# Env:
#   REPOS_ROOT    default /home/ubuntu/cran-repos
#   STACK_FILE    default /home/ubuntu/Crandore/stack.yml   (pinned snapshots)
#   RETAIN_DAYS   default 30
#   R_VERSION     default 4.5.0
#   DISTROS       default "jammy,noble"
#   HUB_NAME      default crandore-hub
#   PPM_BASE_URL  default https://packagemanager.posit.co/cran
#   DEDUP         default true — hardlink-deduplicate cran-repos at the end
#                 (jdupes -rL, content-based, never by name)
set -euo pipefail

REPOS_ROOT=${REPOS_ROOT:-/home/ubuntu/cran-repos}
STACK_FILE=${STACK_FILE:-/home/ubuntu/Crandore/stack.yml}
RETAIN_DAYS=${RETAIN_DAYS:-30}
R_VERSION=${R_VERSION:-4.5.0}
DISTROS=${DISTROS:-jammy,noble}
HUB_NAME=${HUB_NAME:-crandore-hub}
PPM_BASE_URL=${PPM_BASE_URL:-https://packagemanager.posit.co/cran}
DEDUP=${DEDUP:-true}
LOCK_FILE=${LOCK_FILE:-/tmp/crandore-rolling.lock}
TODAY=$(date -u +%F)

log() { echo "[$(date -u +'%F %T')] $*"; }

# --- 0. Lock to prevent overlapping runs ---------------------------------
exec 9>"$LOCK_FILE"
if ! flock -n 9; then
  log "another rolling-update is running; exiting"
  exit 0
fi

# --- 1. PPM snapshot date selection --------------------------------------
# PPM may not yet have indexed today's snapshot (server-side delay). Probe
# /__linux__/jammy/<date>/src/contrib/PACKAGES and step back day by day until
# we find one that exists. Cap at 7 days back.
pick_ppm_date() {
  local d
  for i in 0 1 2 3 4 5 6 7; do
    d=$(date -u -d "$TODAY -$i day" +%F)
    if curl -sf -o /dev/null -I "$PPM_BASE_URL/__linux__/jammy/$d/src/contrib/PACKAGES"; then
      echo "$d"
      return 0
    fi
  done
  return 1
}

PPM_DATE=$(pick_ppm_date) || { log "ERROR: no usable PPM snapshot in last 7 days"; exit 1; }
if [[ "$PPM_DATE" != "$TODAY" ]]; then
  log "today's PPM snapshot ($TODAY) unavailable; using $PPM_DATE"
fi

# --- 2. Build the temporary stack file -----------------------------------
tmpstack=$(mktemp)
trap 'rm -f "$tmpstack"' EXIT

python3 - "$tmpstack" "$PPM_DATE" "$R_VERSION" "$DISTROS" <<'PY'
import sys, yaml
out, date, rver, distros_csv = sys.argv[1:5]
distros = [d.strip() for d in distros_csv.split(",") if d.strip()]

profiles = {
    "rolling_source": {
        "os": "source",
        "full_snapshot": True,
    },
}
for d in distros:
    profiles[f"rolling_linux_{d}"] = {
        "os": "linux",
        "arch": "x86_64",
        "r_version": rver,
        "distros": [d],
        "full_snapshot": True,
    }

doc = {
    "settings": {
        "local_root": "/minicran",
    },
    "profiles": profiles,
    "snapshots": {
        date: {"profiles": list(profiles.keys())},
    },
}
with open(out, "w") as f:
    yaml.safe_dump(doc, f, sort_keys=False)
PY

log "rolling refresh — date=$PPM_DATE, R=$R_VERSION, distros=$DISTROS"
log "stack file:"; sed 's/^/  /' "$tmpstack"

# --- 3. Run the full-CRAN snapshot into the persistent rolling/ dir -------
mkdir -p "$REPOS_ROOT/rolling"
docker run --rm \
  -v "$REPOS_ROOT/rolling:/minicran" \
  -v "$tmpstack:/stack.yml:ro" \
  -e CRANDORE_STACK_FILE=/stack.yml \
  -e CRANDORE_ONLY_DATE="$PPM_DATE" \
  -e CRANDORE_RESUME=true \
  crandore

# --- 4. Update the `latest` alias ----------------------------------------
# rm -f handles both "no symlink yet" and "old symlink to a dated dir".
# We use a relative target so the symlink survives if REPOS_ROOT is moved.
ln -sfn rolling "$REPOS_ROOT/latest"
log "symlink: $REPOS_ROOT/latest -> rolling/"

# --- 5. Retention purge of legacy dated dirs ------------------------------
# Pinned snapshots in stack.yml are spared. Empty rolling artefacts left over
# from previous (broken) runs are purged unconditionally.
pinned=$(python3 -c "
import yaml
with open('$STACK_FILE') as f: s = yaml.safe_load(f) or {}
for k in (s.get('snapshots') or {}): print(k)
")
cutoff=$(date -u -d "$RETAIN_DAYS days ago" +%s)

shopt -s nullglob
for dir in "$REPOS_ROOT"/*/; do
  name=$(basename "$dir")
  [[ "$name" == "rolling" ]] && continue
  [[ "$name" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || continue
  if grep -Fxq "$name" <<<"$pinned"; then
    continue
  fi
  if ! dt=$(date -u -d "$name" +%s 2>/dev/null); then
    continue
  fi
  # Purge unpinned dated dirs that are either old or empty (no PACKAGES).
  empty=true
  if find "$dir" -name PACKAGES -print -quit 2>/dev/null | grep -q .; then
    empty=false
  fi
  if (( dt < cutoff )) || $empty; then
    reason=$([[ $empty == true ]] && echo "empty" || echo ">$RETAIN_DAYS days")
    log "purge $name ($reason)"
    rm -rf "$dir"
  fi
done

# --- 6. Re-index the hub (in-place, no restart) ---------------------------
if docker ps --format '{{.Names}}' | grep -qx "$HUB_NAME"; then
  log "re-indexing hub"
  if docker exec "$HUB_NAME" crandore-hub -mode index \
       -repos /srv/cran-repos \
       -db /var/lib/crandore-hub/hub.db 2>&1 | sed 's/^/  /'; then
    log "hub re-index OK"
  else
    log "hub re-index failed — falling back to restart"
    docker restart "$HUB_NAME" >/dev/null && log "$HUB_NAME restarted"
  fi
else
  log "$HUB_NAME container not running — skipping re-index"
fi

# --- 7. Hardlink-deduplicate the mirror (saves disk across snapshots) -----
# Two `.tar.gz` with the same name but different content (e.g. source vs
# linux-binary tarballs) MUST NOT be merged. jdupes compares by content
# (size + hash), never by name. Safe to run here: we hold the rolling lock
# so no concurrent download is writing into cran-repos.
if [[ "$DEDUP" == "true" ]] && command -v jdupes >/dev/null; then
  before=$(du -sb "$REPOS_ROOT" | cut -f1)
  log "dedup: scanning $REPOS_ROOT (before: $(numfmt --to=iec --suffix=B $before))"
  # -r recursive, -L hardlink duplicates, -q quiet, -X size+ skips empty files
  jdupes -rLq -X 'size+:1' "$REPOS_ROOT" 2>&1 | tail -5 | sed 's/^/  /'
  after=$(du -sb "$REPOS_ROOT" | cut -f1)
  saved=$((before - after))
  log "dedup: after $(numfmt --to=iec --suffix=B $after), saved $(numfmt --to=iec --suffix=B $saved)"
elif [[ "$DEDUP" == "true" ]]; then
  log "dedup: jdupes not installed, skipping (apt-get install jdupes)"
fi

log "done"
