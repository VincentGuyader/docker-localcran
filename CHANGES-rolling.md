# Changements `rolling-update.sh` — 2026-04-25

## Avant
- `$REPOS_ROOT/$TODAY/` (un dossier daté par jour, partiel `tidyverse,golem`).
- Symlink `$REPOS_ROOT/latest -> $TODAY`.
- 3 profils : source, linux/jammy, windows.
- `docker restart crandore-hub` pour réindexer.
- Aucun lock → runs concurrents possibles.

## Après
- `$REPOS_ROOT/rolling/` (un dossier persistant, **full CRAN**).
  - `rolling/src/contrib/...` — sources, full
  - `rolling/linux/jammy-x86_64/R-4.5/src/contrib/...` — binaires jammy, full
  - `rolling/linux/noble-x86_64/R-4.5/src/contrib/...` — binaires noble, full
- Symlink `$REPOS_ROOT/latest -> rolling/`.
- 3 profils : source, linux/jammy, linux/noble (**windows retiré du rolling**, à pinner via `stack.yml` si besoin).
- `docker exec crandore-hub crandore-hub -mode index` pour réindexer en place (fallback restart si échec).
- `flock` sur `/tmp/crandore-rolling.lock` → run concurrent → exit 0 silencieux.
- Nouvelle fonction `pick_ppm_date()` : descend de jour en jour si PPM n'a pas encore le snapshot du jour.

## Variables d'env (avec leurs défauts)

| Var | Défaut | Rôle |
|---|---|---|
| `REPOS_ROOT` | `/home/ubuntu/cran-repos` | racine du miroir |
| `STACK_FILE` | `/home/ubuntu/Crandore/stack.yml` | snapshots pinnés (épargnés par la rétention) |
| `RETAIN_DAYS` | `30` | rétention des dossiers datés non pinnés |
| `R_VERSION` | `4.5.0` | version R cible des binaires Linux |
| `DISTROS` | `jammy,noble` | distros Linux à construire |
| `HUB_NAME` | `crandore-hub` | container à réindexer |
| `PPM_BASE_URL` | `https://packagemanager.posit.co/cran` | source des paquets |
| `DEDUP` | `true` | dédup par hardlinks en fin de run (`jdupes -rL`) |
| `LOCK_FILE` | `/tmp/crandore-rolling.lock` | empêche les runs concurrents |

## Dédup par hardlinks (DEDUP=true par défaut)

Étape 7 du script : après le re-index, `jdupes -rL $REPOS_ROOT` fusionne les
fichiers identiques en contenu (taille + hash) en hardlinks. Gain attendu
**~70 %** quand plusieurs snapshots full sont pinnés en plus du rolling.

- Comparaison **par contenu**, jamais par nom : un même nom de tarball peut
  correspondre à 3 contenus différents (source / binaire jammy / binaire
  noble) — `jdupes` les laisse séparés.
- Sous le **même flock** que le run rolling — aucun race possible avec un
  download.
- Désactivable à chaud : `DEDUP=false /home/ubuntu/Crandore/rolling-update.sh`.

### ⚠ Piège backup à connaître

Avec la dédup activée, un `tar -cf` ou `rsync` *naïf* re-duplique les octets
dans l'archive et perd le bénéfice au restore.

| Outil | Commande correcte |
|---|---|
| `rsync` | `rsync -aH source/ dest/`  (le `-H` est crucial) |
| `tar` | `tar --hard-dereference=no -czf …` (défaut GNU tar — ne pas y toucher) |
| `cp` | `cp -al` pour un snapshot logique sans duplication |

Sans ces flags, le backup pèsera ~5× la taille réelle.

## Cron existant

```
0 3 * * * REPOS_ROOT=/home/ubuntu/cran-repos RETAIN_DAYS=30 \
  /home/ubuntu/Crandore/rolling-update.sh >> /home/ubuntu/logs/rolling-update.log 2>&1
```

→ Inchangé. Le nouveau script honore les mêmes variables.

## URL exposées par le hub après run

- `https://hub.thinkr.fr/latest/` — sources, universel
- `https://hub.thinkr.fr/latest/linux/jammy-x86_64/R-4.5/` — binaires jammy
- `https://hub.thinkr.fr/latest/linux/noble-x86_64/R-4.5/` — binaires noble

Côté R :
```r
options(repos = c(CRAN = "https://hub.thinkr.fr/latest/linux/jammy-x86_64/R-4.5"))
install.packages("ggplot2")
```

## Rollback

```bash
cp /home/ubuntu/Crandore/rolling-update.sh.bak-2026-04-25 /home/ubuntu/Crandore/rolling-update.sh
chmod +x /home/ubuntu/Crandore/rolling-update.sh
```
