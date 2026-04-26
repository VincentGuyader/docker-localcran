# Crandore — How-to sysadmin

Maintenir une stack minimaliste de miroirs CRAN figés, un dépôt par **patch R × OS**, avec binaires cohérents et interface d'admin via `crandore-hub`.

> Exemples : `data.table` + `shiny` (léger, ~40 Mo par dépôt). Section dédiée plus bas pour mirroirer **tout CRAN**.

---

## 1. Architecture en deux briques

| Brique | Rôle | Où |
|---|---|---|
| **Crandore** (R + Docker) | Construit les dépôts figés à partir de PPM. Lit `stack.yml`, écrit sous `$REPOS_ROOT/<date>/<os>/<distro>-<arch>/R-<M.m>/...` | `/home/ubuntu/Crandore` |
| **crandore-hub** (Go) | Sert les dépôts, les indexe (SQLite + FTS5), expose une UI publique (browse / search) et une admin (rebuild + logs live). Remplace Caddy sur le port 80. | `/home/ubuntu/crandore-hub` |

Arborescence produite :

```
$REPOS_ROOT/
├── 2025-05-15/                      # un dossier par snapshot
│   ├── linux/jammy-x86_64/R-4.5/src/contrib/*.tar.gz + PACKAGES*
│   ├── linux/jammy-x86_64/R-4.4/src/contrib/*.tar.gz + PACKAGES*
│   └── windows/windows-x86_64/R-4.5/bin/windows/contrib/4.5/*.zip + PACKAGES*
├── 2026-04-09/
└── latest -> 2026-04-09             # symlink optionnel, indexé à part
```

---

## 2. Prérequis

- Docker (rootless ou rootful) + `docker compose`
- Ports 80 libre (ou autre, voir `docker-compose.yml` de `crandore-hub`)
- ~500 Mo par dépôt `tidyverse`-like, ~50 Mo pour `data.table + shiny`
- Pour mirror intégral : **~50 Go par (snapshot × R × OS)**

---

## 3. Choisir la bonne date de snapshot pour chaque patch R

**Règle d'or** : la date du snapshot doit être **postérieure** à la date de sortie du patch R visé. PPM ne produit de binaires pour un patch qu'à partir de sa sortie officielle. Si vous figez trop tôt, vous récupérerez des binaires compilés pour le patch *précédent* — ou rien du tout.

Table de repère (patchs R ≥ 4.1) :

| R | Sortie | Première date "sûre" pour snapshot binaire |
|---|---|---|
| 4.1.0 | 2021-05-18 | ≥ 2021-05-25 |
| 4.1.2 | 2021-11-01 | ≥ 2021-11-08 |
| 4.1.3 | 2022-03-10 | ≥ 2022-03-17 |
| 4.2.0 | 2022-04-22 | ≥ 2022-04-29 |
| 4.2.3 | 2023-03-15 | ≥ 2023-03-22 |
| 4.3.0 | 2023-04-21 | ≥ 2023-04-28 |
| 4.3.3 | 2024-02-29 | ≥ 2024-03-07 |
| 4.4.0 | 2024-04-24 | ≥ 2024-05-01 |
| 4.4.3 | 2025-02-28 | ≥ 2025-03-07 |
| 4.5.0 | 2025-04-11 | ≥ 2025-04-18 |

Laisser ~1 semaine après la sortie permet à PPM de pousser les binaires compilés pour le nouveau patch.

Distros Linux supportées par PPM (ne pas cibler avant leur sortie Ubuntu/Debian) :
`jammy` (22.04, avril 2022) · `noble` (24.04, avril 2024) · `bookworm` (Deb 12, juin 2023) · `focal` (20.04) · `centos7/8` · `rhel9/10` · `opensuse156` · `manylinux_2_28`.

---

## 4. Stack minimaliste — `data.table` + `shiny`

### 4.1 `.env`

```bash
cd /home/ubuntu/Crandore
cp .env.example .env
# laisser CRANDORE_REPOS_PATH=/home/ubuntu/cran-repos (pas besoin de sudo)
```

### 4.2 `stack.yml`

Un profil par **patch R × OS**, un snapshot par date. Exemple : R 4.4 et R 4.5 sur Ubuntu Jammy + Windows, figé au 2025-05-15 (>= sortie 4.5.0).

```yaml
settings:
  local_root: /minicran
  packages:
    - data.table
    - shiny

profiles:

  linux_jammy_44:
    os: linux
    arch: x86_64
    r_version: "4.4"
    distros: [jammy]

  linux_jammy_45:
    os: linux
    arch: x86_64
    r_version: "4.5"
    distros: [jammy]

  windows_44:
    os: windows
    r_version: "4.4"

  windows_45:
    os: windows
    r_version: "4.5"

snapshots:
  2025-05-15:
    profiles: [linux_jammy_44, linux_jammy_45, windows_44, windows_45]
```

Générateur équivalent :

```bash
CRANDORE_PACKAGES=data.table,shiny \
CRANDORE_INIT_DATES=2025-05-15 \
CRANDORE_INIT_DISTROS=jammy \
CRANDORE_INIT_R_VERSIONS=4.4,4.5 \
CRANDORE_INIT_WINDOWS=true \
docker compose run --rm crandore-init
```

### 4.3 Build

```bash
docker build -t crandore .
./build-and-serve.sh          # monte $REPOS_ROOT/<date>:/minicran et filtre par date
```

Le script itère sur les dates déclarées dans `DATES=(...)` en tête de `build-and-serve.sh` — éditer ce tableau ou utiliser `rolling-update.sh` (§6).

### 4.4 Servir via `crandore-hub`

```bash
cd /home/ubuntu/crandore-hub
CRANDORE_REPOS_PATH=/home/ubuntu/cran-repos \
CRANDORE_PROJECT_DIR=/home/ubuntu/Crandore \
docker compose up -d --build

# http://<host>/               -> liste des repos indexés
# http://<host>/admin/setup    -> créer le premier admin (1ère visite)
```

### 4.5 Côté client R

```r
# Linux Jammy R 4.5, snapshot 2025-05-15
options(
  repos = c(CRAN = "http://<host>/2025-05-15/linux/jammy-x86_64/R-4.5"),
  HTTPUserAgent = sprintf(
    "R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version$platform, R.version$arch, R.version$os)
  )
)
install.packages(c("data.table", "shiny"))
```

> Le `HTTPUserAgent` est **indispensable sous Linux** pour que PPM/serveur renvoient les binaires précompilés plutôt que des sources à compiler. Sans lui, `install.packages` ré-compile depuis `.tar.gz`.

Windows :

```r
options(repos = c(CRAN = "http://<host>/2025-05-15/windows/windows-x86_64/R-4.5"))
install.packages(c("data.table", "shiny"))
```

Persister dans `~/.Rprofile` (Linux) ou `Documents/.Rprofile` (Windows).

---

## 5. Ajouter un nouveau patch R

Quand un nouveau patch sort (ex: R 4.5.1 le 2025-06-13) :

1. **Attendre ~1 semaine** que PPM produise les binaires.
2. Éditer `stack.yml` : ajouter les profils (`linux_jammy_451`, `windows_451`) et un nouveau snapshot daté >= 2025-06-20.
3. Relancer `./build-and-serve.sh` (les builds déjà présents sont skippés, `CRANDORE_RESUME=true` par défaut).
4. Dans l'admin `crandore-hub` : bouton **Rebuild** (ou attendre la réindexation automatique). Le nouveau repo apparaît sur la home dans la minute.

Vérification rapide de cohérence binaire :

```bash
# doit renvoyer des .tar.gz et non "not found"
curl -sI http://<host>/2025-06-20/linux/jammy-x86_64/R-4.5/src/contrib/data.table_*.tar.gz
```

---

## 6. Rolling snapshot quotidien (full CRAN, persistant)

`rolling-update.sh` rafraîchit chaque jour un dossier **persistant** `$REPOS_ROOT/rolling/` qui contient trois dépôts **full CRAN** :

- `rolling/src/contrib/...` — sources, universel
- `rolling/linux/jammy-x86_64/R-4.5/src/contrib/...` — binaires Ubuntu 22.04
- `rolling/linux/noble-x86_64/R-4.5/src/contrib/...` — binaires Ubuntu 24.04

`CRANDORE_RESUME=true` rend les runs incrémentaux (seuls les nouveaux paquets sont téléchargés). Le symlink `$REPOS_ROOT/latest` pointe sur `rolling/`, donc le hub indexe les trois layouts comme alias (`is_alias=1`) sous `/latest/...`.

```bash
# cron déjà en place
0 3 * * * REPOS_ROOT=/home/ubuntu/cran-repos RETAIN_DAYS=30 \
  /home/ubuntu/Crandore/rolling-update.sh >> /home/ubuntu/logs/rolling-update.log 2>&1
```

| Variable | Défaut | Rôle |
|---|---|---|
| `REPOS_ROOT` | `/home/ubuntu/cran-repos` | racine du miroir |
| `STACK_FILE` | `<dir>/stack.yml` | snapshots pinnés (épargnés par la rétention) |
| `RETAIN_DAYS` | `30` | rétention des dossiers datés non-pinnés |
| `R_VERSION` | `4.5.0` | version R cible des binaires Linux |
| `DISTROS` | `jammy,noble` | distros Linux à construire |
| `HUB_NAME` | `crandore-hub` | container réindexé en place |
| `PPM_BASE_URL` | `https://packagemanager.posit.co/cran` | source des paquets |
| `DEDUP` | `true` | dédup par hardlinks en fin de run (`jdupes`) |
| `LOCK_FILE` | `/tmp/crandore-rolling.lock` | flock anti-chevauchement |

Mécaniques utiles :

- **`pick_ppm_date()`** descend d'un jour à la fois si PPM n'a pas encore indexé le snapshot du jour (cap à 7 jours).
- **`flock`** sur `/tmp/crandore-rolling.lock` empêche les runs concurrents (un full CRAN peut dépasser 24 h au tout premier bootstrap).
- **Re-index en place** via `docker exec crandore-hub crandore-hub -mode index ...` — pas de redémarrage HTTP.
- **Purge** des dossiers datés non-pinnés > `RETAIN_DAYS`, ou des dossiers vides résiduels (runs précédents échoués).

### 6.1 Dédup par hardlinks (`DEDUP=true` par défaut)

À la fin du run, `jdupes -rL $REPOS_ROOT` fusionne les fichiers identiques (même *contenu* : taille + hash) en hardlinks. Gain attendu **~70 %** quand on a plusieurs snapshots full pinnés en plus du rolling.

- Comparaison **par contenu, jamais par nom** : `DBI_1.3.0.tar.gz` source ≠ `DBI_1.3.0.tar.gz` binaire jammy ≠ binaire noble. `jdupes` les laisse séparés (tailles différentes).
- Tourne **sous le même flock** que le rolling → aucun race avec les downloads.
- Désactivable à chaud : `DEDUP=false /home/ubuntu/Crandore/rolling-update.sh`.
- Réversible : `cp --remove-destination <fichier> <fichier.tmp> && mv <fichier.tmp> <fichier>` casse le lien.

⚠ **Backup** : un `tar -cf` ou `rsync` *naïf* ré-duplique les octets dans l'archive. Toujours utiliser :
- `rsync -aH …` (le `-H` préserve les hardlinks)
- `tar --hard-dereference=no …` (défaut sur GNU tar — ne pas y toucher)
- `cp -al` pour faire un snapshot logique sans copie

Sans ces flags, le backup pèsera ~5× la taille réelle du miroir et la dédup sera perdue au restore.

### 6.2 Snapshots pinnés vs rolling

Les snapshots déclarés dans `stack.yml:snapshots` sont **épinglés** (jamais purgés, leurs paquets restent même s'ils sont retirés du PPM). Les dossiers `YYYY-MM-DD` non-pinnés et > `RETAIN_DAYS` sont supprimés à chaque run. Le dossier `rolling/` est persistant et n'entre jamais dans la rétention.

---

## 7. Mirror intégral de CRAN

Même mécanique, drapeau `full_snapshot: true`. Compter ~50 Go / combinaison et plusieurs heures par build.

```yaml
settings:
  local_root: /minicran
  packages: []                    # ignoré quand full_snapshot: true

profiles:
  linux_jammy_45_full:
    os: linux
    arch: x86_64
    r_version: "4.5"
    distros: [jammy]
    full_snapshot: true

  windows_45_full:
    os: windows
    r_version: "4.5"
    full_snapshot: true

snapshots:
  2025-05-15:
    profiles: [linux_jammy_45_full, windows_45_full]
```

Ou via init :

```bash
CRANDORE_FULL_SNAPSHOT=true \
CRANDORE_INIT_DATES=2025-05-15 \
CRANDORE_INIT_DISTROS=jammy \
CRANDORE_INIT_R_VERSIONS=4.5 \
docker compose run --rm crandore-init
```

**Reprendre après coupure** : `CRANDORE_RESUME=true` (défaut) — relancer le build saute les paquets déjà présents. Un mirror intégral est safe à interrompre.

---

## 8. Matrice OS × R recommandée

| OS / distro | R versions à figer | Remarque |
|---|---|---|
| `linux/jammy` (Ubuntu 22.04) | 4.2 → 4.5 | valide pour snapshots ≥ 2022-04-22 |
| `linux/noble` (Ubuntu 24.04) | 4.4 → 4.5 | valide pour snapshots ≥ 2024-04-25 |
| `linux/centos8` / `rhel9` | 4.1 → 4.5 | compat large |
| `windows/x86_64` | 4.1 → 4.5 | binaires PPM disponibles même pour vieux patchs |
| `source` (universel) | — | profil sans distro ni R version, sert de fallback (`os: source`) |

Ne **jamais** cibler une distro avant sa date de sortie (le générateur émet un warning pour `noble` < 2024-04).

---

## 9. Commandes d'exploitation

```bash
# logs hub
docker logs -f crandore-hub

# rebuild ciblé d'une date
DATE=2025-05-15
docker run --rm \
  -v /home/ubuntu/cran-repos/$DATE:/minicran \
  -v /home/ubuntu/Crandore/stack.yml:/stack.yml:ro \
  -e CRANDORE_STACK_FILE=/stack.yml \
  -e CRANDORE_ONLY_DATE=$DATE \
  crandore

# réindexer sans rebuild (one-shot)
docker run --rm \
  -v /home/ubuntu/cran-repos:/srv/cran-repos:ro \
  -v crandore-hub_hub-data:/var/lib/crandore-hub \
  crandore-hub:latest -mode index

# tests du code R
docker compose run --rm crandore-test
```

---

## 10. Pièges récurrents

- **Binaire source sous Linux** : oublier `HTTPUserAgent` → R recompile tout depuis `.tar.gz`. Toujours le mettre dans `.Rprofile`.
- **Patch R trop récent** : snapshot daté la veille/jour même de la sortie → 0 binaires ou binaires de l'ancien patch. Attendre ~7 jours.
- **Distro anachronique** : `noble` avant avril 2024, `jammy` avant avril 2022 — PPM renvoie 404.
- **Port 80 déjà pris** : voir `DEPLOYMENT.md` (Traefik/k3s, nginx-proxy). Alternativement, changer le mapping dans `docker-compose.yml` de `crandore-hub`.
- **`latest` symlink** : `crandore-hub` suit les symlinks de 1er niveau uniquement — `latest -> 2025-05-15` OK, `latest/linux -> …` non.
- **Hardlink + backup naïf** : avec `DEDUP=true` (par défaut), un `tar`/`rsync` sans `-H` re-duplique les octets dans l'archive et perd la dédup au restore. Voir §6.1.
- **`du -sh` sur un seul dossier dédupliqué** : retourne la taille « naïve » et masque le partage avec les autres dépôts. Pour la vraie occupation : `du -sh /home/ubuntu/cran-repos` (vue globale) ou `df -h` (filesystem).
- **Suppression d'un snapshot daté hardlinké** : décrémente seulement le link count. Tant qu'un autre dépôt référence l'inode, l'octet reste sur disque. Pour réellement libérer X GB, il faut purger toutes les références.
