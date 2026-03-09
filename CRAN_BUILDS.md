# CRAN_BUILDS.md

Commandes Docker exécutées pour construire les dépôts CRAN sur `https://cran.pim.thinkr.fr`.

Racine locale : `/srv/cran-repos/`

---

## 2026-03-08 — snapshot courant

```bash
# Ubuntu Jammy — R 4.4 — Linux
docker run --rm -v /srv/cran-repos/2026-03-08:/minicran \
  -e CRANDORE_OS=linux -e CRANDORE_DISTRO=jammy -e CRANDORE_ARCH=x86_64 \
  -e CRANDORE_SNAPSHOT_DATE=2026-03-08 -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# Ubuntu Noble — R 4.4 — Linux
docker run --rm -v /srv/cran-repos/2026-03-08:/minicran \
  -e CRANDORE_OS=linux -e CRANDORE_DISTRO=noble -e CRANDORE_ARCH=x86_64 \
  -e CRANDORE_SNAPSHOT_DATE=2026-03-08 -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# CentOS 8 — R 4.4 — Linux
docker run --rm -v /srv/cran-repos/2026-03-08:/minicran \
  -e CRANDORE_OS=linux -e CRANDORE_DISTRO=centos8 -e CRANDORE_ARCH=x86_64 \
  -e CRANDORE_SNAPSHOT_DATE=2026-03-08 -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# Windows — R 4.4
docker run --rm -v /srv/cran-repos/2026-03-08:/minicran \
  -e CRANDORE_OS=windows \
  -e CRANDORE_SNAPSHOT_DATE=2026-03-08 -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# Windows — R 4.5
docker run --rm -v /srv/cran-repos/2026-03-08:/minicran \
  -e CRANDORE_OS=windows \
  -e CRANDORE_SNAPSHOT_DATE=2026-03-08 -e CRANDORE_R_VERSION=4.5.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore
```

---

## 2025-09-08 — snapshot -6 mois

```bash
# Ubuntu Jammy — R 4.4 — Linux
docker run --rm -v /srv/cran-repos/2025-09-08:/minicran \
  -e CRANDORE_OS=linux -e CRANDORE_DISTRO=jammy -e CRANDORE_ARCH=x86_64 \
  -e CRANDORE_SNAPSHOT_DATE=2025-09-08 -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# Ubuntu Noble — R 4.4 — Linux
docker run --rm -v /srv/cran-repos/2025-09-08:/minicran \
  -e CRANDORE_OS=linux -e CRANDORE_DISTRO=noble -e CRANDORE_ARCH=x86_64 \
  -e CRANDORE_SNAPSHOT_DATE=2025-09-08 -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# CentOS 8 — R 4.4 — Linux
docker run --rm -v /srv/cran-repos/2025-09-08:/minicran \
  -e CRANDORE_OS=linux -e CRANDORE_DISTRO=centos8 -e CRANDORE_ARCH=x86_64 \
  -e CRANDORE_SNAPSHOT_DATE=2025-09-08 -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# Windows — R 4.4
docker run --rm -v /srv/cran-repos/2025-09-08:/minicran \
  -e CRANDORE_OS=windows \
  -e CRANDORE_SNAPSHOT_DATE=2025-09-08 -e CRANDORE_R_VERSION=4.4.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# Windows — R 4.5
docker run --rm -v /srv/cran-repos/2025-09-08:/minicran \
  -e CRANDORE_OS=windows \
  -e CRANDORE_SNAPSHOT_DATE=2025-09-08 -e CRANDORE_R_VERSION=4.5.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore
```

---

## 2024-03-08 — snapshot -2 ans

> R 4.5 n'existait pas encore (sorti avril 2025). Noble non plus (sorti avril 2024).
> Seule R 4.3 est pertinente pour cette date.

```bash
# Ubuntu Jammy — R 4.3 — Linux
docker run --rm -v /srv/cran-repos/2024-03-08:/minicran \
  -e CRANDORE_OS=linux -e CRANDORE_DISTRO=jammy -e CRANDORE_ARCH=x86_64 \
  -e CRANDORE_SNAPSHOT_DATE=2024-03-08 -e CRANDORE_R_VERSION=4.3.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# CentOS 8 — R 4.3 — Linux
docker run --rm -v /srv/cran-repos/2024-03-08:/minicran \
  -e CRANDORE_OS=linux -e CRANDORE_DISTRO=centos8 -e CRANDORE_ARCH=x86_64 \
  -e CRANDORE_SNAPSHOT_DATE=2024-03-08 -e CRANDORE_R_VERSION=4.3.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore

# Windows — R 4.3
docker run --rm -v /srv/cran-repos/2024-03-08:/minicran \
  -e CRANDORE_OS=windows \
  -e CRANDORE_SNAPSHOT_DATE=2024-03-08 -e CRANDORE_R_VERSION=4.3.0 \
  -e CRANDORE_PACKAGES=tidyverse crandore
```

---

## URLs résultantes

| Date | OS | Distro / Arch | R | URL repos |
|------|----|---------------|---|-----------|
| 2026-03-08 | Linux | jammy-x86_64 | 4.4 | `https://cran.pim.thinkr.fr/2026-03-08/linux/jammy-x86_64/R-4.4/` |
| 2026-03-08 | Linux | noble-x86_64 | 4.4 | `https://cran.pim.thinkr.fr/2026-03-08/linux/noble-x86_64/R-4.4/` |
| 2026-03-08 | Linux | centos8-x86_64 | 4.4 | `https://cran.pim.thinkr.fr/2026-03-08/linux/centos8-x86_64/R-4.4/` |
| 2026-03-08 | Windows | — | 4.4 | `https://cran.pim.thinkr.fr/2026-03-08/windows/windows-x86_64/R-4.4/` |
| 2026-03-08 | Windows | — | 4.5 | `https://cran.pim.thinkr.fr/2026-03-08/windows/windows-x86_64/R-4.5/` |
| 2025-09-08 | Linux | jammy-x86_64 | 4.4 | `https://cran.pim.thinkr.fr/2025-09-08/linux/jammy-x86_64/R-4.4/` |
| 2025-09-08 | Linux | noble-x86_64 | 4.4 | `https://cran.pim.thinkr.fr/2025-09-08/linux/noble-x86_64/R-4.4/` |
| 2025-09-08 | Linux | centos8-x86_64 | 4.4 | `https://cran.pim.thinkr.fr/2025-09-08/linux/centos8-x86_64/R-4.4/` |
| 2025-09-08 | Windows | — | 4.4 | `https://cran.pim.thinkr.fr/2025-09-08/windows/windows-x86_64/R-4.4/` |
| 2025-09-08 | Windows | — | 4.5 | `https://cran.pim.thinkr.fr/2025-09-08/windows/windows-x86_64/R-4.5/` |
| 2024-03-08 | Linux | jammy-x86_64 | 4.3 | `https://cran.pim.thinkr.fr/2024-03-08/linux/jammy-x86_64/R-4.3/` |
| 2024-03-08 | Linux | centos8-x86_64 | 4.3 | `https://cran.pim.thinkr.fr/2024-03-08/linux/centos8-x86_64/R-4.3/` |
| 2024-03-08 | Windows | — | 4.3 | `https://cran.pim.thinkr.fr/2024-03-08/windows/windows-x86_64/R-4.3/` |

## Notes

- Pour ajouter une nouvelle version R Windows, relancer uniquement la commande Windows avec la nouvelle `CRANDORE_R_VERSION` — le `local_root` commun permet de cohabiter plusieurs versions R dans le même arbre.
- Pour ajouter un paquet, modifier `CRANDORE_PACKAGES` (ex: `tidyverse,shiny`) et relancer toutes les commandes. Le mode resume (`CRANDORE_RESUME=true` par défaut) ne re-télécharge pas ce qui est déjà présent.
- Pour mettre à jour la date courante, créer un nouveau dossier (ex: `/srv/cran-repos/2026-09-08`) et relancer les commandes avec la nouvelle date.
