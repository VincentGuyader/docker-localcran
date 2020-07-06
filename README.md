# docker-localcran
use miniCRAN to localy create a CRAN repo snapshot using Docker

Using this Docker container you will be able to create a local CRAN snapshot

the Container is "smart" and only needed pacakges and dependencies will be downloaded ( ie : you can stop and rerun the process without loosing time)


# Build the image 

```
docker build -t minicran .
```

This docker containe use by default the MRAN as repos see : <https://mran.microsoft.com/timemachine>.
Use `R_VERSION_DATE="2019-12-31"` to fix the snapshot date, you can also pass the full repos link with `CRAN_MIRROR=https://cran.rstudio.com/`

# Full snapshot (Download all available R package in the repos )

```
docker run -v local_path_to/miniCRAN:/miniCRAN -e R_VERSION_DATE="2020-07-05" -e FULL_SNAPSHOT=true minicran
```

# Partial snapshot

```
docker run -v local_path_to/miniCRAN:/miniCRAN -e R_VERSION_DATE="2020-07-05" --env-file=packages.list minicran
```

`packages.list` is a file containing the list of packages to download : 

```
PACKAGE_TO_DL=golem,rusk,tidyverse
```
