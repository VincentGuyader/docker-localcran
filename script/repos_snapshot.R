# print(Sys.getenv("R_VERSION_DATE"))
# print(Sys.getenv("FULL_SNAPSHOT"))
# print(Sys.getenv("CRAN_mirror"))
# print(Sys.getenv("R_VERSION"))

if (Sys.getenv("R_VERSION_DATE")==""){
    Sys.setenv("R_VERSION_DATE"= as.character(Sys.Date()) )
}


if (Sys.getenv("R_VERSION")==""){
    Sys.setenv("R_VERSION"= paste(R.Version()$major,R.Version()$minor,sep=".") )
}

if (tolower(Sys.getenv("FULL_SNAPSHOT")) %in% c("","false","f")){
  Sys.setenv("FULL_SNAPSHOT" = FALSE )
} else{
  Sys.setenv("FULL_SNAPSHOT" = TRUE )
  }

if (Sys.getenv("CRAN_mirror")==""){
  Sys.setenv("CRAN_mirror"= paste0('https://mran.microsoft.com/snapshot/',Sys.getenv("R_VERSION_DATE")) )
}


options(repos=Sys.getenv("CRAN_mirror"))
localCRAN <- file.path('/miniCRAN',Sys.getenv("R_VERSION"))
dir.create(localCRAN ,showWarnings = FALSE,recursive = TRUE)

message(paste("R version = ",Sys.getenv("R_VERSION") ))
message(paste("Snapshot Date = ",Sys.getenv("R_VERSION_DATE") ))
message(paste("CRAN repos = ",Sys.getenv("CRAN_mirror") ))
message(paste("Internal folder local CRAN repos = ",localCRAN ))


if (!requireNamespace("miniCRAN")){ install.packages("miniCRAN")}
library(miniCRAN,quietly = TRUE,verbose = FALSE)

les_packages_cible <- les_packages <- NULL
available_packages <-  rownames(available.packages())

message(paste("Number of available packages in repos : ", length(available_packages)))

if ( as.logical(Sys.getenv("FULL_SNAPSHOT")) ){
  
  message("Full Snapshot")
  
  les_packages_cible <- les_packages <- available_packages
}else{
  message("partial snapshot")  
  les_packages <- unique(unlist(strsplit(Sys.getenv("PACKAGE_TO_DL"),split = ",")))
  if (length(les_packages)>0){
  les_packages_cible <- les_packages <- unique(miniCRAN::pkgDep(les_packages))
  }else{
    les_packages_cible <- les_packages <- NULL
  }
}

message(paste("Number of packages asked : ", length(les_packages_cible)))

deja_dl <- unlist(lapply(  strsplit(basename(list.files(path = localCRAN, recursive = TRUE)),split = "_") ,function(.){.[1]}))
deja_dl_current <- intersect(deja_dl,les_packages_cible)
les_packages <- setdiff(les_packages_cible,deja_dl)

message(paste("Number of packages already downloaded (current): ", length(deja_dl_current)))
message(paste("Number of packages already downloaded (total): ", length(deja_dl)))


message(paste0(length(les_packages), " packages to download"))

for ( i in seq_along(les_packages)){
  avancement <- paste(
    round((i-1+length(deja_dl_current))/length(les_packages_cible)
          ,digits = 2)*100," %")
  
  message(paste(les_packages[i], " - ",avancement))
  makeRepo(les_packages[i], path = localCRAN, type = "source", writePACKAGES = FALSE, quiet = TRUE)
  
}

message("DL done")

# on update le depot (on ecrit le fichier PACKAGE)

message("update PACKAGES, PACKAGES.gz and PACKAGES.rds")
tools::write_PACKAGES(dir = file.path(localCRAN,"src/contrib"), type = "source",verbose=TRUE)
message("done")


message("  control")
deja_dl <- unlist(lapply(  strsplit(basename(list.files(path = localCRAN, recursive = TRUE)),split = "_") ,function(.){.[1]}))
missing_packages <-  setdiff(les_packages_cible,deja_dl)

if ( length(missing_packages)> 0){
message(paste("missing packages : "),paste(missing_packages,collapse=", "))
} else {
  message("All pacakges downloaded")
  
}

# on peut verifier que tout est ok en listant les packages disponibles dans le depot local.
#available.packages(repos = paste0("file:///",localCRAN))

# ceci doit retourner TRUE
#print(nrow(available.packages(repos = paste0("file:///",localCRAN))) == nrow(available.packages(repos = paste0('https://mran.microsoft.com/snapshot/',Sys.getenv("R_VERSION_DATE")))))




