## Author : Simon Moulds
## Date   : August 2015

## Script to reproject GAUL latlong maps to aea

fs <- c("GAUL_India_District_1956",
        "GAUL_India_District",
        "GAUL_India_State",
        "GAUL_India_Country")

for (f in fs) {
    if (file.exists(paste0(mod.path, "/", paste0(f, "_aea.shp")))) {
        system(paste0("rm ", paste0(mod.path, "/", paste0(f, "_aea.*"))))
    }

    system(paste0("ogr2ogr -t_srs '+proj=aea +lat_1=28 +lat_2=12 +lat_0=20 +lon_0=78 +x_0=2000000 +y_0=2000000 +ellps=WGS84 +towgs=0,0,0,0,0,0,0 +units=m' ",
                  paste0(mod.path, "/", paste0(f, "_aea.shp")), " ",
                  paste0(mod.path, "/", paste0(f, "_ll.shp")), " ",
                  "-t_srs '+proj=aea +lat_1=28 +lat_2=12 +lat_0=20 +lon_0=78 +x_0=2000000 +y_0=2000000 +ellps=WGS84 +towgs=0,0,0,0,0,0,0 +units=m'"))

    system(paste0("cp ", paste0(mod.path, "/", paste0(f, "*"), " inst/shapes")))
}

