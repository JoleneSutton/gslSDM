#' Make an sdmTMB barrier mesh in the Gulf Region
#'
#' Makes a barrier mesh by applying `sdmTMB::make_mesh` and `sdmTMBextra::add_barrier_mesh`
#' with the `coastline` shapefile from `sglSpatial`. The function `add_barrier_mesh`is applied with the default range_fraction = 0.2.
#' User supplies a data frame with latitude and longitude (epsg:4269).
#' User supplies a cutoff value to indicate the minimum allowed triangle edge length (see sdmTMB documentation).
#' The function projects to coordinate reference system: '+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs'.
#' @param x A data frame with a longitude and latitude (epsg:4269).
#' @param mesh.cutoff The minimum allowed triangle edge length (see sdmTMB documentation).
#' @importFrom sf st_as_sf st_as_sf st_transform st_coordinates
#' @importFrom dplyr select
#' @importFrom sdmTMB make_mesh
#' @importFrom sdmTMBextra add_barrier_mesh
#' @import ggplot2
#' @importFrom terra project ext
#' @import gslSpatial
#' @export
make_barrier_mesh<-function(x,mesh.cutoff){
  DAT<-x

  land<-gslSpatial::get_shapefile('coastline')
  #project to crs NAD83 UTM zone 20N
  #land2<-terra::project(land,'epsg:26920') #units are meters, want km
  land3<-terra::project(land,'+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs')
  crs_utm20N<-'+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs'
  SHAPE.SF<-sf::st_as_sf(land3)

  survey <- as.data.frame(DAT) |> dplyr::select('longitude', 'latitude') |> #, RESP.VAR
    sf::st_as_sf(crs = 4269, coords = c("longitude", "latitude")) |>#not 4326
    sf::st_transform(crs_utm20N)
  surv_utm_coords <- sf::st_coordinates(survey) # just to be sure...
  #DAT$X1000 <- surv_utm_coords[,1] / 1000 #only needed if units were meters
  #DAT$Y1000 <- surv_utm_coords[,2] / 1000 # only needed if units were meters

  # apply make_mesh, and check number of nodes
  mesh.regular <- sdmTMB::make_mesh(DAT, xy_cols = c("X", "Y"), cutoff = mesh.cutoff)
  print(paste0('mesh nodes: ', mesh.regular$mesh$n)) #number of vertices (nodes)
  print(paste0('data points: ', nrow(DAT)))
  print("There should be fewer nodes than data points")

  mesh.barrier <- sdmTMBextra::add_barrier_mesh(
    mesh.regular,
    SHAPE.SF,
    range_fraction = 0.2,#default
    #proj_scaling = 1000, # only needed if original values were meters
    plot = F)

  mesh_df_water <- mesh.barrier$mesh_sf[mesh.barrier$normal_triangles, ]
  mesh_df_land <- mesh.barrier$mesh_sf[mesh.barrier$barrier_triangles, ]

  fig<-ggplot(SHAPE.SF) +
    geom_sf() +
    geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
    geom_sf(data = mesh_df_land, size = 1, colour = "green") +
    coord_sf(xlim = c(terra::ext(survey)[1]*0.9,terra::ext(survey)[2]*1.1),
             ylim=c(terra::ext(survey)[3]*0.99,terra::ext(survey)[4]*1.01))

  print(fig)

  return(mesh.barrier)

}
