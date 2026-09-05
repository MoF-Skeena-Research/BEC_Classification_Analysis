library(sf)
library(rmapshaper)

polys <- st_read("D:/OneDrive - Government of BC/CommonTables/BGC_maps/BGCv13_dissolved.gpkg")

# Make geometries valid
bad <- !st_is_valid(polys)

sum(bad)

#polys <- st_make_valid(polys)


nrow(polys)
sum(st_npoints(polys))
object.size(polys)

# msimplify
# polys_simp <- ms_simplify(
#   polys,
#   keep = 0.1,
#   keep_shapes = TRUE
# )
#st_write(polys_simp, "D:/OneDrive - Government of BC/CommonTables/BGC_maps/BGCv13_simplified.gpkg")

polys_simp2 <- st_simplify(
  polys,
  dTolerance = 100,
  preserveTopology = TRUE
)

st_write(polys_simp2, "D:/OneDrive - Government of BC/CommonTables/BGC_maps/BGCv13_simplified.gpkg")
