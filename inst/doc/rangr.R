## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(rangr)
library(terra)

lapply(list.files(system.file("extdata", package = "rangr"), pattern = "sim_", 
                  full.names = TRUE), load, envir = globalenv())
sim_data_01$id <- unwrap(sim_data_01$id)
sim_data_02$id <- unwrap(sim_data_02$id)
sim_data_01$K_map <- unwrap(sim_data_01$K_map)
sim_data_02$K_map <- unwrap(sim_data_02$K_map)


## ----diagram, echo = FALSE, out.width = '70%', fig.cap="rangr structure", fig.align='center'----
# knitr::include_graphics("img/rangr_diagram.png")

## ----load_rangr, eval = FALSE-------------------------------------------------
#  install.packages("rangr")
#  library(rangr)

## ----load_raster, eval = FALSE------------------------------------------------
#  install.packages("terra")
#  library(terra)

## ----SP1.0, eval=FALSE, include=FALSE-----------------------------------------
#  #' @srrstats {SP1.0} Specified domain of applicability

## ----help_input_maps, eval=FALSE----------------------------------------------
#  ?n1_small.tif
#  ?K_small.tif

## ----load_input_maps----------------------------------------------------------
n1_small <- rast(system.file("input_maps/n1_small.tif", package = "rangr"))
K_small <-  rast(system.file("input_maps/K_small.tif", package = "rangr"))

## ----print_maps---------------------------------------------------------------
n1_small
K_small

## ----fig.align='center', fig.cap="Input maps", message=FALSE, out.width='70%'----
plot(c(n1_small, K_small))

## ----initialise, eval=FALSE---------------------------------------------------
#  sim_data_01 <- initialise(
#    n1_map = n1_small,
#    K_map = K_small,
#    r = log(2),
#    rate = 1 / 1e3
#  )

## ----class_sim_data_01--------------------------------------------------------
class(sim_data_01)

## ----summary_sim_data_01------------------------------------------------------
summary(sim_data_01)

## ----sim_result_01, eval=FALSE------------------------------------------------
#  sim_result_01 <- sim(obj = sim_data_01, time = 100)

## ----class_sim_result_01------------------------------------------------------
class(sim_result_01)

## ----summary_sim_result_01, warning=FALSE, fig.align='center', message=FALSE, out.width='70%'----
summary(sim_result_01)

## ----warning=FALSE, fig.align='center', fig.cap="Abundances", message=FALSE, out.width='70%'----
plot(sim_result_01,
  time_points = c(1, 10, 25, 50),
  template = sim_data_01$K_map
)

## ----warning=FALSE, fig.align='center', fig.cap="Abundances", message=FALSE, out.width='70%'----
plot(sim_result_01,
  time_points = c(1, 10, 25, 50),
  breaks = seq(0, max(sim_result_01$N_map + 5, na.rm = TRUE), by = 5),
  template = sim_data_01$K_map
)

## ----warning=FALSE, fig.align='center', fig.cap="Abundances", message=FALSE, out.width='70%'----
# raster construction
my_rast <- to_rast(
  sim_result_01,
  time_points = 1:sim_result_01$simulated_time,
  template = sim_data_01$K_map
)

# print raster
my_rast

## ----warning=FALSE, fig.align='center', fig.cap="Abundances", message=FALSE, out.width='70%'----
# plot selected time points
plot(my_rast, c(1, 10, 25, 50))

## ----ve_01--------------------------------------------------------------------
set.seed(123)
sample_01 <- get_observations(
   sim_data_01,
   sim_result_01,
   type = "random_one_layer",
   prop = 0.1
)
str(sample_01)

## ----ve_coords_01-------------------------------------------------------------
unique(sample_01[c("x", "y")])

## ----ve_01_2------------------------------------------------------------------
sample_01 <- get_observations(
   sim_data_01,
   sim_result_01,
   type = "random_one_layer",
   prop = 0.1,
   obs_error = "rlnorm",
   obs_error_param = log(1.2)
)

## ----ve_02--------------------------------------------------------------------
set.seed(123)
sample_02 <- get_observations(
   sim_data_01,
   sim_result_01,
   type = "random_all_layer",
   prop = 0.1,
   obs_error = "rlnorm",
   obs_error_param = log(1.2)
)

## ----ve_coords_02-------------------------------------------------------------
coords_02 <- unique(sample_02[c("x", "y")])
nrow(coords_02)

## ----new_n1_map---------------------------------------------------------------
n1_small_02 <- n1_small
values(n1_small_02) <- (sim_result_01$N_map[, , 100])

## ----summary_K_small_changing-------------------------------------------------
K_small_changing <- rast(system.file("input_maps/K_small_changing.tif", 
                                     package = "rangr"))
K_small_changing

## ----fig.align='center', fig.cap="Input maps", message=FALSE, out.width='70%'----
plot(c(n1_small_02, K_small_changing),
     range = range(values(c(n1_small_02, K_small_changing)), na.rm = TRUE),
     main = c("n1", paste0("K", 1:nlyr(K_small_changing))))

## ----K_get_interpolation------------------------------------------------------
# duplicate 1st layer of K_small_changing
K_small_changing_altered <- c(K_small, K_small_changing)

# interpolate to generate maps for each time step
K_small_changing_interpolated <- K_get_interpolation(
  K_small_changing_altered, 
  K_time_points = c(1, 20, 80, 200))
K_small_changing_interpolated

## ----fig.align='center', fig.cap="Interpolation results", message=FALSE, out.width='90%'----
# visualise results
vis_layers <- c(1, 20, 30, seq(50, 200, by = 20), 200)
plot(subset(K_small_changing_interpolated, subset = vis_layers),
     range = range(values(K_small_changing_interpolated), na.rm = TRUE), 
     main = paste0("K", vis_layers),
)

## ----print_sim_data_01--------------------------------------------------------
sim_data_01

## ----update_sim_data_01, eval=FALSE-------------------------------------------
#  sim_data_02 <- update(sim_data_01,
#    n1_map = K_small,
#    K_map = K_small_changing_interpolated,
#    K_sd = 0.1,
#    r = log(5),
#    r_sd = 0.05,
#    growth = "ricker",
#    A = 0.2,
#    dens_dep = "K",
#    border = "reprising",
#    rate = 1 / 500
#  )

## ----print_sim_data_02--------------------------------------------------------
sim_data_02

## ----sim_result_02, eval=FALSE------------------------------------------------
#  library(parallel)
#  cl <- makeCluster(detectCores() - 2)
#  
#  sim_result_02 <-
#    sim(obj = sim_data_02, time = 200, cl = cl)
#  
#  stopCluster(cl)

## ----summary_sim_result_02, fig.align='center', out.width='70%'---------------
summary(sim_result_02)

## ----warning=FALSE, fig.align='center', fig.cap="Abundances", message=FALSE, out.width='90%'----
plot(sim_result_02,
  time_points = c(1, 10, seq(20, 200, by = 20)),
  breaks = seq(0, max(sim_result_02$N_map + 5, na.rm = TRUE), by = 20),
  template = sim_data_02$K_map
)

## ----ve_obs_points------------------------------------------------------------
set.seed(123)
str(observations_points) # required structure

all_coords <- crds(sim_data_02$K_map)
observations_coords <- 
  all_coords[sample(1:nrow(all_coords), 0.1 * nrow(all_coords)),]
time_steps <- sim_result_02$simulated_time
ncells <- nrow(observations_coords)

points <- data.frame(
    x = rep(observations_coords[, "x"], times = time_steps),
    y = rep(observations_coords[, "y"], times = time_steps),
    time_step = rep(1:time_steps, each = ncells)
  )
str(points)

## ----ve_03--------------------------------------------------------------------
set.seed(123)

sample_03 <- get_observations(
  sim_data_02,
  sim_result_02,
  type = "from_data",
  points = points,
  obs_error = "rlnorm",
  obs_error_param = log(1.2)
)
str(sample_03)

## ----ve_04--------------------------------------------------------------------
set.seed(123)

sample_04 <- get_observations(
  sim_data_02,
  sim_result_02,
  type = "monitoring_based",
  cells_coords = observations_coords,
  obs_error = "rlnorm",
  obs_error_param = log(1.2)
)
str(sample_04)
length(unique(sample_04$obs_id))

## ----ve_04_2------------------------------------------------------------------
set.seed(123)

sample_04 <- get_observations(
  sim_data_02,
  sim_result_02,
  type = "monitoring_based",
  cells_coords = observations_coords,
  obs_error = "rlnorm",
  obs_error_param = log(1.2),
  prob = 0.2
)
str(sample_04)
length(unique(sample_04$obs_id))

