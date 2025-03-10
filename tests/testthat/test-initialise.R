test_that("initialise works", {
  test_rast_with_nan <- rast(test_path("fixtures", "test_rast.tif"))
  # reclassify to remove NaNs (that were NAs before saving)
  test_rast <- classify(test_rast_with_nan, cbind(NaN, NA))

  test_rast_res <- test_rast
  test_rast_res <- terra::aggregate(test_rast, fact = c(1,2))

  test_rast_neg <- test_rast
  values(test_rast_neg)[2:3] <- c(-2, -3)


  #' @srrstats {G5.2, G5.2a, G5.2b} tests of errors and warnings (with messages)
  #' @srrstats {G5.8, G5.8c, G5.8d} edge condition tests: unsupported data
  #' types, data outside the scope - negative values in the input maps

  # input maps
  expect_error(
    initialise(
      n1_map = 1,
      K_map = test_rast,
      r = log(1.2)
    ),
    "n1_map does not inherit from class SpatRaster")

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = 1,
      r = log(1.2),

    ),
    "K_map does not inherit from class SpatRaster")


  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast_res,
      r = log(1.2)
    ),
    "number of rows and/or columns do not match")

  # expect_message(
  #   initialise(
  #     n1_map = test_rast,
  #     K_map = test_rast_with_nan,
  #     r = log(1.2),
  #     quiet = FALSE
  #   ),
  #   "NaN values were found in input maps and replaced with NA")
  #
  # expect_message(
  #   initialise(
  #     n1_map = test_rast_with_nan,
  #     K_map = test_rast,
  #     r = log(1.2),
  #     quiet = FALSE
  #   ),
  #   "NaN values were found in input maps and replaced with NA")

  expect_error(
    initialise(
      n1_map = test_rast_neg,
      K_map = test_rast,
      r = log(1.2)
    ),
    "n1_map can contain only non-negative values or NAs (which will be automatically reclassified to NA)", #nolint
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast_neg,
      r = log(1.2)
    ),
    "K_map can contain only non-negative values or NAs (which will be automatically reclassified to NA)", #nolint
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast_res,
      r = log(1.2)
    ),
    "compareGeom")

  # K_sd
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      K_sd = c(1, 2, NA)
    ),
    "length(K_sd) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      K_sd = "1"
    ),
    "K_sd is not a numeric or integer vector",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      K_sd = -1
    ),
    "K_sd not greater than or equal to 0",
    fixed = TRUE)

  # r
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = c(1, 2, NA)
    ),
    "length(r) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = "1"
    ),
    "r is not a numeric or integer vector",
    fixed = TRUE)

  # K_sd
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      r_sd = c(1, 2, NA)
    ),
    "length(r_sd) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      r_sd = "1"
    ),
    "r_sd is not a numeric or integer vector",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      r_sd = -1
    ),
    "r_sd not greater than or equal to 0",
    fixed = TRUE)

  # growth
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      growth = c(1, 2, NA)
    ),
    "length(growth) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      growth = 1
    ),
    "growth is not a character vector",
    fixed = TRUE)

  # A
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      A = c(1, 2, NA)
    ),
    "length(A) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      A = "1"
    ),
    "parameter A can be set either as NA or as a single number",
    fixed = TRUE)

  # dens_dep
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      dens_dep = c("1", "none")
    ))

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      dens_dep = "1"
    ))

  # border
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      border = c("1", "gompertz")
    ))

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      border = "1"
    ))

  # kernel_fun
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      kernel_fun = c(1, 2, NA)
    ),
    "length(kernel_fun) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      kernel_fun = 1
    ),
    "kernel_fun is not a character vector",
    fixed = TRUE)


  # max_dist
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      max_dist = c(1, 2, NA)
    ),
    "length(max_dist) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      max_dist = -5
    ),
    "parameter max_dist can be set either as NA or as a single positive number",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      max_dist = "3"
    ),
    "parameter max_dist can be set either as NA or as a single positive number",
    fixed = TRUE)

  # calculate_dist
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      calculate_dist = c(1, 2, NA)
    ),
    "length(calculate_dist) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      calculate_dist = 1
    ),
    "is.logical(calculate_dist) is not TRUE",
    fixed = TRUE)

  # dlist
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      dlist = c(1, 2, NA)
    ),
    "parameter dlist can be set either as NULL or as a list with integers",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      dlist = NA
    ),
    "parameter dlist can be set either as NULL or as a list with integers",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      dlist = 5
    ),
    "parameter dlist can be set either as NULL or as a list with integers",
    fixed = TRUE)

  # progress_bar
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      progress_bar = c(1, 2, NA)
    ),
    "length(progress_bar) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      progress_bar = 1
    ),
    "is.logical(progress_bar) is not TRUE",
    fixed = TRUE)

  # quiet
  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      quiet = c(1, 2, NA)
    ),
    "length(quiet) not equal to 1",
    fixed = TRUE)

  expect_error(
    initialise(
      n1_map = test_rast,
      K_map = test_rast,
      r = log(1.2),
      quiet = 1
    ),
    "is.logical(quiet) is not TRUE",
    fixed = TRUE)
})


test_that("K_n1_map_check works", {
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  test_rast <- classify(test_rast, cbind(NaN, NA))

  test_rast_invalid <- test_rast
  values(test_rast_invalid)[2] <- -1

  expect_error(K_n1_map_check(
    n1_map = test_rast,
    K_map = test_rast_invalid,
    FALSE
  ))
  expect_error(K_n1_map_check(
    n1_map = test_rast_invalid,
    K_map = test_rast,
    FALSE
  ))
  expect_error(K_n1_map_check(
    n1_map = test_rast_invalid,
    K_map = test_rast_invalid,
    FALSE
  ))
})


test_that("K_get_init_values works", {
  test_rast_layer1 <- rast((test_path("fixtures", "test_rast.tif")))
  test_rast_layer1 <- classify(test_rast_layer1, cbind(NaN, NA))

  test_rast_layer2 <- test_rast_layer1 + 6


  test_rast_many_layers <- c(test_rast_layer1, test_rast_layer2)
  test_values_layer1 <- values(test_rast_layer1)

  expect_equal(
    K_get_init_values(test_rast_many_layers, TRUE), test_values_layer1)
})


test_that("target ids precalculation works", {
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  test_rast <- classify(test_rast, cbind(NaN, NA))
  test_resolution <- 1000
  test_max_dist <- 2000
  test_data_table <- readRDS(test_path("fixtures", "test_data_table_mini.rds"))


  test_within_list <- !is.na(test_data_table[, "K"])
  test_id_within <- test_data_table[test_within_list, "id"]
  test_data <- cbind(test_data_table[, c("id", "x", "y")], dist = NA)

  dist_list_res <- readRDS(test_path("fixtures", "test_dlist_mini.rds"))


  test_sim_data <- initialise(
    n1_map = test_rast,
    K_map = test_rast,
    r = log(1.2),
    max_dist = 1000,
    calculate_dist = FALSE
  )

  expect_equal(calc_dist(
    calculate_dist = TRUE,
    id = test_rast,
    data_table = test_data_table,
    id_within = test_id_within,
    max_dist = test_max_dist,
    dist_resolution = test_sim_data$dist_resolution,
    dist_bin = test_sim_data$dist_bin,
    progress_bar = FALSE,
    quiet = TRUE),
  dist_list_res)

  expect_null(calc_dist(
    calculate_dist = FALSE,
    id = test_rast,
    data_table = test_data_table,
    id_within = test_id_within,
    max_dist = test_max_dist,
    dist_resolution = test_sim_data$dist_resolution,
    dist_bin = test_sim_data$dist_bin,
    progress_bar = FALSE,
    quiet = TRUE))

  expect_null(test_sim_data$dlist)

  expect_equal(
    dist_list(
      id = test_rast,
      data_table = test_data_table,
      id_within = test_id_within,
      max_dist = test_max_dist,
      dist_resolution = test_resolution,
      dist_bin = test_sim_data$dist_bin,
      progress_bar = FALSE),
    dist_list_res)

  expect_equal(
    target_ids(1, test_rast, test_data, 1, test_max_dist / test_resolution,
               test_resolution, test_sim_data$dist_bin, test_id_within),
    dist_list_res[[1]])

  expect_equal(
    get_bins(ids = 1,
           ds = 2,
           idx = NULL,
           dist_bin = 2),
    cbind(1, 1:4))

  expect_equal(
    get_bins(ids = 1,
             ds = 2,
             idx = 2,
             dist_bin = 2),
    cbind(c(rep(2, 3), rep(1, 4)),
          c(0:2,1:4)))
})


test_that("ncell_in_circle works", {
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  test_rast <- classify(test_rast, cbind(NaN, NA))
  test_ncells_in_circle <-
    readRDS(test_path("fixtures", "test_ncells_in_circle_mini.rds"))
  test_sim_data_lon_lat_circle <-
    readRDS(test_path("fixtures", "test_sim_data_lon_lat_circle.rds"))
  test_ncells_in_circle_lon_lat <-
    readRDS(test_path("fixtures", "test_ncells_in_circle_lon_lat.rds"))

  expect_equal(ncell_in_circle_planar(test_rast, res(test_rast)[1]), test_ncells_in_circle)

  expect_equal(
  ncell_in_circle_lonlat(terra::unwrap(test_sim_data_lon_lat_circle$id), test_sim_data_lon_lat_circle$dist_resolution, test_sim_data_lon_lat_circle$dist_bin, test_sim_data_lon_lat_circle$id_within, test_sim_data_lon_lat_circle$max_avl_dist, FALSE, TRUE),
  test_ncells_in_circle_lon_lat
  )

})


test_that("get_initialise_call works", {
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  test_rast <- classify(test_rast, cbind(NaN, NA))

  test_sim_data_1 <- initialise(
    n1_map = test_rast,
    K_map = test_rast,
    r = log(1.2),
    max_dist = 1000
  )

  test_sim_data_2 <- initialise(
    n1_map = test_rast,
    K_map = test_rast,
    r = log(1.2),
    dlist = test_sim_data_1$dlist
  )

  expect_equal("dlist" %in% names(test_sim_data_1$call), FALSE)
  expect_equal("dlist" %in% names(test_sim_data_2$call), TRUE)
})


test_that("K_add_stochasticity works", {

  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  test_rast <- classify(test_rast, cbind(NaN, NA))

  test_rast_many_layers <- c(test_rast, test_rast)
  test_sd <- 2

  test_sim_data_1 <- initialise(
    n1_map = test_rast,
    K_map = test_rast,
    K_sd = test_sd,
    r = log(1.2),
    max_dist = 1000
  )

  test_sim_data_2 <- initialise(
    n1_map = test_rast,
    K_map = test_rast_many_layers,
    K_sd = test_sd,
    r = log(1.2),
    max_dist = 1000
  )


  expect_s4_class(test_sim_data_1$K_map, "PackedSpatRaster")
  expect_s4_class(test_sim_data_2$K_map, "PackedSpatRaster")
})


test_that("calculate_dist_params works", {
  test_id_rast_lon_lat <- rast(test_path("fixtures", "test_id_rast_lon_lat.tif"))
  test_data_table_lon_lat <- readRDS(test_path("fixtures", "test_data_table_lon_lat.rds"))

  test_within_list_lon_lat <- !is.na(test_data_table_lon_lat[, "K"])
  test_id_within_lon_lat <- test_data_table_lon_lat[test_within_list_lon_lat, "id"]

  expect_equal(
    calculate_dist_params(id = test_id_rast_lon_lat,
                        id_within = test_id_within_lon_lat,
                        data_table = test_data_tabl_lon_lat,
                        progress_bar = FALSE,
                        quiet = TRUE),
    c(dist_bin = 1, dist_resolution = 4, max_avl_dist = 12)

  )

})
