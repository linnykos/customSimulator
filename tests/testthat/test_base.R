context("Test base functions")

## .construct_scheduler is correct

test_that(".construct_scheduler works", {
  res <- .construct_scheduler(10, 100, NA)
  expect_true(is.data.frame(res))
  expect_true(all(sort(names(res)) == sort(c("row", "trial"))))
  expect_true(nrow(res) == 10*100)
  expect_true(all(res$row %% 1 == 0))
  expect_true(all(res$row > 0))
  expect_true(max(res$row) == length(unique(res$row)))
  expect_true(all(res$trial %% 1 == 0))
  expect_true(all(res$trial > 0))
  expect_true(max(res$trial) == length(unique(res$trial)))

  for(i in 1:10){
    idx <- which(res$row == i)
    tmp <- res$trial[idx]
    expect_true(length(idx) == 100)
  }

  for(i in 1:100){
    idx <- which(res$trial == i)
    tmp <- res$row[idx]
    expect_true(length(idx) == 10)
  }
})

test_that(".construct_scheduler works with specific trials", {
  res <- .construct_scheduler(10, NA, c(10:15))
  expect_true(is.data.frame(res))
  expect_true(all(sort(names(res)) == sort(c("row", "trial"))))
  expect_true(nrow(res) == 10*6)
  expect_true(all(res$row %% 1 == 0))
  expect_true(all(res$row > 0))
  expect_true(max(res$row) == length(unique(res$row)))
  expect_true(all(res$trial %% 1 == 0))
  expect_true(all(res$trial > 0))
  expect_true(6 == length(unique(res$trial)))

  for(i in 1:10){
    idx <- which(res$row == i)
    tmp <- res$trial[idx]
    expect_true(all(sort(tmp) == 10:15))
  }

  for(i in 10:15){
    idx <- which(res$trial == i)
    tmp <- res$row[idx]
    expect_true(length(idx) == 10)
  }
})

test_that(".construct_scheduler errors appropriately", {
  expect_error(.construct_scheduler(10, 10, c(10:15)))
})

###########################

## .shuffle is correct

test_that(".shuffle works", {
  df_schedule <- .construct_scheduler(10, 20, NA)
  shuffle_group <- list(1:3, 4:6, 7:10)
  res <- .shuffle(df_schedule, shuffle_group)

  for(vec in shuffle_group){
    counter <- 1
    idx <- which(res$row %in% vec)
    expect_true(all(diff(sort(idx)) == 1))
    res2 <- res[idx,]
    for(i in 1:20){
      tmp <- res2$trial[counter:(counter+length(vec)-1)]
      expect_true(length(unique(tmp)) == 1)
      expect_true(all(tmp == i))

      tmp <- res2$row[counter:(counter+length(vec)-1)]
      expect_true(length(tmp) == length(unique(tmp)))

      counter <- counter+length(vec)
    }
  }
})

test_that(".shuffle works with specific trials", {
  df_schedule <- .construct_scheduler(10, NA, c(10:15))
  shuffle_group <- list(1:3, 4:6, 7:10)
  res <- .shuffle(df_schedule, shuffle_group)

  for(vec in shuffle_group){
    counter <- 1
    idx <- which(res$row %in% vec)
    expect_true(all(diff(sort(idx)) == 1))
    res2 <- res[idx,]
    for(i in 10:15){
      tmp <- res2$trial[counter:(counter+length(vec)-1)]
      expect_true(length(unique(tmp)) == 1)
      expect_true(all(tmp == i))

      tmp <- res2$row[counter:(counter+length(vec)-1)]
      expect_true(length(tmp) == length(unique(tmp)))

      counter <- counter+length(vec)
    }
  }
})

########################

## .split_rows is correct

test_that(".split_rows works", {
  res <- .split_rows(150, 10)
  expect_true(is.list(res))
  expect_true(length(res) == 10)
  expect_true(all(sort(unlist(res)) == 1:150))
  expect_true(length(unique(sapply(res, length))) == 1)
})

test_that(".split_rows works when there isn't evenly-split", {
  res <- .split_rows(150, 11)

  expect_true(is.list(res))
  expect_true(length(res) == 11)
  expect_true(all(sort(unlist(res)) == 1:150))

  tmp <- diff(sort(sapply(res, length)))
  expect_true(length(which(tmp == 1)) == 1)
  expect_true(length(which(tmp == 0)) == length(tmp)-1)
})

######################

## simulator is correct

test_that("simulator works without parallelization", {
  generator <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }

  executor <- function(dat, vec, y, ...){
    c(mean(dat), stats::sd(dat))
  }

  len <- 6
  df_param <- as.data.frame(matrix(1:len, nrow = len))

  ntrials <- 10
  res <- simulator(generator, executor, df_param, ntrials = ntrials,
                   specific_trials = NA, cores = 1, shuffle_group = NA,
                   filepath = NA, verbose = F)

  expect_true(is.list(res))
  expect_true(length(res) == nrow(df_param))
  expect_true(all(sort(names(res)) == sort(paste0("row_", 1:nrow(df_param)))))
  expect_true(all(sapply(res, length) == ntrials))
  for(i in 1:length(res)){
    expect_true(all(sort(names(res[[i]])) == sort(paste0("trial_", 1:ntrials))))
    for(j in 1:ntrials){
      expect_true(all(sort(names(res[[i]][[j]])) == sort(c("result", "elapsed_time"))))
      expect_true(length(res[[i]][[j]]$result) == 2)
    }
  }
})

test_that("simulator works with parallelization", {
  generator <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }

  executor <- function(dat, vec, y, ...){
    c(mean(dat), stats::sd(dat))
  }

  len <- 6
  df_param <- as.data.frame(matrix(1:len, nrow = len))

  ntrials <- 10
  res <- simulator(generator, executor, df_param, ntrials = ntrials,
                   specific_trials = NA, cores = 2, shuffle_group = NA,
                   filepath = NA, verbose = F)

  expect_true(is.list(res))
  expect_true(length(res) == nrow(df_param))
  expect_true(all(sort(names(res)) == sort(paste0("row_", 1:nrow(df_param)))))
  expect_true(all(sapply(res, length) == ntrials))
  for(i in 1:length(res)){
    expect_true(all(sort(names(res[[i]])) == sort(paste0("trial_", 1:ntrials))))
    for(j in 1:ntrials){
      expect_true(all(sort(names(res[[i]][[j]])) == sort(c("result", "elapsed_time"))))
      expect_true(length(res[[i]][[j]]$result) == 2)
    }
  }
})

test_that("simulator can be reproducable with parallelization", {
  generator <- function(vec, ...){
    mat <- matrix(rnorm(50^2), 50, 50)
    mat <- crossprod(mat)
    mat
  }

  executor <- function(dat, vec, y, ...){
    tmp <- eigen(dat)
    tmp$values[sample(1:50, size = 1)]
  }

  len <- 6
  df_param <- as.data.frame(matrix(1:len, nrow = len))

  ntrials <- 10
  res1 <- simulator(generator, executor, df_param, ntrials = ntrials,
                   specific_trials = NA, cores = 2, shuffle_group = NA,
                   filepath = NA, verbose = F)
  res2 <- simulator(generator, executor, df_param, ntrials = ntrials,
                    specific_trials = NA, cores = 2, shuffle_group = NA,
                    filepath = NA, verbose = F)
  res3 <- simulator(generator, executor, df_param, ntrials = ntrials,
                    specific_trials = NA, cores = 2, shuffle_group = list(1:6),
                    filepath = NA, verbose = F)

  for(i in 1:6){
    for(j in 1:10){
      expect_true(abs(res1[[i]][[j]]$result - res2[[i]][[j]]$result) <= 1e-6)
      expect_true(abs(res1[[i]][[j]]$result - res3[[i]][[j]]$result) <= 1e-6)
    }
  }

  res1 <- simulator(generator, executor, df_param, ntrials = NA,
                    specific_trials = c(10:14), cores = 2, shuffle_group = NA,
                    filepath = NA, verbose = F)
  res2 <- simulator(generator, executor, df_param, ntrials = NA,
                    specific_trials = c(10:14), cores = 2, shuffle_group = NA,
                    filepath = NA, verbose = F)
  res3 <- simulator(generator, executor, df_param, ntrials = NA,
                    specific_trials = c(10:14), cores = 2, shuffle_group = list(1:6),
                    filepath = NA, verbose = F)

  for(i in 1:6){
    for(j in 1:5){
      expect_true(abs(res1[[i]][[j]]$result - res2[[i]][[j]]$result) <= 1e-6)
      expect_true(abs(res1[[i]][[j]]$result - res3[[i]][[j]]$result) <= 1e-6)
    }
  }
})

test_that("simulation_generator works for errors", {
  generator1 <- function(vec, ...){
    if(vec[1] == 2) stop()
    stats::rnorm(100, mean = vec[1])
  }

  executor <- function(dat, vec, y, ...){
    c(mean(dat), stats::sd(dat))
  }

  len <- 3
  df_param <- as.data.frame(matrix(1:len, nrow = len))

  ntrials <- 4
  res1 <- simulator(generator1, executor, df_param, ntrials = ntrials,
                   specific_trials = NA, cores = 2, shuffle_group = NA,
                   filepath = NA, verbose = F)

  ##

  generator2 <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }
  res2 <- simulator(generator2, executor, df_param, ntrials = ntrials,
                   specific_trials = NA, cores = 2, shuffle_group = NA,
                   filepath = NA, verbose = F)

  for(i in c(1,3)){
    for(j in 1:ntrials){
      expect_true(sum(abs(res1[[i]][[j]]$result - res2[[i]][[j]]$result)) <= 1e-4)
    }
  }

  i <- 2
  for(j in 1:ntrials){
    expect_true(all(is.na(res1[[2]])))
  }
})

test_that("simulation_generator can save", {
  generator <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }

  executor <- function(dat, vec, y, ...){
    c(mean(dat), stats::sd(dat))
  }

  len <- 6
  df_param <- as.data.frame(matrix(1:len, nrow = len))

  ntrials <- 10
  res <- simulator(generator, executor, df_param, ntrials = ntrials,
                   specific_trials = NA, cores = 2, shuffle_group = NA,
                   filepath = "tmp.RData", verbose = F)

  expect_true(is.list(res))
  file.remove("tmp.RData")
})

