#' Simulation setup
#'
#' This function applies \code{rule} (which takes in parameter settings and outputs
#' a synthetic dataset) and \code{criterion} (which takes a synthetic dataset and outputs
#' the results from estimators) to all the rows of \code{df_param} (which is a matrix
#' that contains different parameter settings for each row). The distinction between
#' \code{rule} and \code{criterion} is only made by the user, as the user can design
#' exactly the same simulation that uses one but not the other.
#'
#' The input to \code{rule} must be a vector (a row from \code{df_param}), while
#' the input to \code{criterion} must be first the output of \code{rule} and second
#' a vector (the same row from \code{df_param}). Both these functions is allowed
#' to output lists.
#'
#' The output to \code{simulator} is a list, one element for each
#' row of \code{df_param}. Each element of the list is typically a list or
#' a matrix. This depends on how the user set up what \code{criterion} returns.
#'
#' The function has a \code{tryCatch} call, so if an error happens, the result for that
#' trial and row of \code{df_param} will be an \code{NA}.
#'
#' The remaining inputs for \code{simulator} are cosmetic.
#'
#' \code{filepath} is a filepath to the temporary save location. If set to not \code{NA},
#' \code{simulator} will save the results of every row of \code{df_param} there
#' as it runs the simulations.
#'
#' @param rule function
#' @param criterion function
#' @param df_param data frame
#' @param ntrials number of trials for each row
#' @param specific_trials vector of integers of specific trials to run
#' @param cores number of cores, where if larger than \code{1}, parallelization occurs
#' @param shuffle_group either \code{NA} or a list where each element of the list
#' is a vector containing unique integers ranging within \code{1:nrow(df_param)}
#' @param chunking_num integer, where if \code{!is.na(chunking_num)}, dictatees how many
#' intermediary files are saved during the simulation
#' @param required_packages packages to load into the multisession if
#' \code{cores>1}
#' @param filepath string
#' @param verbose boolean
#'
#' @return list
#' @export
simulator <- function(rule, criterion, df_param,
                      ntrials = 10, specific_trials = NA, cores = 1,
                      shuffle_group = NA, chunking_num = nrow(df_param),
                      required_packages = NULL,
                      filepath = NA, verbose = T){
  stopifnot(is.data.frame(df_param), is.numeric(cores), cores > 0)

  # construct the scheduler
  df_schedule <- .construct_scheduler(nrow(df_param), ntrials, specific_trials)

  # if shuffling is used, shuffle the scheduler
  if(!all(is.na(shuffle_group))){
    df_schedule <- .shuffle(df_schedule, shuffle_group)
  }

  # create the empty list that we will populate with the results
  res_all <- lapply(1:nrow(df_param), function(i){
    if(!is.na(ntrials)){
      tmp <- vector("list", length = ntrials)
      names(tmp) <- paste0("trial_", 1:ntrials)
      tmp
    } else {
      tmp <- vector("list", length = length(specific_trials))
      names(tmp) <- paste0("trial_", specific_trials)
      tmp
    }
  })

  # create the chunking
  chunking_list <- .split_rows(nrow(df_schedule), chunking_num)

  # function for each trial and row of df_param
  # the random seed is handled by future.seed in future_lapply
  fun <- function(i){
    x <- df_schedule$row[i]
    y <- df_schedule$trial[i]
    set.seed(y)

    tryCatch({
      dat <- rule(df_param[x,])
      start_time <- proc.time()
      res <- criterion(dat, df_param[x,], y)
      end_time <- proc.time()

      res <- list(result = res)
      res$start_time <- start_time
      res$end_time <- end_time
      res
    }, error = function(e){
      NA
    })
  }

  # finally: run the simulations
  if(cores > 1) future::plan(future::multisession, workers = cores)

  for(k in 1:length(chunking_list)){
    if(verbose) cat(paste0("\n", Sys.time(), ": Chunk ", k, " of ", length(chunking_list), " started!\n"))

    if(cores > 1){
      # parallel version
      res_tmp <- future.apply::future_lapply(chunking_list[[k]], function(i){
        fun(i)
      }, future.globals = list(rule = rule, criterion = criterion,
                               df_param = df_param, df_schedule = df_schedule),
      future.packages = required_packages, future.seed = TRUE)

    } else {
      # sequential version
      res_tmp <- lapply(chunking_list[[k]], function(i){
        fun(i)
      })
    }

    # copy the results in
    for(i in 1:length(chunking_list[[k]])){
      x <- df_schedule$row[chunking_list[[k]][i]]
      y <- df_schedule$trial[chunking_list[[k]][i]]

      if(!is.na(ntrials)){
        res_all[[x]][[y]] <- res_tmp[[i]]
      } else {
        res_all[[x]][[which(specific_trials == y)]] <- res_tmp[[i]]
      }
    }

    if(!is.na(filepath)) save(res_all, file = filepath)
  }

  # close the parallel backend
  if(cores > 1) future::plan(future::sequential)

  names(res_all) <- paste0("row_", 1:length(res_all))
  res_all
}

#####################

.construct_scheduler <- function(n, ntrials, specific_trials){
  stopifnot(is.na(ntrials) | all(is.na(specific_trials)))

  if(all(is.na(specific_trials))){
    df_schedule <- as.data.frame(do.call(rbind, lapply(1:n, function(x){
      cbind(x, 1:ntrials)
    })))
  } else {
    df_schedule <- as.data.frame(do.call(rbind, lapply(1:n, function(x){
      cbind(x, specific_trials)
    })))
  }

  colnames(df_schedule) <- c("row", "trial")
  df_schedule
}

.shuffle <- function(df_schedule, shuffle_group){
  stopifnot(is.list(shuffle_group), is.data.frame(df_schedule),
            all(sort(colnames(df_schedule)) == sort(c("row", "trial"))))
  tmp <- unlist(shuffle_group)
  stopifnot(all(tmp %% 1 == 0), all(tmp > 0), all(tmp <= max(df_schedule$row)),
            length(unique(tmp)) == length(tmp))

  for(i in 1:length(shuffle_group)){
    idx <- which(df_schedule$row %in% shuffle_group[[i]])
    df_schedule[idx,] <- df_schedule[idx[order(df_schedule$trial[idx])],]
    trial_vals <- unique(df_schedule$trial[idx])

    for(j in trial_vals){
      idx2 <- intersect(which(df_schedule$trial == j), idx)

      if(length(idx2) > 1){
        df_schedule[idx2,] <- df_schedule[sample(idx2),]
      }
    }
  }

  df_schedule
}

.split_rows <- function(n, chunking_num){
  stopifnot(n >= chunking_num, chunking_num %% 1 == 0, chunking_num > 0)
  if(chunking_num == 1) return(1:n)

  split_idx <- sort(unique(round(seq(0, n, length.out = chunking_num+1))))
  lapply(1:(length(split_idx)-1), function(i){
    (split_idx[i]+1):(split_idx[i+1])
  })
}
