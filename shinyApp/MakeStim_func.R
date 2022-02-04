#' Create stimulus timing tsv file per subject per run
#' 
#' @param emerge An R dataframe read from an e-merge csv file
#' @param subid A number.
#' @param sessid A number.
#' @return Function either creates timing files within BIDS, or print an error message.
#' @examples
#' makeStim(emerge=scan1, subid=100005, sessid=64584317)
makeStim <- function(emerge, subid, sessid){
  df <- readSubPerSes(emerge, subid)
  for (run_idx in 1:length(runs)) {
    df_run <- readRunPerSub(df, run_idx)
    if (!is.null(df_run)) {
      timing <- getTimingDF(subid, df_run)
      tsvname <- getOutputName(path = "/Shared/lstrathearn/functional/Mother_Infant/derivatives/heudiconv/BIDS",
                               subid, sessid, run_idx)
      if (!is.null(tsvname)) {
        write.table(timing, sep = "\t", row.names=FALSE, quote = FALSE, file=tsvname)
      }
    }
  }
}

readSubPerSes <- function(emerge, subid){
  require(dplyr)
  # Filter emerge by subject
  df <- emerge %>% filter(Subject == subid)
  # Order runs (column named Session) in chronological order
  df$DateTime <- lubridate::mdy_hm(as.character(df$SessionStartDateTimeUtc))
  df <- df[order(df$DateTime), ]
  run_ele <- unique(df$Session) 
  for (x in seq_along(run_ele)) { 
    df[df$Session == run_ele[x], 'Session'] <- x + 4
  }
  df <- df %>% mutate(Session = Session - 4)
  return(df)
}

readRunPerSub <- function(df, run_idx){
  df_run <- df %>%
    filter(Session == run_idx)
  if (nrow(df_run)==0) {
    print(paste0("No data in the merged scan 1 for subject ", subid,
                 " run number: ", run_idx))
    return(NULL)
  } else {return(df_run)}
}

getOutputName <- function(path, subid, sessid, run_idx){
  runs <- c("run-1", "run-2", "run-3", "run-4")
  tsvname <- Sys.glob(paste0(path, "/sub-", subid,"/ses-", sessid,"/func/", "sub-", subid, "_ses-", sessid,
                             '*', runs[run_idx], "_events.tsv"))
  if (identical(tsvname, character(0))) {
    print(paste0("Cannot find: ", paste0(path, "/sub-", subid,"/ses-", sessid,"/func/", "sub-", subid, "_ses-", sessid,
                                         '*', runs[run_idx], "_events.tsv"), ". Check if there are download/heudiconv errors."))
    return(NULL)
  } else {return(tsvname)}
}

getTimingDF <- function(subid, df_run) {
  df_run <- df_run %>% 
    mutate(trial_time = ifelse(!is.na(CatchSlide.OnsetTime), CatchSlide.OnsetTime,
                               ifelse(!is.na(TargetC.OnsetTime), TargetC.OnsetTime,
                                      ifelse(!is.na(TargetF.OnsetTime), TargetF.OnsetTime, NA)))) %>%
    mutate(onset = (trial_time - WaitScanner.RTTime) / 1000 ) %>%
    mutate(trial_type = ifelse(StimCry %in% cryo , "cryown",
                               ifelse(StimCry %in% cryu , "cryunknown",
                                      ifelse(StimFace %in% ho , "happyown",
                                             ifelse(StimFace %in% hu , "happyunknown",
                                                    ifelse(StimFace %in% so , "sadown",
                                                           ifelse(StimFace %in% su , "sadunknown", 
                                                                  ifelse(StimFace %in% bell, "bell","Fail"))))))))
  # load the excel file with duration, filter by subject
  cry_duration_subject <- cry_duration_excel %>%
    filter(subjid == subid) %>% 
    mutate_if(is.character, as.numeric)
  
  # merge cry and face into 1 column called Stim_All
  df_run <- tidyr::unite(df_run, Stim_All, StimCry:StimFace, sep='')
  # create a new duration column in df_run, set all face duration == 2
  df_run$duration <- ifelse(df_run$Stim_All %in% ho | df_run$Stim_All %in% hu | 
                              df_run$Stim_All %in% so | df_run$Stim_All %in% su | df_run$Stim_All %in% bell,  
                            2, NA )
  # get the order in which each each crying stim was presented, and the corresponding duration
  run_cry_stim <- df_run$Stim_All[df_run$Stim_All %in% cry_stims]
  run_cry_index <- which(df_run$Stim_All %in% cry_stims)
  df_run$duration[run_cry_index] <- unlist(cry_duration_subject[run_cry_stim])
  
  response_time <- rep.int("n/a", 38)
  #   onset <- df_run$onset[2:49]
  #   trial_type <- df_run$trial_type[2:49]
  timing <- data.frame(onset = df_run$onset, duration = df_run$duration, trial_type = df_run$trial_type, response_time)
  return(timing)
}
  




