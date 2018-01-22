#' Imputing values for missing data
#'
#' Differnt sources provide different data. The functions below will fill out
#' NA values for some sources with values based on data from other sources
#' @name Impute
NULL

#' @rdname Impute
pass_impute <- function(pass_tbl){
  # If there is a pass average column in the table then we adjust the pass attempt
  # column based on the avereage and yards column if they both have non zero values
  if(any(names(pass_tbl) == "pass_avg")){
    sel <- pass_tbl$pass_yds != 0 & !is.na(pass_tbl$pass_yds) &
      pass_tbl$pass_avg != 0 & !is.na(pass_tbl$pass_avg)
    pass_tbl$pass_att[sel] <- pass_tbl$pass_yds[sel] / pass_tbl$pass_avg[sel]
  }

  # Impute values for NAs based on rates, i.e. pass yds and pass attempts will
  # get imputed values based on the average pass yds per attempt for a player
  # across sources.
  passing <-  pass_tbl %>% val_from_rate(pass_yds,pass_att) %>%
    miss_rate(pass_tbl, pass_yds, pass_att) %>%
    val_from_calc(pass_tbl, pass_comp, pass_att) %>%
    val_from_calc(pass_tbl, pass_int, pass_att) %>%
    val_from_calc(pass_tbl, pass_tds, pass_comp)

  # If there are distance variables in the table, we will figure out the overall
  # distribution an distribute date out by distance
  if(length(select(pass_tbl, matches("tds_[0-9]{2,}$"))) > 0){
    passing <- passing %>%
      dist_rate(pass_tbl, pass_tds, pass_tds_09, pass_tds_1019, pass_tds_2029,
                pass_tds_3039, pass_tds_4049, pass_tds_50)
  }

  if(any(names(pass_tbl) == "pass_1st"))
    passing <-  passing %>% val_from_calc(pass_tbl, pass_1st, pass_comp)

  if(any(names(pass_tbl) == "pass_40_yds"))
    passing <-  passing %>% val_from_calc(pass_tbl, pass_40_yds, pass_comp)

  if(any(names(pass_tbl) == "pass_40_tds"))
    passing <-  passing %>% val_from_calc(pass_tbl, pass_40_tds, pass_tds)

  return(passing)
}

#' @rdname Impute
rush_impute <- function(rush_tbl){
  if(any(names(rush_tbl) == "rush_avg")){
    sel <- rush_tbl$rush_yds != 0 & !is.na(rush_tbl$rush_yds) &
      rush_tbl$rush_avg != 0 & !is.na(rush_tbl$rush_avg)
    rush_tbl$rush_att[sel] <- rush_tbl$rush_yds[sel] / rush_tbl$rush_avg[sel]
  }

  rushing <-  rush_tbl %>% val_from_rate(rush_yds,rush_att) %>%
    miss_rate(rush_tbl, rush_yds, rush_att) %>%
    val_from_calc(rush_tbl, rush_tds, rush_att)

  if(length(select(rush_tbl, matches("tds_[0-9]{2,}$"))) > 0){
    rushing <- rushing %>%
      dist_rate(rush_tbl, rush_tds, rush_tds_09, rush_tds_1019, rush_tds_2029,
                rush_tds_3039, rush_tds_4049, rush_tds_50)
  }

  if(any(names(rush_tbl) == "rush_1st"))
    rushing <-  rushing %>% val_from_calc(rush_tbl, rush_1st, rush_att)

  if(any(names(rush_tbl) == "rush_40_yds"))
    rushing <-  rushing %>% val_from_calc(rush_tbl, rush_40_yds, rush_att)

  if(any(names(rush_tbl) == "rush_40_tds"))
    rushing <-  rushing %>% val_from_calc(rush_tbl, rush_40_tds, rush_tds)

  return(rushing)
}

#' @rdname Impute
rec_impute <- function(rec_tbl){
  if(any(names(rec_tbl) == "rec_avg")){
    sel <- rec_tbl$rec_yds != 0 & !is.na(rec_tbl$rec_yds) &
      rec_tbl$rec_avg != 0 & !is.na(rec_tbl$rec_avg)
    rec_tbl$rec[sel] <- rec_tbl$rec_yds[sel] / rec_tbl$rec_avg[sel]
  }

  receiving <-  rec_tbl %>% val_from_rate(rec_yds,rec) %>%
    miss_rate(rec_tbl, rec_yds, rec) %>%
    val_from_calc(rec_tbl, rec_tds, rec)

  if(length(select(rec_tbl, matches("tds_[0-9]{2,}$"))) > 0){
    receiving <- receiving %>%
      dist_rate(rec_tbl, rec_tds, rec_tds_09, rec_tds_1019, rec_tds_2029,
                rec_tds_3039, rec_tds_4049, rec_tds_50)
  }

  if(any(names(rec_tbl) == "rec_tgt"))
    receiving <-  receiving %>% val_from_calc(rec_tbl, rec_tgt, rec)

  if(any(names(rec_tbl) == "rec_1st"))
    receiving <-  receiving %>% val_from_calc(rec_tbl, rec_1st, rec)

  if(any(names(rec_tbl) == "rec_40_yds"))
    receiving <-  receiving %>% val_from_calc(rec_tbl, rec_40_yds, rec)

  if(any(names(rec_tbl) == "rec_40_tds"))
    receiving <-  receiving %>% val_from_calc(rec_tbl, rec_40_tds, rec_tds)

  return(receiving)
}

#' @rdname Impute
kick_impute <- function(kick_tbl){
  # Checking to see if there is a 10-19 fg column listed in the table. If there
  # is then it is then the value is moved to the 0-19 column and the 10-19 column
  # is removed.
  if("fg_1019" %in% names(kick_tbl)){
    if("fg_0019" %in% names(kick_tbl)){
      kick_tbl <- mutate(kick_tbl, fg_0019 = ifelse(is.na(fg_0019) & !is.na(fg_1019), fg_1019, fg_0019))
    } else {
      kick_tbl <- mutate(kick_tbl, fg_0019 = fg_1019)
    }

    kick_tbl <- select(kick_tbl, -fg_1019)
  }

  # Adding up the field goals by distance and checking to make sure that the
  # field goal column has the total.
  if(all(c("fg_0019", "fg_2029", "fg_3039", "fg_4049", "fg_50") %in% names(kick_tbl))){
    kick_tbl <- kick_tbl %>%
      mutate(fg_tot = sum_columns(kick_tbl, fg_0019, fg_2029, fg_3039, fg_4049, fg_50),
             fg = ifelse(!is.na(fg_tot) & is.na(fg), fg_tot, fg))
  }

  if(all(c("xp", "xp_miss") %in% names(kick_tbl)) & !("xp_att" %in% names(kick_tbl))){
    kick_tbl <- mutate(kick_tbl, xp_att = ifelse(!is.na(xp) & !is.na(xp_miss), xp + xp_miss, as.numeric(NA)))
  }

  if(all(c("fg", "fg_miss") %in% names(kick_tbl)) & !("fg_att" %in% names(kick_tbl))){
    kick_tbl <- mutate(kick_tbl, fg_att = ifelse(!is.na(fg) & !is.na(fg_miss), fg + fg_miss, as.numeric(NA)))
  }

  if(all(c("fg_att", c("xp_att")) %in% names(kick_tbl))){
    kicking <- kick_tbl %>% val_from_rate(fg, fg_att) %>%
      inner_join(val_from_rate(kick_tbl, xp, xp_att), by = c("id", "data_src")) %>%
      mutate(fg_miss = fg_att - fg, xp_miss = xp_att - xp)
  }

  if(all(c("fg_0019", "fg_2029", "fg_3039", "fg_4049", "fg_50") %in% names(kick_tbl))){
    kicking <- kicking %>%
      dist_rate(kick_tbl, fg, fg_0019, fg_2029, fg_3039, fg_4049, fg_50)
  }

  return(kicking)
}

