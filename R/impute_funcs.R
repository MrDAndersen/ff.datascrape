#' Imputing values for missing data
#'
#' Differnt sources provide different data. The functions below will fill out
#' NA values for some sources with values based on data from other sources
#' @name Impute
NULL

#' @rdname Impute
impute_na_off <- function(tbl){
  if(length(tbl) == 0)
    return(tbl)

  type_col <- c(pass = "pass_yds", rush = "rush_yds", rec = "rec_yds")
  stat_type <- names(type_col)[which(type_col %in% names(tbl))]

  names(tbl) <- gsub("(pass|rush|rec)_(.+)", "\\2", names(tbl))

  if("rec" %in% names(tbl))
    tbl <- rename(tbl, att = rec)

  if("avg" %in% names(tbl)){
    tbl$att <- calc_touch_from_avg(tbl$yds, tbl$avg, tbl$att)
  }

  out_df <- tbl %>% val_from_rate(yds, att) %>% miss_rate(tbl, yds, att)

  if("comp" %in% names(tbl)){
    out_df <- out_df %>% val_from_calc(tbl, comp, att) %>%
      val_from_calc(tbl, int, att) %>%
      val_from_calc(tbl, tds, comp)
  } else {
    out_df <- out_df %>% val_from_calc(tbl, tds, att)
  }

  if(any(grepl("tds_[0-9]{2,}$", names(tbl)))){
    out_df <- out_df %>%
      dist_rate(tbl, tds, tds_09, tds_1019, tds_2029, tds_3039, tds_4049, tds_50)
  }

  if(any(names(tbl) == "1st" & stat_type == "pass"))
    out_df <-  out_df %>% val_from_calc(tbl, `1st`, comp)

  if(any(names(tbl) == "1st" & stat_type != "pass"))
    out_df <-  out_df %>% val_from_calc(tbl, `1st`, att)

  if(any(names(tbl) == "40_yds" & stat_type == "pass"))
    out_df <-  out_df %>% val_from_calc(tbl, `40_yds`, comp)

  if(any(names(tbl) == "40_yds" & stat_type != "pass"))
    out_df <-  out_df %>% val_from_calc(tbl, `40_yds`, att)

  if(any(names(tbl) == "40_tds"))
    out_df <-  out_df %>% val_from_calc(tbl, `40_tds`, tds)

 id_pt <- paste(stat_type, "id", sep = "_")
 src_pt <- paste(stat_type, "data_src", sep = "_")
  names(out_df) <- paste(stat_type, names(out_df), sep = "_") %>%
    gsub(id_pt, "id", ., fixed = TRUE) %>%
    gsub(src_pt, "data_src", ., fixed = TRUE) %>%
    gsub("rec_att", "rec", ., fixed = TRUE)

  return(out_df)
}


#' @rdname Impute
kick_impute <- function(kick_tbl){
  kick_cols <- names(kick_tbl)
  kick_dist <- c("fg_0019", "fg_2029", "fg_3039", "fg_4049", "fg_50")

  # Checking to see if there is a 10-19 fg column listed in the table. If there
  # is then it is then the value is moved to the 0-19 column and the 10-19 column
  # is removed.
  if("fg_1019" %in% kick_cols){
    if("fg_0019" %in% kick_cols){
      kick_tbl <- mutate(kick_tbl, fg_0019 = ifelse(is.na(fg_0019) & !is.na(fg_1019), fg_1019, fg_0019))
    } else {
      kick_tbl <- mutate(kick_tbl, fg_0019 = fg_1019)
    }

    kick_tbl <- select(kick_tbl, -fg_1019)
  }

  # Adding up the field goals by distance and checking to make sure that the
  # field goal column has the total, taking into account that some sources
  # does not distribute fg below 39 yards.
  if(all(kick_dist %in% kick_cols)){
    if(any(kick_cols == "fg_0039")){
      kick_tbl <- kick_tbl %>%
        mutate(fg_tot = sum_columns(kick_tbl, fg_0019, fg_2029, fg_0039, fg_3039, fg_4049, fg_50),
               fg = ifelse(!is.na(fg_tot) & is.na(fg), fg_tot, fg))
    } else {
      kick_tbl <- kick_tbl %>%
        mutate(fg_tot = sum_columns(kick_tbl, fg_0019, fg_2029, fg_3039, fg_4049, fg_50),
               fg = ifelse(!is.na(fg_tot) & is.na(fg), fg_tot, fg))
    }
    kick_tbl <- kick_tbl %>% select(-fg_tot)
  } else if(all(c("fg_0039", "fg_4049", "fg_50") %in% kick_cols)){
    kick_tbl <- kick_tbl %>%
      mutate(fg_tot = sum_columns(kick_tbl, fg_0039, fg_4049, fg_50),
             fg = ifelse(!is.na(fg_tot) & is.na(fg), fg_tot, fg)) %>%
      select(-fg_tot)
  }

  # Making sure that attempts are calculated if only made and missed are present
  if(all(c("xp", "xp_miss") %in% kick_cols) & !("xp_att" %in% kick_cols)){
    kick_tbl <- mutate(kick_tbl, xp_att = ifelse(!is.na(xp) & !is.na(xp_miss), xp + xp_miss, as.numeric(NA)))
  }

  if(all(c("fg", "fg_miss") %in% kick_cols) & !("fg_att" %in% kick_cols)){
    kick_tbl <- mutate(kick_tbl, fg_att = ifelse(!is.na(fg) & !is.na(fg_miss), fg + fg_miss, as.numeric(NA)))
  }

  # Imputing values for attempts and made, for both field goals and XP and
  # calulating the missed field goals and XP.
  if(all(c("fg_att", "xp_att") %in% kick_cols)){
    kicking <- kick_tbl %>% val_from_rate(fg, fg_att) %>%
      inner_join(val_from_rate(kick_tbl, xp, xp_att), by = c("id", "data_src")) %>%
      mutate(fg_miss = fg_att - fg, xp_miss = xp_att - xp)
  }

  # Imputing values for field goals by distance.
  if(all(kick_dist %in% kick_cols)){
    if(any(kick_cols == "fg_0039")){
      kick_tbl <- kick_tbl %>%
        mutate(tot_0039 = sum_columns(kick_tbl, fg_0019, fg_2029, fg_3039),
               fg_0039 = ifelse(!is.na(tot_0039) & is.na(fg_0039), tot_0039, fg_0039)) %>%
        select(-tot_0039)

      kicking <- kicking %>%
        dist_rate(kick_tbl, fg_0039, fg_0019, fg_2029, fg_3039) %>%
        dist_rate(kick_tbl, fg, fg_0019, fg_2029, fg_3039, fg_4049, fg_50)
    } else {
      kicking <- kicking %>%
        dist_rate(kick_tbl, fg, fg_0019, fg_2029, fg_3039, fg_4049, fg_50)
    }

    kicking <- kicking %>%
      mutate(fg_miss_0019 = fg_miss * fg_0019 / fg,
             fg_miss_2029 = fg_miss * fg_2029 / fg,
             fg_miss_3039 = fg_miss * fg_3039 / fg,
             fg_miss_4049 = fg_miss * fg_4049 / fg,
             fg_miss_59 = fg_miss * fg_50 / fg
      )
  }

  return(kicking)
}



calc_touch_from_avg <- function(yds, avg, touch){
  sel <- yds != 0 & !is.na(yds) & avg != 0 & !is.na(avg)
  touch[sel] <- yds[sel] / avg[sel]
  touch
}


