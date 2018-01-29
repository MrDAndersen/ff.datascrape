
make_rule_tbl <- function(lst){
  tbl <- as.tibble(lst)
  allPos <- NULL
  if("allPositions" %in% names(tbl)){
    allPos <- tbl$allPositions
    tbl <- tbl %>% select(-allPositions)
  }

  if(is.null(allPos) || !allPos){
    rule_tbl <- tbl %>% gather("data_col", "points")
  } else {
    rule_tbl <- tbl %>% map_if(is.list, as.tibble) %>% bind_rows() %>%
      add_column(position = names(tbl)) %>%
      gather("data_col", "points", -position)
  }
  return(rule_tbl)
}


add_pos_col <- function(dat_tbl, pos_list){
  if("position" %in% names(dat_tbl))
    return(dat_tbl)
  map(pos_list, ~ mutate(dat_tbl, position = .x))
}

add_rule_pos <- function(rule_list, scoring_positions){
  rlist <- map2(scoring_positions[names(rule_list)], rule_list,
       ~ if(!"position" %in% names(.y)){
           map(.x, add_pos_col, dat_tbl = .y) %>% map(`[[`, 1) %>% bind_rows()
       }
  )

  rlist <- Filter(Negate(is.null), rlist)

  rule_list[names(rlist)] <- rlist
  return(rule_list)
}
