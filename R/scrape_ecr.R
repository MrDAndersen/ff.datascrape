#' Scrape FantasyPro rankings
#'
#' Use this function to scrape Expert Consensus Rankings (ECR) from FantasyPros
#' @param rank_period Specify the period rankings are being scraped for. Should
#' be one of \code{c("draft", "weekly", "ros", "dynasty")}. If omitted then draft
#' rankings will be scraped
#' @param position The position that rankings should be scraped for. Should be one
#' of \code{c("Overall", "QB", "RB", "WR", "TE", "K", "FLEX", "DST", "IDP", "DL", "LB", "DB")}.
#' If not specified then Overall rankings will be scraped.
#' @param rank_type The type of rankings to be scraped. Should be one of
#' \code{c("Std", "PPR", "Half")}. If not speficied then Standard rankings will be
#' scraped.
#' @export
scrape_ecr <- function(rank_period = c("draft", "weekly", "ros", "dynasty"),
                       position = c("Overall", "QB", "RB", "WR", "TE", "K", "FLEX",
                                    "DST", "IDP", "DL", "LB", "DB"),
                       rank_type = c("Std", "PPR", "Half")){
  rank_period <- match.arg(rank_period)
  position <- match.arg(position)
  rank_type <- match.arg(rank_type)

  if(rank_period == "weekly" & position == "Overall"){
    stop("Overall weekly ranks are not provided", call. = FALSE)
  }

  if(rank_period == "ros" & position == "IDP"){
    stop("Combined IDP ROS ranks are not provided", call. = FALSE)
  }

  ranks_url <- "https://www.fantasypros.com/nfl/rankings/"

  rk_type <- switch(rank_type, "Std" = "", "PPR" = "ppr", "Half" = "half-point-ppr")

  rk_pos <- tolower(position)

  if(rank_period == "draft"){
    if(position == "Overall"){
      rk_php <- ifelse(rank_type == "Std", "consensus-cheatsheets.php",
                       paste0(rk_type, "-cheatsheets.php"))
    } else {
      rk_php <- switch(rank_type,
                       "Std" = paste0(rk_pos, "-cheatsheets.php"),
                       ifelse(position %in% c("RB", "WR", "TE", "FLEX"),
                              paste0(rk_type, "-", rk_pos, "-cheatsheets.php"),
                              paste0(rk_pos, "-cheatsheets.php")))
    }
  } else if(rank_period == "weekly"){
    rk_php <- switch(rank_type,
                     "Std" = paste0(rk_pos, ".php"),
                     ifelse(position %in% c("RB", "WR", "TE", "FLEX"),
                            paste0(rk_type, "-", rk_pos, ".php"),
                            paste0(rk_pos, ".php")))
  } else if(rank_period == "ros"){
    rk_php <- switch(rank_type,
                     "Std" = paste0("ros-", rk_pos, ".php"),
                     ifelse(position %in% c("RB", "WR", "TE", "FLEX", "Overall"),
                            paste0("ros-", rk_type, "-", rk_pos, ".php"),
                            paste0("ros-", rk_pos, ".php")))
  } else {
    rk_php <- paste0("dynasty-", rk_pos, ".php")
  }

  rank_page <- read_html(paste0(ranks_url, rk_php))

  rank_page %>% html_nodes("tr.tier-row") %>% xml_remove()

  rank_page %>% html_nodes("#rank-data tr.static, tr.table-ad") %>% xml_remove()
  rank_tbl <- rank_page %>% html_node("#rank-data") %>% html_table(fill = TRUE) %>%
    repair_names() %>% select(-matches("V[0-9]+|Notes"))

  fp_ids <- rank_page %>% html_nodes("#rank-data tr.player-row td.player-label > a[href*='players'],a[href*='/teams/']") %>%
    html_attr("href") %>% basename() %>% gsub(".php", "", .)

  player_col <- rank_tbl %>% select(matches("\\(Team\\)$|DST$")) %>% unlist(use.names = FALSE)
  rank_tbl <- rank_tbl %>% select(-matches("\\(Team\\)$|^WSI|DST$")) %>%
    add_column(Player = player_col, .before = 1) %>%
    add_column(fantasypro_id = fp_ids, .before = 1) %>%
    extract(Player, c("Player", "Team"), "(.+)\\s([A-Z]{2,}$)")

  if(any(is.na(rank_tbl$Player)))
    rank_tbl$Player[is.na(rank_tbl$Player)] <- player_col[is.na(rank_tbl$Player)]

  if(any(names(rank_tbl) == "Pos"))
    rank_tbl <- rank_tbl %>% extract(Pos, c("Pos", "Pos Rank"), "([A-Z]+)([0-9]+)")

  rank_tbl <- rank_tbl %>% janitor::clean_names()

  if(any(names(rank_tbl) == "fantasypro_id"))
    rank_tbl <- rank_tbl %>% add_column(id = id_col(rank_tbl$fantasypro_id, "fantasypro_id"), .before = 1)

  return(rank_tbl)
}

