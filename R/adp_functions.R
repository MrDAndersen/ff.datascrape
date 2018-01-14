#' Get ADP/AAV data from RTSports
#'
#' This function scrapes ADP or AAV data from RTSports
#' @param aav Indicates whether AAV data is scraped. If set to \code{FALSE} (Default),
#' ADP data is scraped. Set it to \code{TRUE} to scrape AAV data.
#' @return A \link{data.frame} with the results.
#' @export
rts_draft <- function(aav = FALSE){
  draft_url <- "https://www.freedraftguide.com/football/adp-aav-provider.php?NUM=&AAV="
  if(aav)
    draft_url <- paste0(draft_url, "YES")

  draft_col <- "avg"
  names(draft_col) <- ifelse(aav, "aav", "adp")

  rts_adp <- httr::content(httr::GET(draft_url))

  adp_tbl <- bind_rows(lapply(rts_adp$player_list, data.frame, stringsAsFactors = FALSE)) %>%
    rename(rts_id = player_id)

  adp_tbl <- add_column(adp_tbl, id = id_col(adp_tbl$rts_id, "rts_id"), .before = 1) %>%
    rename(!!!draft_col) %>% clean_names()

  if(aav){
    adp_tbl <- mutate(adp_tbl, aav = as.numeric(aav))
  } else {
    adp_tbl <- mutate(adp_tbl, adp = as.numeric(adp))
  }

  return(adp_tbl)
}

#' Get ADP data from CBS
#'
#' This function scrapes ADP data from CBS Sports
#' @return A \link{data.frame} with the results.
#' @export
cbs_draft <- function(){
  draft_url <- "https://www.cbssports.com/fantasy/football/draft/averages?&print_rows=9999"

  draft_pge <- read_html(draft_url)

  draft_pge %>% html_node("table.data tr.title") %>% xml_remove()
  draft_pge %>% html_node("table.data tr.footer") %>% xml_remove()

  cbs_ids <- draft_pge %>% html_nodes("table.data tr td a") %>% html_attr("href") %>%
    str_extract_all(pattern = "[0-9]{3,}") %>% unlist(use.names = FALSE)

  draft_tbl <- draft_pge %>% html_node("table.data") %>% html_table(header = TRUE) %>%
    extract(Player, c("Player", "Pos", "Team"), "(.+)\\,\\s(.+)\\,\\s(.+)") %>%
    extract("HI/LO", c("Best", "Worst"), "([0-9]+)/([0-9]+)") %>%
    rename(adp = "Avg Pos") %>% add_column(cbs_id = cbs_ids, .before = 1)

  draft_tbl <- add_column(draft_tbl, id = id_col(draft_tbl$cbs_id, "cbs_id"), .before = 1) %>%
    clean_names() %>% mutate(adp = as.numeric(adp))

  return(draft_tbl)
}

#' Get ADP/AAV data from ESPN
#'
#' This function scrapes ADP and AAV data from ESPN
#' @return A \link{data.frame} with the results. Contains both ADP and AAV
#' @export
espn_draft <- function(){
  draft_url <- "http://games.espn.com/ffl/livedraftresults?position=ALL"

  draft_pge <- read_html(draft_url)

  draft_pge %>% html_node("table.tableBody tr.tableHead") %>% xml_remove()
  draft_pge %>% html_node("table.tableBody tr.tableSubHead") %>% xml_remove()
  draft_pge %>% html_node("table.tableBody tr.tableBody") %>% xml_remove()

  espn_ids <- draft_pge %>% html_nodes("table.tableBody td a.flexpop") %>%
    html_attr("playerid")

  draft_tbl <- draft_pge %>% html_node("table.tableBody") %>% html_table(header = TRUE) %>%
    clean_names() %>%  rename(adp = "avg_pick", aav = "avg_value") %>%
     extract(player_team, c("player", "team"), "([A-Za-z\\.\\s\\-/\\']+)\\*?\\,?\\s?(.*)") %>%
    add_column(espn_id = espn_ids, .before = 1)

  draft_tbl <- add_column(draft_tbl, id = id_col(draft_tbl$espn_id, "espn_id"), .before = 1) %>%
     mutate(adp = as.numeric(adp), aav = as.numeric(aav))

  return(draft_tbl)
}

#' Get ADP/AAV data from Yahoo
#'
#' This function scrapes ADP or AAV data from Yahoo Sports
#' @param aav Indicates whether AAV data is scraped. If set to \code{FALSE} (Default),
#' ADP data is scraped. Set it to \code{TRUE} to scrape AAV data.
#' @return A \link{data.frame} with the results.
#' @export
yahoo_draft <- function(aav = FALSE){
  draft_type <- ifelse(aav, "AD", "SD")

  draft_url <- sprintf("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=%s&pos=ALL",
                       draft_type)

  draft_col <- ifelse(aav, "Avg Cost",  "Avg Pick")
  names(draft_col) <- ifelse(aav, "aav",  "adp")

  draft_session <- html_session(draft_url)
  yahoo_adp <- data.frame()

  repeat({

    draft_page <- read_html(draft_session)

    draft_tbl <- draft_page %>% html_node("#draftanalysistable") %>% html_table()

    names(draft_tbl) <- gsub("[^[:alnum:]]$", "", names(draft_tbl))

    yahoo_ids <-  draft_session %>%
      html_nodes("a[href *= 'nfl/players']:not(a[class *='playernote']), a[href *= 'nfl/teams']:not(a[class *='playernote'])") %>%
      html_attr("href") %>%
      basename()

    draft_tbl <- draft_tbl %>%
      rename(!!!draft_col) %>% add_column(yahoo_id = yahoo_ids, .before = 1)

    if(any(names(draft_tbl) == "aav"))
      draft_tbl <- draft_tbl %>% mutate(aav = as.numeric(gsub("^\\$", "", aav)))

    yahoo_adp <- bind_rows(yahoo_adp, draft_tbl) %>%
      mutate(yahoo_id = recode(yahoo_id, !!!yahoo_def))

    next_url <- draft_session %>%
      html_node("a:contains('Next')") %>%
      html_attr("href")

    if(is.na(next_url))
      break

    draft_session <- next_url %>% jump_to(x=draft_session, url =.)
  })
  yahoo_adp <- yahoo_adp %>%
    extract(., Name, c("Note", "Player", "Team", "Pos", "Status/Game/Opp"),
            "\\s*(.+Note[s]*)\\s+(.+)\\s([[:alpha:]]{2,3})\\s\\-\\s([[:alpha:]]{1,3},*[[:alpha:]]*)\\s{2,}(.+)") %>%
    select(., -one_of(c("Note", "Status/Game/Opp"))) %>% clean_names() %>%
    add_column(id = id_col(yahoo_adp$yahoo_id, "stats_id"), .before = 1)

  return(yahoo_adp)
}

#' Get ADP/AAV data from NFL
#'
#' This function scrapes ADP or AAV data from NFL
#' @return A \link{data.frame} with the results. Contains both ADP and AAV data
#' @export
nfl_draft <- function(){

  api_url <- "http://api.fantasy.nfl.com/v1/players/userdraftranks?format=json&count=100&offset=0"
  draft_tbl <- data.frame()

  repeat({
    nfl_tbl <- bind_rows(lapply(content(GET(api_url))$players, function(x){
      x <- x[!sapply(x, is.null)]
      x <- lapply(x, as.character)
      data.frame(x,  stringsAsFactors = FALSE)
    }))

    if(nrow(nfl_tbl) == 0)
      break

    nfl_tbl <- nfl_tbl %>% rename(adp = rank, team = teamAbbr) %>%
      add_column(nfl_id = nfl_esbid$nfl_id[match(nfl_tbl$esbid, nfl_esbid$esbid)], .before = 1)

    draft_tbl <- bind_rows(draft_tbl, nfl_tbl)
    api_url <- parse_url(api_url)

    api_qry <- api_url$query
    api_qry$offset <- as.integer(api_qry$offset) + 100

    api_url <- modify_url(api_url, query = api_qry)

  })

  draft_tbl <- draft_tbl %>% rowwise() %>%
    mutate(nfl_id = ifelse(is.na(nfl_id), nflTeam.id[which(nflTeam.abb == team)], nfl_id)) %>%
    as.data.frame()

  draft_tbl <- draft_tbl %>%
    add_column(id = id_col(draft_tbl$nfl_id, "nfl_id"), .before = 1) %>%
    mutate(adp = as.numeric(adp), aav = as.numeric(aav))

  return(draft_tbl)
}


#' Get ADP/AAV data from multple sources
#'
#' This function scrapes ADP or AAV data from multiple sources
#' @param sources Indicates what sources to retrieve data from. Choose from
#' \code{c("RTS", "CBS", "ESPN", "Yahoo", "NFL")}. Multiple sources are allowed.
#' If omitted all sources will be scraped.
#' @param type Specifies what data to collect. Should one of \code{c("ADP", "AAV")}.
#' If omitted then ADP data will be scraped.
#' @return A \link{data.frame} with the results. The player's id from the
#' \code{player_ids} table and a column for each source. The average value is also
#' returned if multiple sources are specified
#' @export
get_adp <- function(sources = c("RTS", "CBS", "ESPN", "Yahoo", "NFL"),
                    type = c("ADP", "AAV")){
  type <- match.arg(type)
  sources <- match.arg(sources, several.ok = TRUE)

  draft_type <- tolower(type)

  if("CBS" %in% sources & type == "AAV")
    sources <- setdiff(sources, "CBS")

  draft_funs <- list(rts = rts_draft, cbs = cbs_draft, espn = espn_draft,
                     yahoo = yahoo_draft, nfl = nfl_draft)

  draft_funs <- draft_funs[tolower(sources)]

  draft_list <- lapply(draft_funs, function(f){
    f_args <- list()

    if(!is.null(formals(f)))
      f_args$aav <- type == "AAV"

    tbl <- do.call(f, args = f_args)
    return(tbl[!is.na(tbl$id),])
  })

  draft_table <- draft_list[[1]][, c("id", draft_type)]

  if(length(draft_funs) > 1)
    for(src in 2:length(draft_funs)){
      adp_suffix <- paste0("_", names(draft_funs)[(src-1):src])

      draft_table <- full_join(draft_table, draft_list[[src]][, c("id", draft_type)],
                               by = "id", suffix = adp_suffix)
    }

  if(any(names(draft_table) == draft_type)){
    draft_col <- draft_type
    names(draft_col) <- paste(draft_type, names(draft_funs)[length(draft_funs)], sep = "_")
    draft_table <- rename(draft_table, !!!draft_col)
  }

  if(length(sources) > 1){
    draft_table <- draft_table %>%
      mutate(adp_avg = select(draft_table, matches(draft_type)) %>% rowMeans(na.rm = TRUE))

    if(type == "ADP"){
      draft_table <-  arrange(draft_table, adp_avg)
    } else {
      draft_table <-  arrange(draft_table, desc(adp_avg))
    }
  }

  names(draft_table) <-  c("id", sources, "Avg")[1:length(draft_table)]

  return(draft_table)
}



