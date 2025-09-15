

html_site <- list(
  www.cbssports.com = list(
    id_col = "cbs_id",
    table_css = "table.data",
    pid_css = "table a[href *= 'playerpage']",
    rm_elem = list("table.data tr.title",
                   "table.data tr.footer")
  ),
  games.espn.com = list(
    id_col = "espn_id",
    table_css = "#playertable_0",
    pid_css = "table td.playertablePlayerName a.flexpop:first-of-type"
  ),
  fantasydata.com = list(
    id_col = "fantasydata_id",
    table_css = "table"
  ),
  www.fantasypros.com = list(
    id_col = "fantasypro_id",
    table_css = "#data",
    pid_css = "a.player-name"
  ),
  www.fantasysharks.com = list(
    id_col = "id",
    table_css = "#toolData",
    pid_css = "td.playerLink a",
    rm_elem = list("tr.separator",
                   "#toolData tr[valign ='middle']:not(:first-child)",
                   "#toolData tr[height ='20px']")
  ),
  www.fftoday.com = list(
    id_col = "fftoday_id",
    table_css = "table",
    pid_css = "a[href *='stats/players/']"
  ),
  www.fleaflicker.com = list(
    id_col = "fleaflicker_id",
    table_css = "#body-center-main table",
    pid_css = "td[class='player'] a"
  ),
  www.numberfire.com = list(
    id_col = "numfire_id",
    table_css = "table",
    pid_css = "td[class='player'] a"
  ),
  football.fantasysports.yahoo.com = list(
    id_col = "stats_id",
    table_css = "table[class *='Table-interactive']",
    pid_css = "a[href *= 'nfl/players']:not(a[class *='playernote']), a[href *= 'nfl/teams']:not(a[class *='playernote'])"
  )
)

make_df_colnames <- function(tbl){
  rm_txt <- c("DEFENSIVE PLAYERS ", "PLAYERS ", "KICKERS ", "[[:cntrl:]]",
              "Sort", "First:", "Last:", "^Projected ", "\\sWeek [0-9]+",
              "\\sWild Card", "\\sDivisional", "\\sConference",  "\\sSuper Bowl",
              "[^[:alnum:]]$")
  rm_pattern <- paste(rm_txt, collapse = "|")
  cnames <- str_trim(paste(names(tbl), tbl[1,]))
  cnames <- str_trim(gsub(rm_pattern, "", cnames))
  cnames[which(nchar(cnames) == 0)] <- "Z"
  return(make.unique(cnames, sep = ""))
}

scrape_html_data <- function(data_url){
  # Get the host name from the url
  data_host <- parse_url(data_url)$hostname

  # Lookup the css selector for the table. If not found throw an error
  table_css <- html_site[[data_host]][["table_css"]]

  if(is.null(table_css))
    stop("Table selector not defined for ", data_host, call. = FALSE)

  # Lookup the css selector for the players
  pid_css <- html_site[[data_host]][["pid_css"]]

  # Look up elements that needs to be removed
  rm_elem <- html_site[[data_host]][["rm_elem"]]

  # Start an html session
  if(data_host != "www.fantasysharks.com"){
    data_session <- html_session(data_url)
  } else{
    data_session <- data_url
  }

  # Initialize data frame to hold data.
  table_data <- tibble()

  repeat{
    data_page <- read_html(data_session)

    if(length(rm_elem) > 0){
      map(rm_elem, html_nodes, x = data_page) %>% map(xml_remove)
    }

    data_table <- switch(data_host,
                         "www.fftoday.com" = html_nodes(data_page, table_css)[[11]],
                         data_page %>% html_node(table_css)) %>%
      html_table(header = TRUE)

    if(nrow(data_table) == 0)
      break

    if(data_host == "fantasydata.com"){
      names(data_table)[2:length(data_table)] <- data_page %>%
        html_nodes("table tr th a") %>%
        html_attr("href") %>%
        gsub("(^.+','Sort\\$)(.+)('\\))", "\\2", .) %>%
        gsub("Fantasy*", "", ., ignore.case = TRUE)
    } else if(is.na(as.numeric(data_table[1, length(data_table)]))){
      names(data_table) <- make_df_colnames(data_table)

      data_table <- data_table %>% slice(-1)
    } else {
      names(data_table) <- make.unique(names(data_table), sep = "")
    }

    if(any(grepl("^[Zz]", names(data_table)))){
      data_table <- data_table %>% select(-matches("^[Zz]"))
    }

    if(!is.null(pid_css)){
      if(data_host == "games.espn.com"){
        player_ids <- data_page %>% html_nodes(pid_css) %>% html_attr("playerid")
      } else{
        pid_urls <- data_page %>% html_nodes(pid_css) %>% html_attr("href")
        player_ids <- switch(data_host,
                             "www.fantasypros.com" = gsub(".php", "", basename(pid_urls)),
                             "www.numberfire.com"= basename(pid_urls),
                             "football.fantasysports.yahoo.com" = basename(pid_urls),
                             "www.fantasysharks.com" = sapply(pid_urls, function(u)parse_url(u)$query$id, USE.NAMES = FALSE),
                             str_extract(pid_urls, "[0-9]{2,8}")
        )

        if(data_host == "www.fantasysharks.com")
          player_ids <- str_pad(player_ids, 4, "left", "0")
      }

      if(length(player_ids) == nrow(data_table))
        data_table <- data_table %>% add_column(src_id = player_ids, .before = 1)
    } else {
      if(data_host == "fantasydata.com")
        data_table <- rename(data_table, src_id = PlayerID)
    }

    table_data <- bind_rows(table_data, data_table)

    next_url <- data_page %>%
      html_node("a:contains('NEXT'), a:contains('Next')") %>% html_attr("href")

    if(is.na(next_url))
      break

    data_session <- data_session %>% jump_to(next_url)
  }

  if("src_id" %in% names(table_data))
    table_data <- table_data %>% add_column(id = id_col(table_data$src_id, html_site[[data_host]][["id_col"]]), .before = 1)

  return(table_data)
}

ff_clean_names <- function(tbl){
  tbl <- tbl %>% janitor::clean_names() %>%
    clean_format() %>%  type_convert(col_types = cols(id = col_character()))
  if("src_id" %in% names(tbl))
    tbl <- mutate(tbl, src_id = as.character(src_id))
  return(tbl)
}
