#' Retrieve salaries for a specific site
#'
#' This function retrieves the player salaries for a site.
#' @param site Specifies the site to retrieve salaries from. Should be one of
#' \code{c("fanduel", "draftkings", "yahoo", "fantasydraft", "fantasyscore")}.
#' If ommited then FanDuel salaries will be scraped.
#' @return A \link{data.frame} with salary data.
#' @export
get_salaries <- function(site =  c("fanduel", "draftkings", "yahoo", "fantasydraft", "fantasyscore")){
  site <- match.arg(site)

  if(site != "fantasyscore"){
    salary_url <- paste("https://www.fantasypros.com/daily-fantasy/nfl/",
                        site, "-salary-changes.php")
    if(site == "yahoo")
      site <- "yahoo dfs"

    salary_table <-  salary_url %>% read_html() %>% html_node("table") %>%
      html_table() %>%
      select(Player, Salary = "This Week") %>%
      extract(Player, c("Player", "Team", "Pos"),
              "(.+)\\s\\(([A-Z]+)\\s\\-\\s([A-Z]+)\\)$") %>%
      mutate(Salary = as.numeric(gsub("[\\$,]","", Salary)))

    player_ids <- salary_url %>% read_html() %>%
      html_nodes("table a[href *= '/nfl/players/']") %>% html_attr("href") %>%
      basename() %>% gsub(".php", "", .)

    salary_table <- salary_table %>% add_column(fantasypro_id = player_ids, .before = 1)

    salary_table <- salary_table %>% add_column(id = id_col(salary_table$fantasypro_id, "fantasypro_id"), .before = 1)
  } else {
    salary_url <- "https://www.rtsports.com/daily/salaries/football"

    salary_table <- read.csv(salary_url) %>%
      select(!!!c(Player = "NAME", Pos = "POS", Team = "TEAM", Salary = "SALARY"))
  }

  salary_table <- janitor::clean_names(salary_table)

  return(salary_table)
}






