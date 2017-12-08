#' @export
get_salaries <- function(site =  c("fanduel", "draftkings", "yahoo", "fantasydraft", "fantasyscore")){
  site <- match.arg(site)
  # url
  
  if(site != "fantasyscore"){
    salary_url <- paste("https://www.fantasypros.com/daily-fantasy/nfl/", 
                        site, "-salary-changes.php")
    if(site == "yahoo")
      site <- "yahoo dfs"
    
    salary_table <-  salary_url %>% read_html() %>% html_node("table") %>% 
      html_table() %>% 
      select(!!!c(Salary = "This Week")) %>%
      extract(Player, c("Player", "Team", "Pos"), 
              "(.+)\\s\\(([A-Z]+)\\s\\-\\s([A-Z]+)\\)$") %>%
      mutate(salary = as.numeric(gsub("[\\$,]","", salary)))
    
    player_ids <- salary_url %>% read_html() %>%
      html_nodes("table a[href *= '/nfl/players/']") %>% html_attr("href") %>%
      basename() %>% gsub(".php", "", .)
    
    salary_table <- salary_table %>% add_column(id = player_ids, .before = 1)
    
  } else {
    salary_url <- "https://www.rtsports.com/daily/salaries/football"
    
    salary_table <- read.csv(salary_url) %>%
      select(!!!c(Player = "NAME", Pos = "POS", Team = "TEAM", Salary = "SALARY"))
  }
  return(salary_table) 
}






