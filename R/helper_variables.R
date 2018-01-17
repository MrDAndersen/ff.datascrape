yahoo_def <- c("jac" = "30", "bal" = "33", "lar" = "14", "phi" = "21", "det" = "8",
               "lac" = "24", "nor" = "18", "sea" = "26", "chi" = "3",  "car" = "29",
               "pit" = "23", "nwe" = "17",  "kan" = "12", "min" = "16", "dal" = "6",
               "was" = "28", "den" = "7", "ari" = "22", "ten" = "10", "tam" = "27",
               "buf" = "2", "cin" = "4", "atl" = "1", "gnb" = "9", "mia" = "15",
               "ind" = "11", "nyg" = "19",  "hou" = "34", "sfo" = "25", "cle" = "5",
               "nyj" = "20", "oak" = "13")


nflTeam.abb <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
                 "DAL", "DEN", "DET", "GB",  "HOU", "IND", "JAX", "KC",
                 "MIA", "MIN", "NO",  "NE",  "NYG", "NYJ", "PHI", "PIT",
                 "LA",  "SF",  "LAC", "TB",  "TEN", "WAS", "SEA", "OAK")

mflTeam.abb <- gsub("JAX", "JAC", nflTeam.abb)

nflTeam.id <- c("100026", "100001", "100002", "100003", "100004", "100005", "100006", "100007",
                "100008", "100009", "100010", "100011", "100013", "100014", "100015", "100016",
                "100019", "100020", "100022", "100021", "100023", "100024", "100025", "100027",
                "100017", "100029", "100028", "100031", "100012", "100032", "100030", "100018")


nflTeam.city <- c("Arizona",   "Atlanta",       "Baltimore",   "Buffalo",     "Carolina",  "Chicago",      "Cincinnati",   "Cleveland",
                  "Dallas",    "Denver",        "Detroit",     "Green Bay",   "Houston",   "Indianapolis", "Jacksonville", "Kansas City",
                  "Miami",     "Minnesota",     "New Orleans", "New England", "New York",  "New York",     "Philadelphia", "Pittsburgh",
                  "Los Angeles", "San Francisco", "Los Angeles",    "Tampa Bay",  "Tennessee", "Washington",   "Seattle",      "Oakland")


nflTeam.name <- c("Cardinals", "Falcons", "Ravens", "Bills",      "Panthers", "Bears",    "Bengals",  "Browns",
                  "Cowboys",   "Broncos", "Lions",  "Packers",    "Texans",   "Colts",    "Jaguars",  "Chiefs",
                  "Dolphins",  "Vikings", "Saints", "Patriots",   "Giants",   "Jets",     "Eagles",   "Steelers",
                  "Rams",  "49ers",   "Chargers",   "Buccaneers", "Titans",   "Redskins", "Seahawks", "Raiders")



cbs_def <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
             "DET", "GB", "HOU", "IND", "JAC", "KC",  "LAC", "LAR", "MIA", "MIN",
             "NE" , "NO",  "NYG", "NYJ", "OAK", "PHI", "PIT", "SEA", "SF",  "TB",
             "TEN", "WAS")

cbs_def_id <- c("1901", "1902", "1903", "1904", "1905", "1906", "1907", "1930", "1908", "1909",
                "1910", "1911", "1932", "1912", "1913", "1914", "1924", "1923", "1915", "1916",
                "1931", "1917", "1918", "1919", "1920", "1921", "1922", "1926", "1925", "1927",
                "1928", "1929")

name_corrections <- list(
  "Ty Hilton" = "T.Y. Hilton",
  "Timothy Wright" = "Tim Wright",
  "Christopher Ivory" = "Chris Ivory",
  "Domanique Davis" = "Dominique Davis",
  "Ben Watson" = "Benjamin Watson",
  "Stevie Johnson" = "Steve Johnson",
  "Lesean McCoy" = "LeSean McCoy",
  "Luke Wilson" = "Luke Willson",
  "Thaddeus Lewis" = "Thad Lewis",
  "Walter Powell" = "Walt Powell",
  "Wilson VanHooser" = "Wilson Van Hooser",
  "Steve Hauschka" = "Steven Hauschka",
  "Stephen Hauschka" = "Steven Hauschka",
  "Daniel Herron" = "Dan Herron",
  "Robert Housler" = "Rob Housler",
  "Corey Philly Brown" = "Philly Brown",
  "Foswhitt Whittaker" =  "Fozzy Whittaker",
  "CJ Anderson" = "C.J. Anderson",
  "TY Hilton" = "T.Y. Hilton",
  "Boobie Dixon" = "Anthony Dixon",
  "EZ Nwachukwu" = "Uzoma Nwachukwu",
  "Dave Paulson" = "David Paulson",
  "Joe DonDuncan" = "Joe Don Duncan",
  "T Y Hilton" = "T.Y. Hilton",
  "Dqwell Jackson" = "DQwell Jackson",
  "Art Jones" = "Arthur Jones",
  "Navorro Bowman" =  "NaVorro Bowman",
  "Devante Parker" = "DeVante Parker",
  "AJ McCarron" = "A.J. McCarron",
  "TJ Yeldon" = "T.J. Yeldon",
  "CJ Prosise" = "C.J. Prosise",
  "AJ Green" = "A.J. Green",
  "David A. Johnson" = "David Johnson",
  "Adrian L. Peterson" = "Adrian Peterson",
  "Jonathan C. Stewart" = "Jonathan Stewart",
  "Chris D. Johnson" = "Chris Johnson",
  "Austin D. Johnson" = "Austin Johnson",
  "Steve L. Smith" = "Steve Smith",
  "Michael A. Thomas" = "Michael Thomas",
  "Devin A. Smith" = "Devin Smith",
  "Michael D. Thomas" = "Michael Thomas",
  "Robert Kelley" = "Rob Kelley",
  "Fairbairn Ka'imi" = "Ka'imi Fairbairn",
  "Will Lutz" = "Wil Lutz")

pos_corrections = list(Def = "DST", D = "DST", DEF = "DST", "D/ST" = "DST", PK = "K" ,
                       CB = "DB", S = "DB", DE = "DL", DT = "DL")


team_corrections <- list(KCC = "KC", SFO = "SF", TBB = "TB", NEP = "NE", RAM = "LAR",
                         LA = "LAR", SDC = "SD", ARZ = "ARI", NOR = "NO", GBP = "GB",
                         JAX = "JAC", WSH = "WAS", HST = "HOU", CLV = "CLE", BLT = "BAL",
                         NWE = "NE")
