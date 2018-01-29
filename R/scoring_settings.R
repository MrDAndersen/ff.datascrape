scoring_positions = list(
  pass = c("QB" = "QB"),
  rush = c("QB" = "QB", "RB" ="RB", "WR" = "WR", "TE" = "TE"),
  rec =  c("RB" = "RB", "WR" = "WR", "TE"),
  fumb = c("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"),
  kick = c(K = "K"),
  ret = c("RB" = "RB", "WR" = "WR", "TE" ="TE"),
  idp = c(DL = "DL", LB = "LB", DB = "DB"),
  dst = c(DST = "DST")
)

default_scoring <- list(
  pass = list(
    pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
    pass_int = -3, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
    pass_400_yds = 0, pass_1st = 0, pass_40_tds = 0, pass_tds_09 = 0,
    pass_tds_1019 = 0,  pass_tds_2029 = 0, pass_tds_3039 = 0, pass_tds_4049 = 0,
    pass_tds_50 = 0
  ),
  rush = list(
    allPositions = FALSE,
    rush_yds = 0.1, sacks = 0, rush_att = 0, rush_40_yds = 0, rush_tds = 6,
    two_pts = 2, rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0,
    rush_1st = 0, rush_40_tds = 0, rush_tds_09 = 0, rush_tds_1019 = 0,
    rush_tds_2029 = 0, rush_tds_3039 = 0, rush_tds_4049 = 0, rush_tds_50 = 0
  ),
  rec = list(
    allPositions = FALSE,
    rec = 0, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, return_yds = 0,
    rec_100_yds = 0, rec_150_yds = 0, rec_200_yds = 0, rec_1st = 0, rec_40_tds = 0,
    rec_tds_09 = 0, rec_tds_1019 = 0, rec_tds_2029 = 0, rec_tds_3039 = 0,
    rec_tds_4049 = 0, rec_tds_50 = 0
  ),
  fumb = list(
    allPositions = FALSE,
    fumbles_total = 0,
    fumbles_lost = -3
  ),
  kick = list(
    xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
    fg_50 = 5.0,  fg_miss = 0.0
  ),
  ret = list(
    allPositions = FALSE,
    return_tds = 6, return_yds = 0
  ),
  idp = list(
    allPositions = FALSE,
    idp_solo = 1, idp_ast = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
    idp_fum_Rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
  ),
  dst = list(
    dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sack = 1, dst_td = 6,
    dst_blk = 1.5, dst_return_yds = 0, dst_pts_allow = 0
  )
)


