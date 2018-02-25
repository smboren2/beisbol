# Retrosheet Data Pre-processing

library(dplyr)
library(lubridate)
library(magrittr)
library(data.table)
library(tidyr)

setwd("~/Retrosheet")

# Import Event Data
RetroSample <- read.csv("RetrosheetEV_Sample.csv", stringsAsFactors = FALSE, na.strings = c("", " ", "NA")) 

RetroSample %<>% 
  mutate(DATE = ymd(substr(GAME_ID, 4, 11)),
         DAY_GM_NBR = substr(GAME_ID, 12, 12))
  
# --------------------------------------------------------------------------------------------------

SB_GameLog <- RetroSample %>%
  select(GAME_ID, ends_with("RUN_ID"), ends_with("SB_FL"), ends_with("CS_FL")) %>%
  gather(key = BASE, value = BASE_RUN_ID, BASE1_RUN_ID:BASE3_RUN_ID) %>% 
  gather(key = FLAG_ID, value = RESULT, RUN1_SB_FL:RUN3_CS_FL) %>%
  filter(!is.na(BASE_RUN_ID) & RESULT == TRUE) %>%
  filter(substr(BASE, 5, 5) == substr(FLAG_ID, 4, 4)) %>% # Ensure Same Runner
  mutate(EVENT_TYPE = substr(FLAG_ID, 6,7)) %>%
  group_by(GAME_ID, BASE_RUN_ID, EVENT_TYPE) %>%
  summarize(COUNT = n()) %>%
  ungroup() %>%
  spread(key = EVENT_TYPE, value = COUNT)

# Number of Runs Scored per player per game 
  # --- This will not include runs scored on homers, so homeruns should be added to this value
Runs_GameLog <- RetroSample %>% 
  select(GAME_ID, ends_with("RUN_ID"), ends_with("DEST_ID"), -BAT_DEST_ID) %>% 
  gather(key = BASE, value = BASE_RUN_ID, BASE1_RUN_ID:BASE3_RUN_ID) %>% 
  gather(key = DEST_ID, value = RESULT, RUN1_DEST_ID:RUN3_DEST_ID) %>%
  filter(RESULT %in% c(4, 5, 6) & (substr(BASE, 5, 5) == substr(DEST_ID, 4, 4))) %>%
  group_by(GAME_ID, BASE_RUN_ID) %>%
  summarize(RUNS = n()) %>%
  ungroup()

GameLog <- RetroSample %>%
  filter(BAT_EVENT_FL == T) %>%
  group_by(GAME_ID, BAT_ID) %>%
  summarize(HITS   = sum(H_CD > 0),
            AB     = sum(AB_FL == T),
            PA     = sum(BAT_EVENT_FL == T),
            BB     = sum(EVENT_CD == 14),
            IBB    = sum(EVENT_CD == 15),
            HBP    = sum(EVENT_CD == 16),
            SO     = sum(EVENT_CD == 3),
            B_1B   = sum(EVENT_CD == 20),
            B_2B   = sum(EVENT_CD == 21),
            B_3B   = sum(EVENT_CD == 22),
            B_HR   = sum(EVENT_CD == 23),
            RBI    = sum(RBI_CT),
            SACH   = sum(SH_FL == T),
            SACF   = sum(SF_FL == T),
            GB_CNT = sum(BATTEDBALL_CD == "G" & !is.na(BATTEDBALL_CD)),
            FB_CNT = sum(BATTEDBALL_CD == "F" & !is.na(BATTEDBALL_CD)),
            LD_CNT = sum(BATTEDBALL_CD == "L" & !is.na(BATTEDBALL_CD)),
            PU_CNT = sum(BATTEDBALL_CD == "P" & !is.na(BATTEDBALL_CD)),
            RUN1B  = sum(!is.na(BASE1_RUN_ID)),
            RUN2B  = sum(!is.na(BASE2_RUN_ID)),
            RUN3B  = sum(!is.na(BASE3_RUN_ID)),
            BALLS_CNT    = sum(PA_BALL_CT),
            STRIKES_CNT  = sum(PA_CALLED_STRIKE_CT + PA_SWINGMISS_STRIKE_CT + PA_FOUL_STRIKE_CT)) %>%
  left_join(SB_GameLog, by = c("GAME_ID", "BAT_ID" = "BASE_RUN_ID")) %>% 
  left_join(Runs_GameLog, by = c("GAME_ID", "BAT_ID" = "BASE_RUN_ID")) %>%
  mutate_at(vars(CS:RUNS), .funs = funs(ifelse(is.na(.), 0, .))) %>%
  mutate(RUNS = RUNS + B_HR,
         DK_PTS = (B_1B*3) + (B_2B*5) + (B_3B*8) + (B_HR*10) +
           (RBI*2) + (RUNS*2) + ((BB+IBB+HBP)*2) + (SB*5),
         DATE = ymd(substr(GAME_ID, 4, 11)),
         DAY_GM_NBR = substr(GAME_ID, 12, 12)) %>%
  ungroup()

# NOTE: Batted ball data will differ by source. Statcast data would be better for that
Cumul_GameLog <- GameLog %>%
  group_by(BAT_ID) %>%
  arrange(DATE, DAY_GM_NBR) %>%
  mutate_at(.vars = vars(HITS:RUNS), .funs = funs(cum = cumsum)) %>%
  mutate(DK_PTS_cummean = cummean(DK_PTS)) %>%
  # Standard Statistics
  mutate(AVG = HITS_cum / AB_cum,
         OBP = (HITS_cum + BB_cum + IBB_cum + HBP_cum) / 
                  (AB_cum + BB_cum + IBB_cum + HBP_cum + SACF_cum),
         SLG = (B_1B_cum + (2*B_2B_cum) + (3*B_3B_cum) + (4*B_HR_cum)) / AB_cum,
         OPS = OBP + SLG,
         BABIP = (HITS_cum - B_HR_cum) / (AB_cum - SO_cum - B_HR_cum + SACF),
         ISO   = SLG - AVG,
         HR_FB_ratio = B_HR_cum / FB_CNT_cum,
         wOBA        = ((0.693*BB_cum) + (0.723*HBP_cum) + (0.877*B_1B_cum) + 
                          (1.232*B_2B_cum) + (1.552*B_3B_cum) + (1.98*B_HR_cum)) / 
                       (AB_cum + BB_cum - IBB_cum + SACF_cum + HBP_cum),
         BB_pct = BB_cum / PA_cum,
         SO_pct = SO_cum / PA_cum) %>% 
  group_by(BAT_ID) %>%
  arrange(DATE, DAY_GM_NBR) %>%
  mutate_at(.vars = vars(HITS_cum:SO_pct), .funs = funs(lag(.)))
  
  