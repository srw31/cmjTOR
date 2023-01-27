## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----prep, echo = T, results = 'hide', warning=F, message=F-------------------
# SET UP WORKSPACE, LOAD PACKAGES & DATA ----------------------------------------------------
# load general use packages using pacman
if (!require("pacman")) install.packages("pacman", INSTALL_opts = "--no-multiarch")
pacman::p_load(tidyverse, here, janitor, skimr, ggpubr, data.table, install = TRUE)

# set working directory
here()
wd <- here()

# create folder structure
for(df in c('code', 'data_files', 'output')){
  if(!exists(df))
  dir.create(path = paste0(wd, "/", df))
}

# load data
cmj_raw <- data.table::fread(here("data_files/CMJ_data.csv"))

## ----data clean, echo = T, results = 'hide'-----------------------------------
# INITIAL DATA REVIEW -----------------------------------------------------

# convert date from char to date as new variable
cmj_raw$date <- as.Date(cmj_raw$Date)
cmj_raw$month <- month(cmj_raw$date)

# overall snapshot of the data
cmj_raw %>%  skimr::skim() # 6 missing values for MetricValue

# ID rows with missing data
cmj_raw[!complete.cases(cmj_raw)] # All from Player1 on 2022-06-16

# metric values distributions
x <- cmj_raw %>% group_by(MetricName) %>% skim(MetricValue) %>% select(-numeric.hist)

print(x, include_summary = FALSE)

## ----cmj frequency------------------------------------------------------------
# when/how many times did each player perform CMJ
cmj_raw %>% dplyr::group_by(Player) %>% skimr::skim(date) %>% 
  select(-c(skim_variable, n_missing, complete_rate, Date.median))

cmj_raw %>% dplyr::group_by(Player) %>% skimr::skim(testid) %>% 
  select(-c(n_missing, complete_rate, character.min, character.max, character.empty, character.whitespace))


## ----asymmetry boxplots-------------------------------------------------------
cats <- c("Braking", "Landing", "Propulsive")

for(i in cats){
  print(ggboxplot(cmj_raw %>% 
          filter(str_detect(MetricName, i) & str_detect(MetricName, "\\|")), x = "MetricName", y = "MetricValue", color = "Player") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    geom_hline(yintercept=c(0), linetype='dashed', color=c('red')) +
  labs(title = paste0(i, " Asymmetry Vals")))
}

## ----asymmetry player avgs----------------------------------------------------
cats <- c("Braking", "Landing", "Propulsive")

for(i in cats){
x <- cmj_raw %>% filter(str_detect(MetricName, i) & str_detect(MetricName, "\\|")) %>% 
  group_by(Player, MetricName) %>% skim(MetricValue) %>% select(-c(skim_type, skim_variable, n_missing, complete_rate, numeric.p25, numeric.p50, numeric.hist))

print(x, include_summary = FALSE)
}

## ----monthly data, echo = T, warning=FALSE, message=FALSE---------------------
ggboxplot(cmj_raw %>% filter(MetricName == "L|R Landing Impulse Index(%)"), x = "month", y = "MetricValue", color = "Player", add = "jitter") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = paste0("Asymmetry - Landing Impulse - Monthly"))


ggboxplot(cmj_raw %>% filter(MetricName == "mRSI"), x = "month", y = "MetricValue", color = "Player", add = "jitter") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = paste0("mRSI - Monthly"))



