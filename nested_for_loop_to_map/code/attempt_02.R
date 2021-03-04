rm(list=ls())
`%notin%` <- Negate(`%in%`)
suppressMessages(library(tidyverse))
suppressMessages(library(readtext))
suppressMessages(library(stringdist))
suppressMessages(library(utf8))
library(furrr)
library(tictoc)
plan(multisession, workers = 8)
Sys.setlocale("LC_ALL", locale = "chs")
library(repurrrsive)
library(data.table)

# Set up strings to match
target_strings_1 <- as_utf8(c("脑死亡后用麻醉机维持呼吸", "死亡后迅速建立人工呼吸", "自主呼吸丧失的脑死亡供体,在特定条件下应尽可能迅速建立辅助呼吸支持循环,维持供心的血氧供应,避免或缩短热缺血时间,同时迅速剖胸取心.", "供体大脑死亡后,首先分秒必争地建立呼吸与静脉通道", "经气管切开气管插管建立人工呼吸", "快速胸部正中切口进胸", "供者脑死亡后迅速建立人工呼吸", "供心保护脑死亡后用麻醉机维持呼吸", "供体确定脑死亡后,气管插管,彻底吸除气道分泌物,用简易呼吸器人工控制呼吸", "供体脑死亡后,迅速建立人工呼吸", "供体脑死亡后快速正中开胸,同时插入气管导管人工通气", "脑死亡后,紧急气管插管", "供者行气管插管", "供者行气管插管,球囊加压通气,静脉注射肝素200mg", "脑死亡后，用麻醉机维持呼吸", "供体在确认脑死亡后,气管插管,建立人工呼吸", "脑死亡后气管紧急插管,纯氧通气", "供体死亡后行人工呼吸、循环支持"))

names(target_strings_1) <-  rep("intubation", length(target_strings_1))

# full_list_for_search <- read_rds(r"(C:\Users\m\projects\ANALYSISHERE\data\dead_donor_file_list_for_search.Rds)")

Sys.setlocale("LC_CTYPE", locale = "C")
# path <- ("./nested_for_loop_to_map/data")
path <- (r"(C:\Users\m\projects\PUTDBHERE\data\merged\txt_clean\01\)")

# path <- ("./data/temp")

# file_list <- as_utf8(list.files(path)) 

file_list_orig <- as_utf8(list.files(path)) %>% 
  as_tibble() %>% 
  separate(col = value, into = c("doc_id", "title"), remove = FALSE, sep = "--", extra = "drop")
file_list <- file_list_orig
# file_list <- file_list_orig %>% slice(1:2)
toobig <- file_list %>% mutate(slength = str_count(value)) %>% arrange(desc(slength)) %>% head(10)
file_list <- file_list %>% filter(value %notin% toobig[["value"]])

Sys.setlocale("LC_CTYPE", locale = "chs")


get_full_match <- function(file, path_to_file, s_target) {
  
  doc_id <- file
  s_full <- readtext(paste0(path_to_file,"\\",file), encoding='utf-8')[[2]]
  r_afind <- afind(s_full, s_target, window = nchar(s_target), method="running_cosine")
  s_context <- map(r_afind[["location"]], ~substr(s_full, .x-30, .x+nchar(.x)+30), .id = "context")
  
  #store location of the match
  names(r_afind[["location"]]) <- rep("location", length(r_afind[["location"]]))
  names(r_afind[["match"]]) <- rep("match", length(r_afind[["match"]]))
  names(r_afind[["distance"]]) <- rep("distance", length(r_afind[["distance"]]))
  names(s_context) <- rep("context", length(s_context))
  
  return(list(c(doc_id, s_target, r_afind[["location"]], r_afind[["match"]], r_afind[["distance"]], s_context)))
  
}


tic()
df_all <- future_map(file_list[["value"]], ~get_full_match(.x, path, target_strings_1))
toc()

tic()
dfout <- tibble(paper = df_all) 
dfout <- file_list %>% bind_cols(dfout)
dfout <- dfout %>% unnest_longer(paper)
dfout <- dfout[["paper"]] %>% as_tibble(.name_repair = "unique")
dfout <- dfout %>% map_df(~as_tibble_row(.x, .name_repair = "unique"))

df_s_context <- dfout %>% select(starts_with("cont")) %>% 
  pivot_longer(cols = starts_with("context"), names_to = "drop", values_to = "s_context") %>% 
  select(-drop)

df_s_location <- dfout %>% select(starts_with("loc")) %>% 
  pivot_longer(cols = starts_with("loc"), names_to = "drop", values_to = "s_location") %>%
  select(-drop)

df_s_original <- dfout %>%  select(starts_with("mat")) %>%  
  pivot_longer(cols = starts_with("mat"), names_to = "drop", values_to = "s_original") %>%
  select(-drop)

df_d_stringdist <- dfout %>%  select(starts_with("dist")) %>%  
  pivot_longer(cols = starts_with("dis"), names_to = "drop", values_to = "s_distance") %>%
  select(-drop)

df_s_target <- dfout %>% 
  select(starts_with("intub")) %>% 
  pivot_longer(cols = starts_with("intub"), names_to = "var_of_interest", values_to = "s_target") %>%
  distinct(s_target, .keep_all = TRUE)

df_var_of_interest <- dfout %>% select(starts_with("int")) %>% 
           pivot_longer(cols = starts_with("int"), names_to = "var_of_interest", values_to = "s_target")

dfout <- dfout %>% rename(doc_id = `...1`)
df_doc_id <- rep(dfout[["doc_id"]], each = as.integer(nrow(df_s_target))) %>% as_tibble()

df_s_target <- rep_len(df_s_target[["s_target"]], as.integer(nrow(df_s_location))) %>% as_tibble()

output <- bind_cols(df_doc_id, df_var_of_interest, df_s_location, df_s_target, df_d_stringdist, df_s_original, df_s_context) %>% 
  rename(doc_id = "value...1")


output %>% filter(s_distance < 0.3)
toc()
