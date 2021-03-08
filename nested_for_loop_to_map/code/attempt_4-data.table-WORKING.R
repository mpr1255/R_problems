rm(list = ls())
`%notin%` <- Negate(`%in%`)

suppressMessages(library(readtext))
suppressMessages(library(stringdist))
suppressMessages(library(utf8))
suppressMessages(library(tictoc))
suppressMessages(library(data.table))
suppressMessages(library(tidyverse))

# You have to tweak this per your computer settings.
suppressMessages(library(furrr))
plan(multisession, workers = 8)

Sys.setlocale("LC_ALL", locale = "chs")

# Set up strings to match
target_strings_1 <- as_utf8(c("脑死亡后用麻醉机维持呼吸", "死亡后迅速建立人工呼吸", "自主呼吸丧失的脑死亡供体,在特定条件下应尽可能迅速建立辅助呼吸支持循环,维持供心的血氧供应,避免或缩短热缺血时间,同时迅速剖胸取心", "供体大脑死亡后,首先分秒必争地建立呼吸与静脉通道", "经气管切开气管插管建立人工呼吸", "快速胸部正中切口进胸", "供者脑死亡后迅速建立人工呼吸", "供心保护脑死亡后用麻醉机维持呼吸", "供体确定脑死亡后,气管插管,彻底吸除气道分泌物,用简易呼吸器人工控制呼吸", "供体脑死亡后,迅速建立人工呼吸", "供体脑死亡后快速正中开胸,同时插入气管导管人工通气", "脑死亡后,紧急气管插管", "供者行气管插管", "供者行气管插管,球囊加压通气,静脉注射肝素200mg", "脑死亡后，用麻醉机维持呼吸", "供体在确认脑死亡后,气管插管,建立人工呼吸", "脑死亡后气管紧急插管,纯氧通气", "供体死亡后行人工呼吸、循环支持", "脑死亡后,气管插管", "脑死亡后立即气管内插管给氧", "脑死亡,面罩加压给氧,辅助呼吸", "脑死亡后,将供体取仰卧位,争取做气管插管", "脑死亡后迅速气管插管", "脑死亡后迅速气管插管进行机械通气", "协助麻醉医生进行支纤镜检查后进行供体气管插管", "脑死亡后,4例气管插管,3例面罩吸氧", "脑死亡后插入气管导管", "在这紧急情况下,必须在紧急开胸的同时,进行紧急气管插管及辅助呼吸", "供体手术气管插管通气", "供体手术气管插管", "气管切开气管插管", "供体心脏的提取供心者取仰卧位,垫高胸腔,气管插管", "进行供心、肺切取,吸净气管分泌物,气管插管给氧", "供体心肺的切取气管插管", "供肺切取:供体气管插管", "供者平卧位,气管插管", "供心切取配合,护士协助医生气管插管辅助呼吸", "供心切取配合..气管插管", "供体平卧位，气管插管", "协助麻醉医生进行支纤镜检查后进行气管插管", "供体心肺的获取和保护..行气管插管通气", "供心的切取供体气管插管后", "供者气管插管", "供体全身肝素化后，仰卧位，经口气管内插管"))

target_strings_1 %>% .[duplicated(.)]

names(target_strings_1) <-  rep("intubation", length(target_strings_1))

# full_list_for_search <- read_rds(r"(C:\Users\m\projects\ANALYSISHERE\data\dead_donor_file_list_for_search.Rds)")

Sys.setlocale("LC_CTYPE", locale = "C")
# path <- ("./nested_for_loop_to_map/data")
path_fzmatchbig <- (r"(C:\Users\m\projects\DBHERE\data\merged\txt_clean)")
path_fzmatchbig <- ("nested_for_loop_to_map/data/")
file_list_fzmatchbig <- as_utf8(list.files(path_fzmatchbig))

# file_list_fzmatchbig <- file_list_fzmatchbig_w_path %>% select(value)
# file_list_fzmatchbig <- file_list_fzmatchbig %>% slice(1:4)

Sys.setlocale("LC_CTYPE", locale = "chs")

get_full_match <- function(file, path_to_file, s_target) {
  
  doc_id <- file
  s_full <- readtext(paste0(path_to_file,"\\",file), encoding='utf-8')[[2]]
  r_afind <- afind(s_full, s_target, window = nchar(s_target), method="running_cosine")
  
  #store location of the match
  names(r_afind[["location"]]) <- rep("location", length(r_afind[["location"]]))
  names(r_afind[["match"]]) <- rep("match", length(r_afind[["match"]]))
  names(r_afind[["distance"]]) <- rep("distance", length(r_afind[["distance"]]))
  
  return(list(c(s_target, r_afind[["location"]], r_afind[["match"]], r_afind[["distance"]])))
  
}


# IGNORE ------------------------------------------------------------------
# 165 seconds with 21k files and 44 search terms; then 244 seconds on 20210304_1825. IGNORE ALL THIS> ONLY REFERS TO LOCAL FILES.
# if (!file.exists("./data/df_all.Rds")){
#   df_all <- future_map(file_list_fzmatchbig[["value"]], ~get_full_match(.x, path_fzmatchbig, target_strings_1)) 
#   #Save that output! It's very expensive to run that. We're going to stash it in an rds file.
#   df_all %>% write_rds("./data/df_all.Rds")
# } else{
#   print("df_all.Rds already exists! Loaded to memory")
#   df_all <- read_rds("./data/df_all.Rds")
# }
# 
# RESUME ------------------------------------------------------------------

df_all <- future_map(file_list_fzmatchbig, ~get_full_match(.x, path_fzmatchbig, target_strings_1)) 

dfout <- tibble(paper = df_all) 
# dfout <- dfout %>% slice(1:3)

# dfout <- file_list_fzmatchbig %>% bind_cols(dfout)

dfout <- dfout %>% unnest_longer(paper) # 9 seconds with 21k files and 44 search terms; 10 seconds on 20210304

dfout <- dfout[["paper"]] %>% as_tibble(.name_repair = "unique") #0.22s

DT <- as.data.table(dfout) #0.04s

split_combine_columns <- function(x){
  s_target <- x[1:as.integer(length(target_strings_1))]
  s_location <- x[as.integer(length(target_strings_1)+1):as.integer(length(target_strings_1)*2),]
  s_original <- x[as.integer(length(target_strings_1)*2+1):as.integer(length(target_strings_1)*3)]
  s_stringdist <- x[as.integer(length(target_strings_1)*3+1):as.integer(length(target_strings_1)*4)]
  
  return(cbind(s_target, s_location, s_original, s_stringdist))
}

# define a NULL matrix. Then make it a data.table object.
# dt_final <- matrix(, nrow = as.integer(length(target_strings_1)*4) , ncol = 0)
# as.data.table(dt_final)
# 
# TRYING PAT'S BFM APPROACH


BFM<- matrix(nrow=176, ncol=2732)
Starter<-1
Ender <- 0

for (col in colnames(DT)){
  cols <- ncol(split_combine_columns(DT[,..col]))
  Ender<- Ender+cols
  BFM[,(Starter:Ender)]
  Starter <- Starter + cols
}


# THIS IS THE PROBLEM AREA!! THIS LOOP RUNNING OVER THAT FUNCTION -----------------------------------

# When dt_final was created as a null matrix and I ran this loop, 299.29 sec elapsed before I pulled plug.... Then tried making it a data.table and run again. Then I let it run for 11779.22 sec (over 3hrs) before pulling the plug. That grew it to 743k obs (this is when my inputs were 20k txt files, not 683). Benchmarks on 20210304_2220 -- 20secs grew it to 2828 obs. That was when dfout[,..col] was being sent to split_combine_columns. But then I tried to pre-allocate it to ~800k obs and 4 cols, and used DT[,..col]... BUT - I could not get the pre-allocation working right, and it kept adding them to the end. Without pre-allocation, after 20 seconds, it got through 2704 variables. Changed back into matrix object and after 20 seconds it grew to 2868 varibables. Therefore I could not get pre-allocation working, and whether or not I grew a matrix object or a data.table didn't matter. It's terribly inefficient. I managed to avoid it with future_map and everything, but cannot figure out how to avoid it HERE. 
# 
# The major task to fix this code is to ditch this loop and somehow figure out how to slice the data.table and put it back together properly without this. But I just cannot figure that out right now.

# HERE'S THE BAD LOOP ---------------------------------------------------------

for (col in colnames(DT)){
  dt_final <- cbind(dt_final, split_combine_columns(DT[,..col]))
}

names(dt_final) <- paste0(rep(1:ncol(dt_final), each = 4, length.out = length(names(dt_final))), c("target", "location", "original", "score"))

names(dt_final)[1:4] <- c("s_target","s_location", "s_original", "s_score")

# Make vector of names 
dt_final <- melt(dt_final, measure = patterns("target", "location", "original", "score"), value.name = c("s_target", "s_location", "s_original", "s_score"))

# Going back to the tidyverse
tb_final <- dt_final %>% as_tibble()

file_list_fzmatchbig <- file_list_fzmatchbig %>% 
  as_tibble() %>% 
  mutate(variable = row_number()) %>% 
  mutate(variable = as.factor(variable))

tb_final <- tb_final %>% 
  left_join(file_list_fzmatchbig, by = "variable") %>% 
  rename(doc_id = value) %>% 
  select(-variable) %>% 
  select(doc_id, s_target, s_location, s_original, s_score)

#SAVE THIS OBJECT
# tb_final %>% write_rds("./data/tb_final.Rds")
# 
Sys.setlocale("LC_CTYPE", locale = "C")
dt_final  <-  as.data.table(read_rds(r"(C:\Users\m\projects\dead_donor\data\tb_final.Rds)"))
options(datatable.prettyprint.char=15L)


dt_set_threshold <- dt_final[s_score < .4]

dt_set_threshold[,length(unique(doc_id))]



# function for grabbing context
get_context <- function(doc_id, location){
  file <- doc_id
  
  s_full <- readtext(paste0(path_fzmatchbig,"\\",file), encoding='utf-8')[[2]]
  
  return(substr(s_full, location-50, location+nchar(location)+50))
}

tb_w_context <- dt_set_threshold[,s_context := get_context(doc_id, as.integer(s_location))]

dt_int_2 <- tb_w_context[s_context %like% "供"][s_context %like% "体"][s_context %like% "插管"][, intubation_score := 2]

# test2 <- dt_int_2[, c("drop", "id", "drop") := tstrsplit(doc_id, "--", fixed=TRUE)]

# dt_int_2 %>% fwrite("nested_for_loop_to_map/data/dt_int_2.txt")







dt_int_2[,length(unique(doc_id))]

test
options(datatable.prettyprint.char=10L)
dt_int_2[,doc_id]



tb_w_context <- tb_set_threshold %>% 
  mutate(s_context = get_context(doc_id, as.integer(s_location)))

tb_w_context %>% 
  filter(grepl("供", s_context)) %>% 
  xlsx::write






options(datatable.prettyprint.char=20L)