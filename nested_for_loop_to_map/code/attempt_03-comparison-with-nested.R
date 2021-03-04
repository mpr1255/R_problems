# Variables of interest ---------------------------------------------------
# each on own line to keep track of them
var_of_interest <- c("intubation_after_brain_death")

# Create strings corresponding to the variables of interest ---------------

Sys.setlocale("LC_ALL", locale = "chs")

# intubation_after_brain_death
x1 <- as_utf8(c("脑死亡后用麻醉机维持呼吸", "死亡后迅速建立人工呼吸", "脑死亡后用麻醉机维持呼吸", "自主呼吸丧失的脑死亡供体,在特定条件下应尽可能迅速建立辅助呼吸支持循环,维持供心的血氧供应,避免或缩短热缺血时间,同时迅速剖胸取心.", "供体大脑死亡后,首先分秒必争地建立呼吸与静脉通道", "经气管切开气管插管建立人工呼吸", "快速胸部正中切口进胸", "供者脑死亡后迅速建立人工呼吸", "供心保护脑死亡后用麻醉机维持呼吸", "供体确定脑死亡后,气管插管,彻底吸除气道分泌物,用简易呼吸器人工控制呼吸", "供体脑死亡后,迅速建立人工呼吸", "供体脑死亡后快速正中开胸,同时插入气管导管人工通气", "脑死亡后,紧急气管插管", "供者行气管插管", "供者行气管插管,球囊加压通气,静脉注射肝素200mg", "脑死亡后，用麻醉机维持呼吸", "供体在确认脑死亡后,气管插管,建立人工呼吸", "脑死亡后气管紧急插管,纯氧通气", "供体死亡后行人工呼吸、循环支持"))

# Build dataframe based on those variables and strings --------------------

# Create an empty dataframe
df_variables <- data.frame(matrix(ncol = 2, nrow = 0))
y <- c("var_of_interest", "s_target")
colnames(df_variables) <- y

# Get a list of characters: x1, ..., x10
x_list <- paste0("x", 1)

# Get a list of symbols: x1, ..., xX (whatever the length of x_list is)
x_symbol_list <- syms(paste0("x", seq_along(x_list)))

# Populate the dataframe
df_variables <- map_dfr(seq_along((x_list)), function(i){
  data.frame(var_of_interest = var_of_interest[i],
             x_symbol_list[i]) %>% 
    rename(s_target=x_list[i])
})



# Set up dataframe used for fuzzy string matching -------------------------

n = 30000
t_match_output <- data.frame(x1=double(length=n),x2=character(length=n),x3=character(length=n),x4=character(length=n),x5=integer(length=n),x6=character(length=n),x7=character(length=n))

# use this for doing without preallocation
# t_match_output <- data.frame(matrix(ncol = 7, nrow = 0))
z <- c("s_score", "s_target", "s_original", "s_context", "s_location", "var_of_interest", "doc_id")
colnames(t_match_output) <- z



# Do fuzzymatching --------------------------------------------------------
Sys.setlocale("LC_CTYPE", locale = "C")
path <- ("./nested_for_loop_to_map/data/")
file_list <- as_utf8(list.files(path)) %>% as_tibble() %>% rename(doc_id = value)
Sys.setlocale("LC_CTYPE", locale = "chs")

tic()
for (file in file_list$doc_id){
  
  #read in text file, fix chars, delete whitespace, rename cols
  df_fulltext <- readtext(paste0(path, file), encoding='utf-8')
  df_fulltext <- as.data.frame(apply(df_fulltext,2,function(x)gsub('\\s+', '',x)), encoding='utf-8')
  df_fulltext <- as.data.frame(matrix(t(df_fulltext), ncol=2, 
                                      byrow=TRUE), stringsAsFactors=FALSE) %>% 
    as_tibble() %>% 
    rename(document_id = V1, full_text = V2)
  
  # set up vars
  doc_id <- df_fulltext[[1]]
  s_fulltext <- df_fulltext[[2]]
  
  # iterate over the fulltext article
  for (h in s_fulltext){
    
    # iterate through integers of length my var of interest; set up variables
    for (i in 1:nrow(df_variables)){
      s_target <- as_utf8(df_variables[[2]][i])
      var_of_interest <- df_variables[[1]][i]
      
      t_fuzzymatch <- afind(s_fulltext, s_target, window = nchar(s_target), 
                            method="running_cosine")
      
      # create context variable, so I can lookaround my fuzzymatch
      s_context <- substr(s_fulltext, t_fuzzymatch$location-30,
                          t_fuzzymatch$location+nchar(s_target)+30)
      
      # store location of the match 
      s_location <- t_fuzzymatch$location
      
      # create temporary df with all the data
      t_match_output[nrow(t_match_output) + 1, ] <- rbind(c(t_fuzzymatch$distance,
                                                            s_target,
                                                            t_fuzzymatch$match, 
                                                            s_context, s_location,
                                                            var_of_interest, doc_id))
      
        # replace each match with Xs, allowing capture of top four matches
        # substring(s_fulltext, t_fuzzymatch$location) <- c(str_dup("X",
        # nchar(t_fuzzymatch$match)))
        
        # NOTE: I am painfully aware that this loop is suboptimal. If you know how I can improve it, particularly by using map_df or map2, or some other Rthonic way, please be in touch! 
    }
  }
}

toc()

# Convert to tibble -------------------------------------------------------

t_match_output <- as_tibble(t_match_output)


# Prep the df  -------------------------------------------------------

# t_match_output <- t_match_output %>% 
#   separate(col = doc_id, into = c("id_number", "title"), 
#            sep = "--", 
#            extra = "drop", 
#            remove = FALSE)

# t_match_output <- t_match_output %>% map_df(., ~as_utf8(.), .x)

# t_match_output %>% write_rds("./data/t_match_output.Rds")

# t_match_output <- t_match_output %>% 
#   map_df(., ~str_replace_all(., pattern = en_ch_replace), .x)