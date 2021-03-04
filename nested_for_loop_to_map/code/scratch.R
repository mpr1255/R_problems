





Sys.setlocale("LC_CTYPE", locale = "chs")

df_all %>% map(~bind_rows())

len <- length(df_all[[1]][[1]]) - 1
# len <- as.integer(len)
df_all[[1]][[1]][len-5:len]

unlisted <- df_all[[1]] %>% unlist()
s_target <- unlisted[2:as.integer(len/5+1)] %>% as.list()
location <- unlisted[as.integer(len/5+2):as.integer(len/5*2+1)] %>%  as.list()

rbindlist(c(s_target, location), fill=TRUE, idcol = NULL)

rbindlist(df_all) %>% View()

Sys.setlocale("LC_CTYPE", locale = "chs")

str(df_all)







dfout %>% flatten()

str(dfout1)

dfout1 %>% flatten_dfr(dfout1[[1]][[3]][["match"]])
dfout1[[1]][[3]][["match"]]
dfout1 <- dfout %>% unnest_wider(paper)

Sys.setlocale("LC_CTYPE", locale = "chs")
dfout2 <- dfout1 %>% unnest_wider(...3)
# pivot_longer(names_prefix = "location")

dfout2$location[2,2]


dfout1 %>% View()

dfout2 %>% unnest_wider(...2) %>% View()
dfout2 %>% unnest_longer(...4) %>% View()




View(output)



path_to_file <- path
file <- file_list[[1]][1]


testafind <- afind(readtext(paste0(path_to_file,"\\",file), encoding='utf-8')[[2]], target_strings_1, method="running_cosine")
testfulltext <- readtext(paste0(path_to_file,"\\",file), encoding='utf-8')[[2]]


map(testafind[["location"]], ~get_context(testfulltext, .x))


get_context <- function(x, y){
  substr(x, y-20, y+nchar(y)+20)
}

map(testafind[["location"]], ~substr(testfulltext, .x-20, .x+nchar(.x)+20))


substr(testfulltext, testafind[["location"]]-30, testafind[["location"]]+nchar(testafind[["location"]])+30)










test[["location"]]






get_fuzzy_match <- function(s_full, s_target){
  afind(s_full, s_target, window = nchar(s_target), method="running_cosine")
}

get_read_files <- function(path_to_files, list_of_files){
  path1 <- path_to_files
  list1 <- list_of_files
  return(readtext(paste0(path_to_files,"\\",list_of_files), encoding='utf-8'))
}

# x is the character vector that is being looked within; y is the [integer] location in it
get_context <- function(x, y){
  substr(y, x-10, x+nchar(x)+10)
}

get_pluck <- function(x){
  map_chr(~pluck(.x))
}

file_list <- file_list %>% 
  map_dfr(~get_read_files(path, .x))
# map(text, ~get_fuzzy_match(.x, target_strings_1))

file_list <- file_list %>% 
  mutate(fuzzy_results = map(text, ~get_fuzzy_match(.x, target_strings_1)))

file_list %>% get_pluck(fuzzy_results$location)


file_list %>% map(fuzzy_results, ~get_pluck(.x))

file_list %>% modify_depth(fuzzy_results, n =2)

View(file_list)



file_list %>% get_context(text, fuzzy_results$location)

file_list %>% pluck(location)

file_list %>% pluck(fuzzy_results)




df_fulltext %>% 
  mutate(afind = map(full_text,  ~fuzzymatch(.x, df_variables$s_target))) %>% 
  select(-full_text) %>% 
  unnest_longer(afind)


testdf <- map(df_fulltext$full_text, ~fuzzymatch(.x, df_variables$s_target)) %>% 
  rbindlist(., fill=TRUE) %>% 
  as_tibble() 


map(testdf$location, ~get_context(.x, df_fulltext$full_text)) %>% 
  lapply(data.frame, stringsAsFactors = FALSE) %>% 
  bind_rows() %>% 
  rename(context = `X..i..`) %>% 
  bind_cols(testdf) %>% 
  as_tibble()
Sys.setlocale("LC_CTYPE", locale = "chs")












get_context(read_files(path, file_list1[["value"]])[[2]], file_list2$afind$location[[1]])




file_list <- file_list %>% slice(1)
for (file_num in seq_along(file_list[[1]])){
  
  
  
  
  
  
  
  
}




file_list[["value"]][4]


Sys.setlocale("LC_CTYPE", locale = "chs")

loc_dist_match <- map(read_files(path, file_list1[["value"]])[[2]], ~fuzzymatch(.x, target_strings_1)) %>% 
  rbindlist(., fill=TRUE) %>% 
  as_tibble() 

article_num <- nrow(file_list1)
t_string_num <- length(target_strings_1)

# context <- 

map(loc_dist_match[["location"]], ~get_context(.x, read_files(path, file_list1[["value"]])[[2]])) %>% 
  lapply(data.frame, stringsAsFactors = FALSE)  %>% 
  rbindlist(., fill=TRUE)
Sys.setlocale("LC_CTYPE", locale = "chs")
# %>% 


%>%
  # bind_rows() %>% 
  # rename(context = `X..i..`) %>% 
  # bind_cols(testdf) %>% 
  # as_tibble()
  
  
  
  file_list2$afind$location[[1]]


read_files(path, file_list1[["value"]])[2]

file_list1 %>% mutate(
  fulltext = read_files(path, file_list1[["value"]])[2])





file_list %>% mutate(fulltext = readtext(paste0(path,"\\",file_list), encoding='utf-8')[[2]])


#                      
#                      map(full_text,  ~fuzzymatch(.x, df_variables$s_target))) %>% 
# select(-full_text) %>% 
# unnest_longer(afind)









for (file_num in seq_along(file_list)){
  file <- file_list[file_num]
  
}

# mutate(text = str_replace(text, '\\s+', ''))


for (file in seq_along(file_list[[1]])){
  file <- file_list[[1]] 
  fulltext <- map_dfr(file, ~readtext(paste0(path,"\\",.x), encoding='utf-8')) 
  matches <- map_df(target_strings_1, ~fuzzymatch(fulltext[["text"]], .x))
  s_targets <- as_tibble()
}


df_fulltext <- 
  
  df_fulltext[["text"]][1]

# Main function goes here***************










testdf <- map(df_fulltext$full_text, ~fuzzymatch(.x, df_variables$s_target)) %>% 
  rbindlist(., fill=TRUE) %>% 
  as_tibble() 


map(testdf$location, ~get_context(.x, df_fulltext$full_text)) %>% 
  lapply(data.frame, stringsAsFactors = FALSE) %>% 
  bind_rows() %>% 
  rename(context = `X..i..`) %>% 
  bind_cols(testdf) %>% 
  as_tibble()
Sys.setlocale("LC_CTYPE", locale = "chs")


tic()
test3 <- df_fulltext %>% 
  mutate(afind = map(full_text,  ~fuzzymatch(.x, df_variables$s_target))) %>% 
  select(-full_text) %>% 
  unnest_longer(afind)
# mutate(out = map_df(afind, ~tibble(location = afind$location,
#                                     distance = afind$distance,
#                                   match = afind$match)))
View(test3)

View(test3)

str(test3)

test3$location

test3 %>% unnest_longer(afind)
Sys.setlocale("LC_CTYPE", locale = "chs")
test3 %>% mutate(distance1 = distance)

test3 %>% separate_rows(afind) %>% View()

separate_rows(afind)

<- map_chr(test3$afind$distance, ~pluck(.x)) %>% 
  as_tibble()



get_pluck <- function(x){
  
  map_chr(x, ~pluck(.x)) %>% 
    as_tibble()
}

location1 <- map(test3$afind, ~get_pluck(.x))$location
distance1 <- map(test3$afind, ~get_pluck(.x))$distance
match1 <- map(test3$afind, ~get_pluck(.x))$match

bind_cols(c(location1, distance1, match1)) %>% 
  rename(location = `value...1`, 
         match = `value...2`, 
         string = `value...3`)

separate_rows(test3$afind$location) %>% filter(afind != "")

View(test3)

unnest_longer(test3, col = afind)


str(test3$afind$location)

test3 %>% unnest_longer(afind)
test4 %>% flatten_dfr(afind$, .id = 'yes')
test4$afind$location
str(test4)
View(test4)

str(test3)


test3 %>% 
  map_df(afind, ~tibble())


Sys.setlocale("LC_CTYPE", locale = "chs")



contrived_data <- tibble(subgroup = rep(1:100, each = 10),
                         value    = rnorm(1000, mean = 5, sd = 1))

contrived_data %>% group_by(subgroup) %>%
  summarize(avg     = mean(value),
            std_dev = sd(value),
            ks_stat = ks.test(value, "pnorm", mean = 5, sd = 1)$statistic,
            ks_pval = ks.test(value, "pnorm", mean = 5, sd = 1)$p.value)







test4 %>% View()

test4$afind$distance
toc()

names(test4)
test4$afind$
  
  
  pmap(df_fulltext[2], ~fuzzymatch(.x, df_variables$s_target)) %>% 
  rbindlist(., fill=TRUE) %>% 
  as_tibble()




names(test2)

test2 %>% as_tibble()

Sys.setlocale("LC_CTYPE", locale = "chs")
test2 


map_df(test2, ~as_tibble)


str(testdf)
str(test2)

str(test)


testdf %>% map_df(~bind_rows(.x, .id = 'var2'), .id = 'var1') 


bind_at_any_depth2 <- function(l) {
  if (depth(l) == 2) {
    l <- bind_rows(l, .id = 'source')
    l <- unite(l, 'source', contains('source'))
    return(l)
  } else {
    l <- at_depth(l, depth(l) - 2, bind_rows, .id = paste0('source', depth(l)))
    bind_at_any_depth(l)
  }
}
testdf %>% flatten_dfr()

bind_at_any_depth2(testdf)



View(test2)


testdf %>% unnest(cols = c("location", "distance", "match"))

bind_rows()


substr(df_fulltext$full_text, t_fuzzymatch$location-30, t_fuzzymatch$location+nchar(df_variables$s_target)+30)






View(testdf)










walk(1:4, ~walk(1:6, ~print(paste(.x, .y, sep = "~")), .y = .x))

walk(df_variables$s_target, ~walk(df_fulltext$full_text, ~afind(.x, .y, window = nchar(.x), method = "running_cosine")))


test <- map_df(df_fulltext$full_text, function(.x) 
  map_df(df_variables$s_target, function(.y) 
    afind(.x, .y, window = nchar(.x), method = "running_cosine")))


View(test)


View(test)





#     s_fulltext <- df_fulltext[[2]][6]
#     for (i in s_fulltext){    
# }




t_fuzzymatch <- afind(df_variables$s_target[4], s_fulltext, window = nchar(df_variables$s_target[4]), 
                      method="running_cosine") 


s_context <- substr(s_fulltext, t_fuzzymatch$location-30,
                    t_fuzzymatch$location+nchar(df_variables$s_target)+30)




View(test_result)


# %>% 
as_tibble(.name_repair = "unique") %>% View()


# %>% 
filter(distance > .5)
# mutate(context = )

str(s_fulltext)

fuzzymatch(df_variables$s_target, x.)




df_variables$s_target




# set up vars
doc_id <- df_fulltext[[1]]
s_fulltext <- df_fulltext[[2]]


# iterate over the fulltext article
for (h in s_fulltext){
  
  # iterate through integers of length my var of interest; set up variables
  for (i in seq_along(df_variables)){
    s_target <- as_utf8(df_variables[[2]][i])
    var_of_interest <- df_variables[[1]][i]
    
    t_fuzzymatch <- afind(s_fulltext, s_target, window = nchar(s_target), 
                          method="running_cosine")
    
    # excluding any matches above/below given threshold
    if(is.na(t_fuzzymatch$distance))
      break
    else if(t_fuzzymatch$distance > 0.3){
      break
    }
    
    # create context variable, so I can lookaround my fuzzymatch
    s_context <- substr(s_fulltext, t_fuzzymatch$location-30,
                        t_fuzzymatch$location+nchar(s_target)+30)
    
    # store location of the match 
    s_location <- t_fuzzymatch$location
    
    # create temporary df with all the data
    t_match_output_big[nrow(t_match_output_big) + 1, ] <- cbind(c(t_fuzzymatch$distance,
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

# Convert to tibble -------------------------------------------------------

t_match_output_big <- as_tibble(t_match_output_big)


# Prep the df  -------------------------------------------------------

t_match_output_big <- t_match_output_big %>% 
  separate(col = doc_id, into = c("id_number", "title"), 
           sep = "--", 
           extra = "drop", 
           remove = FALSE)

# t_match_output_big <- t_match_output_big %>% map_df(., ~as_utf8(.), .x)

t_match_output_big %>% write_rds(paste0("./data/t_match_output_big_", run,".Rds"))

# t_match_output_big <- t_match_output_big %>% 
#   map_df(., ~str_replace_all(., pattern = en_ch_replace), .x)








dt_file_list_fzmatchbig <- setDT(file_list_fzmatchbig)

dt_file_list_fzmatchbig[]

dt_file_list_fzmatchbig[,.(rleidv())]

dt_file_list_fzmatchbig[rleid(dt_file_list_fzmatchbig$value)]

dt_file_list_fzmatchbig[, nth := row.names(.SD), by = value]



View(testwork)

View(dt_final)

idNumbers <- rep(1:10,each = 4) 








s_target <- DT[1:as.integer(length(target_strings_1))]
s_location <- DT[as.integer(length(target_strings_1)+1):as.integer(length(target_strings_1)*2)]
s_original <- DT[as.integer(length(target_strings_1)*2+1):as.integer(length(target_strings_1)*3)]
s_stringdist <- DT[as.integer(length(target_strings_1)*3+1):as.integer(length(target_strings_1)*4)]

dttest <- data.table()
for (i in seq_along(ncol(DT))){
  dttest[,ncol(dttest)+1] <- cbind(s_target[,i], s_location[,i], s_original[,i], s_stringdist[,i])
}




Sys.setlocale("LC_CTYPE", locale = "chs")

builddt2 <- cbind(s_target[,2], s_location[,2], s_original[,2], s_stringdist[,2])

combined <- rbind(builddt1, builddt2, use.names=FALSE)


View(combined)

options(datatable.prettyprint.char=20L)
options(datatable.printcols.char=1L)

View(builddt)

View(s_target)
View(s_original)











Sys.setlocale("LC_CTYPE", locale = "chs")

dfout <- dfout[,-1]

test <- dfout %>% map(~split_combine_columns(.x))
View(test)


DT[, (cols) := lapply(.SD, factor), .SDcols = cols]



test <- split_combine_columns(dfout[,2])

split_combine_columns <- function(x){
  
  s_target <- x[1:as.integer(length(target_strings_1))]
  s_location <- x[as.integer(length(target_strings_1)+1):as.integer(length(target_strings_1)*2),]
  s_original <- x[as.integer(length(target_strings_1)*2+1):as.integer(length(target_strings_1)*3)]
  s_stringdist <- x[as.integer(length(target_strings_1)*3+1):as.integer(length(target_strings_1)*4)]
  
  builddt <<- cbind(s_target, s_location, s_original, s_stringdist)
  
}

for (col in colnames(dfout)){
  split_combine_columns(dfout[,..col])
  # print(dfout[,..col])
}

builddt
map(dfout[,..col], ~split_combine_columns(.x))


# xdt <- ensym(x)
# DT <- setDT(xdt, keep.rownames = TRUE)
# DT <- DT[-1]



# return(builddt)


View(builddt)

split



vec <- names(dfout)
for(x in colnames(dfout)){
  
  DT <- setDT(asdf, keep.rownames = TRUE)
}
for(x in colnames(dfout)){
  asdf <- as.data.frame(dfout[[x]])
  DT <- setDT(asdf, keep.rownames = TRUE)
  DT <- DT[-1]
  s_target <- DT[1:as.integer(length(target_strings_1))]
  s_location <- DT[as.integer(length(target_strings_1)+1):as.integer(length(target_strings_1)*2),]
  s_original <- DT[as.integer(length(target_strings_1)*2+1):as.integer(length(target_strings_1)*3)]
  s_stringdist <- DT[as.integer(length(target_strings_1)*3+1):as.integer(length(target_strings_1)*4)]
  
  builddt <- as.data.table(1, 2)
  builddt[,ncol(builddt)+1] <- cbind(s_target, s_location, s_original, s_stringdist)
}



View(builddt)
map(dfout, ~split_combine_columns(.x))

split_combine_columns(dfout)


builddt %>% View()
# View(check3)

dfout_save <- dfout
dfout_save %>% View()
#save it here
# dfout %>% write_rds("./data/dfout.Rds")
# dfout <- read_rds("./data/dfout.Rds")
tic()
dfout <- dfout %>% map_df(~as_tibble_row(.x, .name_repair = "unique"))
toc()


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

output <- bind_cols(df_doc_id, df_var_of_interest, df_s_location, df_s_target, df_d_stringdist, df_s_original, df_s_context)  %>% 
  rename(doc_id = "value...1") %>% 
  select(-c("value...5"))

t_match_output_big <- output %>% filter(s_distance < 0.3)
toc()

# now go get the context






t_match_output_big %>% View()