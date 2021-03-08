dt_alldocs <- dt_set_threshold[,unique(doc_id)]

dt_alldocs %>% write_rds("nested_for_loop_to_map/data/dt_alldocs_for_big_analysis.Rds")
